{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Network.Control.FlowSpec where

import Data.List
import Data.Text.Lazy (unpack)
import Network.Control
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Pretty.Simple

-- types

data Op = Consume | Receive
  deriving (Eq, Show, Bounded, Enum)

data OpWithResult = ConsumeWithResult (Maybe Int) | ReceiveWithResult Bool
  deriving (Eq, Show)

data Step op = Step {stepOp :: op, stepArg :: Int}
  deriving (Eq, Show)

data Trace = Trace
  { traceStart :: RxFlow,
    traceSteps :: [(Int, Step OpWithResult, RxFlow)]
  }
  deriving (Eq, Show)

-- arbitrary instances

maxWindowSize :: Int
maxWindowSize = 200 -- (more realistic: 2_000_000)

minFrameSize :: Int
minFrameSize = -20

instance Arbitrary RxFlow where
  arbitrary = newRxFlow <$> chooseInt (1, maxWindowSize)

instance Arbitrary Op where
  arbitrary = elements [minBound ..]

instance Arbitrary Trace where
  arbitrary = do
    initialFlow <- arbitrary
    len <- chooseInt (0, 500)
    Trace initialFlow <$> runManySteps len 0 initialFlow
    where
      runManySteps :: Int -> Int -> RxFlow -> Gen [(Int, Step OpWithResult, RxFlow)]
      runManySteps 0 _ _ = pure []
      runManySteps len ix oldFlow | len > 0 = do
        (newStep, newFlow) <- runStep oldFlow <$> genStep oldFlow
        ((ix, newStep, newFlow) :) <$> runManySteps (len - 1) (ix + 1) newFlow

      -- Not sure frame size > window size or 0 or engative consumed or received bytes are
      -- legal, but RxFlow works fine with them.  :)
      genStep :: RxFlow -> Gen (Step Op)
      genStep oldFlow = oneof [mkConsume, mkReceive]
        where
          mkReceive =
            Step Receive <$> chooseInt (minFrameSize, rxfWindow oldFlow * 2)

          mkConsume =
            let recv = rxfReceived oldFlow
             in if recv > 0
                  then Step Consume <$> chooseInt (minFrameSize, rxfReceived oldFlow)
                  else mkReceive

      runStep :: RxFlow -> Step Op -> (Step OpWithResult, RxFlow)
      runStep oldFlow = \case
        Step Consume arg ->
          let (newFlow, limitDelta) = maybeOpenRxWindow arg FCTWindowUpdate oldFlow
           in (Step (ConsumeWithResult limitDelta) arg, newFlow)
        Step Receive arg ->
          let (newFlow, isAcceptable) = checkRxLimit arg oldFlow
           in (Step (ReceiveWithResult isAcceptable) arg, newFlow)

  shrink trace@(Trace initialFlow steps) =
    trunc trace <> (Trace initialFlow <$> init (inits steps))
    where
      trunc :: Trace -> [Trace]
      trunc (Trace _ stp) = case reverse stp of
        [] -> []
        [_] -> []
        ((ix, lastStep, lastFlow) : (_, _, initFlow) : _) -> [Trace initFlow [(ix, lastStep, lastFlow)]]

-- invariants

assertTrace :: Trace -> Property
assertTrace (Trace initialFlow steps) = assertStep initialFlow steps

assertStep :: RxFlow -> [(Int, Step OpWithResult, RxFlow)] -> Property
assertStep _ [] = property True
assertStep oldFlow ((ix, step, newFlow) : steps) =
  (counterexample ("step #" <> show ix) check) .&. assertStep newFlow steps
  where
    check :: Expectation
    check = case step of
      Step (ConsumeWithResult limitDelta) arg -> do
        newFlow
          `shouldBe` RxFlow
            { rxfWindow = rxfWindow newFlow,
              rxfConsumed = rxfConsumed oldFlow + arg,
              rxfReceived = rxfReceived oldFlow,
              rxfLimit =
                if rxfLimit oldFlow - rxfReceived oldFlow < rxfWindow oldFlow `div` 2
                  then rxfConsumed oldFlow + arg + rxfWindow oldFlow
                  else rxfLimit oldFlow
            }
        limitDelta
          `shouldBe` case rxfLimit newFlow - rxfLimit oldFlow of
            0 -> Nothing
            n -> Just n
      Step (ReceiveWithResult isAcceptable) arg -> do
        newFlow
          `shouldBe` if isAcceptable
            then
              RxFlow
                { rxfWindow = rxfWindow newFlow,
                  rxfConsumed = rxfConsumed oldFlow,
                  rxfReceived = rxfReceived oldFlow + arg,
                  rxfLimit = rxfLimit oldFlow
                }
            else oldFlow

spec :: Spec
spec = do
  focus . prop "state transition graph checks out" $
    \trace -> counterexample (unpack $ pShowNoColor trace) (assertTrace trace)
