{-# LANGUAGE RecordWildCards #-}

module Network.Control.Flow (
    -- * Flow control for sending
    TxFlow (..),
    newTxFlow,
    txFlowWindow,

    -- * Flow control for receiving
    RxFlow (..),
    newRxFlow,
    FlowControlType (..),
    maybeOpenRxWindow,
    checkRxLimit,
) where

import Data.Bits

data TxFlow = TxFlow
    { txfSent :: Int
    , txfLimit :: Int
    }
    deriving (Show)

newTxFlow :: Int -> TxFlow
newTxFlow win = TxFlow 0 win

txFlowWindow :: TxFlow -> Int
txFlowWindow TxFlow{..} = txfLimit - txfSent

data RxFlow = RxFlow
    { rxfWindow :: Int
    , rxfConsumed :: Int
    , rxfReceived :: Int
    , rxfLimit :: Int
    }
    deriving (Show)

newRxFlow :: Int -> RxFlow
newRxFlow win = RxFlow win 0 0 win

data FlowControlType = FCTWindowUpdate | FCTMaxData

maybeOpenRxWindow :: Int -> FlowControlType -> RxFlow -> (RxFlow, Maybe Int)
maybeOpenRxWindow consumed fct flow@RxFlow{..}
    | available < threshold =
        let limit = consumed' + rxfWindow
            flow' =
                flow
                    { rxfConsumed = consumed'
                    , rxfLimit = limit
                    }
            update = case fct of
                FCTWindowUpdate -> limit - rxfLimit
                FCTMaxData -> limit
         in (flow', Just update)
    | otherwise =
        let flow' = flow{rxfConsumed = consumed'}
         in (flow', Nothing)
  where
    available = rxfLimit - rxfReceived
    threshold = rxfWindow .>>. 1
    consumed' = rxfConsumed + consumed

checkRxLimit :: Int -> RxFlow -> (RxFlow, Bool)
checkRxLimit received flow@RxFlow{..}
    | received' <= rxfLimit =
        let flow' = flow{rxfReceived = received'}
         in (flow', True)
    | otherwise = (flow, False)
  where
    received' = rxfReceived + received
