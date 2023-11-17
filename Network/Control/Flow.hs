{-# LANGUAGE RecordWildCards #-}

module Network.Control.Flow (
    -- * Flow control for sending
    TxFlow (..),
    newTxFlow,
    txWindowSize,
    WindowSize,

    -- * Flow control for receiving
    RxFlow (..),
    newRxFlow,
    FlowControlType (..),
    maybeOpenRxWindow,
    checkRxLimit,
) where

import Data.Bits

-- | Window size.
type WindowSize = Int

-- | Flow for sending
data TxFlow = TxFlow
    { txfSent :: Int
    , txfLimit :: Int
    }
    deriving (Show)

-- | Creating TX flow with an initial window size.
newTxFlow :: WindowSize -> TxFlow
newTxFlow win = TxFlow 0 win

-- | 'txfLimit' - 'txfSent'.
txWindowSize :: TxFlow -> WindowSize
txWindowSize TxFlow{..} = txfLimit - txfSent

-- | Flow for receiving
data RxFlow = RxFlow
    { rxfWindow :: WindowSize
    , rxfConsumed :: Int
    , rxfReceived :: Int
    , rxfLimit :: Int
    }
    deriving (Show)

-- | Creating RX flow with an initial window size.
newRxFlow :: WindowSize -> RxFlow
newRxFlow win = RxFlow win 0 0 win

-- | The representation of window size update.
data FlowControlType
    = -- | HTTP\/2 style
      FCTWindowUpdate
    | -- | QUIC style
      FCTMaxData

-- | When an application consumed received data,
--   this function should be called to update 'rxfConsumed'.
--   If the window size is less than the half of the initial window.
--   the representation of window size update is returned.
maybeOpenRxWindow
    :: Int
    -- ^ The consumed size.
    -> FlowControlType
    -> RxFlow
    -> (RxFlow, Maybe Int)
    -- ^ 'Just' if the size should be informed to the peer.
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

-- | Checking if received data is acceptable against the
--   current window.
checkRxLimit
    :: Int
    -- ^ The size of received data.
    -> RxFlow
    -> (RxFlow, Bool)
    -- ^ Acceptable if 'True'.
checkRxLimit received flow@RxFlow{..}
    | received' <= rxfLimit =
        let flow' = flow{rxfReceived = received'}
         in (flow', True)
    | otherwise = (flow, False)
  where
    received' = rxfReceived + received
