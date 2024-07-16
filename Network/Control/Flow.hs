{-# LANGUAGE RecordWildCards #-}

module Network.Control.Flow (
    -- * Flow control

    -- | This is based on the total approach of QUIC rather than
    --   the difference approach of HTTP\/2 because QUIC'one is
    --   considered safer. Please refer to [Using HTTP\/3 Stream Limits in HTTP\/2](https://datatracker.ietf.org/doc/draft-thomson-httpbis-h2-stream-limits/) to understand that QUIC's approaches are better though its topic is about stream concurrency.

    -- ** Constants for flow control.
    defaultMaxStreams,
    defaultMaxStreamData,
    defaultMaxData,

    -- ** Flow control for sending
    TxFlow (..),
    newTxFlow,
    txWindowSize,
    WindowSize,

    -- ** Flow control for receiving
    RxFlow (..),
    newRxFlow,
    rxWindowSize,
    FlowControlType (..),
    maybeOpenRxWindow,
    checkRxLimit,
) where

import Data.Bits

-- | Default max streams. (64)
defaultMaxStreams :: Int
defaultMaxStreams = 64

-- | Default max data of a stream. (256K bytes)
defaultMaxStreamData :: Int
defaultMaxStreamData = 262144

-- | Default max data of a connection.
--
-- By default, this is set to @defaultMaxStreams * defaultMaxStreamData@. This
-- ensures that streams that are not currently handled cannot exhaust the
-- connection window.
--
-- If you use a smaller connection window size, you __must__ ensure that if you
-- are handling fewer concurrent streams than allowed by 'defaultMaxStreams',
-- that the unhandled streams cannot exhaust the connection window, or risk the
-- entire system deadlocking.
defaultMaxData :: Int
defaultMaxData = defaultMaxStreamData * defaultMaxStreams

-- | Window size.
type WindowSize = Int

-- | Flow for sending
--
-- @
-- -------------------------------------->
--        ^           ^
--     txfSent    txfLimit
--
--        |-----------| The size which this node can send
--        txWindowSize
-- @
data TxFlow = TxFlow
    { txfSent :: Int
    -- ^ The total size of sent data.
    , txfLimit :: Int
    -- ^ The total size of data which can be sent.
    }
    deriving (Eq, Show)

-- | Creating TX flow with a receive buffer size.
newTxFlow :: WindowSize -> TxFlow
newTxFlow win = TxFlow 0 win

-- | 'txfLimit' - 'txfSent'.
txWindowSize :: TxFlow -> WindowSize
txWindowSize TxFlow{..} = txfLimit - txfSent

-- | Flow for receiving.
--
-- @
--                 rxfBufSize
--        |------------------------|
-- -------------------------------------->
--        ^            ^           ^
--   rxfConsumed   rxfReceived  rxfLimit
--
--                     |-----------| The size which the peer can send
--                      rxWindowSize
-- @
data RxFlow = RxFlow
    { rxfBufSize :: Int
    -- ^ Receive buffer size.
    , rxfConsumed :: Int
    -- ^ The total size which the application is consumed.
    , rxfReceived :: Int
    -- ^ The total already-received size.
    , rxfLimit :: Int
    -- ^ The total size which can be recived.
    }
    deriving (Eq, Show)

-- | Creating RX flow with an initial window size.
newRxFlow :: WindowSize -> RxFlow
newRxFlow win = RxFlow win 0 0 win

-- | 'rxfLimit' - 'rxfReceived'.
rxWindowSize :: RxFlow -> WindowSize
rxWindowSize RxFlow{..} = rxfLimit - rxfReceived

-- | The representation of window size update.
data FlowControlType
    = -- | HTTP\/2 style
      FCTWindowUpdate
    | -- | QUIC style
      FCTMaxData

-- | When an application consumed received data, this function should
--   be called to update 'rxfConsumed'. If the available buffer size
--   is less than the half of the total buffer size AND window size update
--   is greater than 1/8 of the the total buffer size,
--   the representation of the window size update is returned.
--
-- @
-- Example:
--
--                 rxfBufSize
--        |------------------------|
-- -------------------------------------->
--        ^            ^           ^
--   rxfConsumed   rxfReceived  rxfLimit
--                     |01234567890|
--
-- In the case where the window update should be informed to the peer,
-- 'rxfConsumed' and 'rxfLimit' move to the right. The difference
-- of old and new 'rxfLimit' is window update.
--
--                   rxfBufSize
--          |------------------------|
-- -------------------------------------->
--          ^          ^             ^
--     rxfConsumed rxfReceived    rxfLimit
--                     |0123456789012| : window glows
--
-- Otherwise, only 'rxfConsumed' moves to the right.
--
--                 rxfBufSize
--        |------------------------|
-- -------------------------------------->
--          ^          ^           ^
--     rxfConsumed rxfReceived  rxfLimit
--                     |01234567890| : window stays
--
-- @
maybeOpenRxWindow
    :: Int
    -- ^ The consumed size.
    -> FlowControlType
    -> RxFlow
    -> (RxFlow, Maybe Int)
    -- ^ 'Just' if the size should be informed to the peer.
maybeOpenRxWindow consumed fct flow@RxFlow{..}
    | available < threshold && winUpdate > minSize =
        let flow' =
                flow
                    { rxfConsumed = consumed'
                    , rxfLimit = rxfLimit'
                    }
            update = case fct of
                FCTWindowUpdate -> winUpdate
                FCTMaxData -> rxfLimit'
         in (flow', Just update)
    | otherwise =
        let flow' = flow{rxfConsumed = consumed'}
         in (flow', Nothing)
  where
    available = rxfLimit - rxfReceived
    threshold = rxfBufSize `unsafeShiftR` 1
    minSize = rxfBufSize `unsafeShiftR` 3
    consumed' = rxfConsumed + consumed
    rxfLimit' = consumed' + rxfBufSize
    winUpdate = rxfLimit' - rxfLimit

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
