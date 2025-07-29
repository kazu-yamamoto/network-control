{-# LANGUAGE RecordWildCards #-}

module Network.Control.Recv where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef

-- | Return 'True' when continuation is possible
data Control = Control
    { controlContinue :: IO Bool
    , controlBuilder :: IORef (Int, [ByteString] -> [ByteString])
    }

newControl :: IO Bool -> IO Control
newControl controlContinue = do
    controlBuilder <- newIORef (0, id)
    return Control{..}

data Terminate = EOF | Break deriving (Eq, Show)

data Result
    = Terminate Terminate
    | NotEnough
    | NBytes ByteString
    deriving (Eq, Show)

controlledRecv :: Control -> (Int -> IO ByteString) -> Int -> IO Result
controlledRecv Control{..} recvN len = do
    cont <- controlContinue
    (blen, builder) <- readIORef controlBuilder
    if cont
        then do
            let wantN = len - blen
            bs <- recvN wantN
            let n = BS.length bs
            if n == 0
                then return $ Terminate EOF
                else do
                    let builder' = builder . (bs :)
                    if n == wantN
                        then do
                            let finalBS = BS.concat $ builder' []
                            writeIORef controlBuilder (0, id)
                            return $ NBytes finalBS
                        else do
                            let blen' = blen + n
                            writeIORef controlBuilder (blen', builder')
                            return NotEnough
        else return $ Terminate Break

-- Use to get leftover for Terminate
getLeftover :: Control -> IO ByteString
getLeftover Control{..} = do
    (_blen, builder) <- readIORef controlBuilder
    let leftover = BS.concat $ builder []
    return leftover

withControlledRecv
    :: Control
    -> (Int -> IO ByteString)
    -> Int
    -> (ByteString -> IO a)
    -> IO (Either Terminate a)
withControlledRecv ctl recvN len action = go
  where
    go = do
        r <- controlledRecv ctl recvN len
        case r of
            NotEnough -> go
            Terminate t -> return $ Left t
            NBytes bs -> Right <$> action bs
