module Main where

-- sudo modprobe vcan
-- sudo ip link add dev vcan0 type vcan
-- sudo ip link set up vcan0
-- candump vcan0
-- cangen vcan0

import Network.Socket as N (Family(AF_CAN), Socket, SockAddr(..), ProtocolNumber, SocketType(Raw), bind, socket, close, recvBufFrom)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as S
import Network.BSD (ifNameToIndex)
import Data.Maybe (fromJust)
import Control.Monad (forever, liftM)
import Foreign.C.Types (CUInt, CUChar)
import Foreign.Storable
import qualified Data.ByteString.Char8 as C
import Foreign.Marshal.Alloc(alloca)
import Can
import Foreign.Ptr

bufSize :: Int
bufSize = 8

main :: IO ()
main = do
  s <- initCan "vcan0"
  canSend s
  forever $ do
    cf <- canRead s
    print $ show cf
  close s
  return ()


-- receive CAN frames from every enabled CAN interface
can0 :: SockAddr
can0 = SockAddrCan 0

-- particular protocol of the protocol family PF_CAN - CAN_RAW
canRaw :: ProtocolNumber
canRaw = read "1" :: ProtocolNumber

initCan :: String ->  IO Socket
initCan addr = do
  s <- socket AF_CAN Raw canRaw
  idx <-  fromJust <$> ifNameToIndex addr
  bind s (SockAddrCan $ fromIntegral idx)
  return s
              where idx = fromJust <$> ifNameToIndex addr

canTestMsg = S.pack [ 0,0,0,0 -- can ID = 0
                    , 4,0,0,0 -- data length counter = 2 (bytes)
                    , 0x80,123,244,55 -- SYNC with some random extra bytes
                    , 0, 0, 0, 0 -- padding
                    ]

canSend :: Socket -> IO Int
canSend s = send s canTestMsg

canRead :: Socket -> IO (Either String CanFrame)
canRead s =
  alloca $ \ptrCf -> do
    (cnt, _) <- recvBufFrom s (ptrCf :: Ptr CanFrame) 16
    if cnt > 0 then do
      frame <- peek ptrCf
      return $ Right frame
    else return $ Left "Buffer is empty"


