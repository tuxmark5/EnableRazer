import Control.Exception
import qualified Data.ByteString as BS (ByteString, empty, length, pack, unpack)
import Data.List (find)
import Data.Word (Word16)
import System.IO
import System.USB

_SET_REPORT = 0x09

{-
0030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........
0040  00 00 00 00 00 01 00 04  03 00 00 00 00 00 00 00   ........ ........
0050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........
0060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........
0070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........
0080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00   ........ ........
0090  00 00 00 00 00 00 00 00  06 00                     ........ ..      
-}

cdata :: BS.ByteString
cdata = BS.pack
 [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04 -- 40
 , 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 --
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -- 50
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 --
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -- 60
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 --
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -- 70
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 --
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -- 80
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 --
 , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -- 90
 , 0x06, 0x00
 ]

deviceFilter :: Word16 -> Word16 -> Device -> Bool
deviceFilter venId prodId dev
  =  deviceVendorId  devDesc == venId
  && deviceProductId devDesc == prodId
  where devDesc = deviceDesc dev
  
withDevice :: Word16 -> Word16 -> (Device -> IO a) -> IO a
withDevice venId prodId hnd = do
  ctx   <- newCtx
  setDebug ctx PrintInfo
  devs  <- getDevices ctx
  case find (deviceFilter venId prodId) devs of
    Nothing   -> return $ error "Device not found"
    Just dev  -> hnd dev
   
enableRazer :: DeviceHandle -> IO ()
enableRazer devHndl = do
  putStrLn "WRITING SET_REPORT"
  writeControlExact devHndl
    Class 
    ToInterface 
    _SET_REPORT 
    0x0300 
    2
    cdata
    noTimeout
  
main :: IO ()
main = withDevice 0x1532 0x010d $ \dev -> do
  withDeviceHandle dev $ \devHndl ->
    withDetachedKernelDriver devHndl 2 $
    withClaimedInterface devHndl 2 $ do
      res <- try $ enableRazer devHndl
      case res of
        Left (SomeException a)  -> putStrLn $ show a
        Right a                 -> putStrLn "OK"

