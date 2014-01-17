import            Control.Concurrent
import            Control.Exception
import qualified  Data.ByteString as BS (ByteString, pack)
import qualified  Data.Vector as V
import            Data.Word (Word16)
import            System.USB

_SET_REPORT :: Request
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

deviceFilter :: Word16 -> [Word16] -> Device -> IO Bool
deviceFilter venId prodIds dev = do
  devDesc <- getDeviceDesc dev
  return  $ deviceVendorId  devDesc == venId
         && any ((==) $ deviceProductId devDesc) prodIds
  
withDevice :: Word16 -> [Word16] -> (Device -> IO a) -> IO ()
withDevice venId prodIds hnd = do
  ctx   <- newCtx
  setDebug ctx PrintInfo
  devs  <- getDevices ctx
  devs1 <- V.filterM (deviceFilter venId prodIds) devs
  if V.null devs1 
    then return $ error "Device not found"
    else V.mapM_ hnd devs1
   
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
main = do
  withDevice 0x1532 [0x010d, 0x010e, 0x010f] $ \dev -> do
  withDeviceHandle dev $ \devHndl ->
    withDetachedKernelDriver devHndl 2 $
    withClaimedInterface devHndl 2 $ do
      res <- try $ enableRazer devHndl
      case res of
        Left (SomeException a)  -> putStrLn $ show a
        Right _                 -> putStrLn "OK"
