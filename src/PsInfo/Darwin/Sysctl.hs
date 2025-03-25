{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs #-}
module PsInfo.Darwin.Sysctl 
    ( getSysctl
    ) where
-- https://github.com/phracker/MacOSX-SDKs/blob/041600eda65c6a668f66cb7d56b7d1da3e8bcc93/MacOSX10.15.sdk/System/Library/Frameworks/Kernel.framework/Versions/A/Headers/sys/sysctl.h
-- https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/sysctl.3.html

import Foreign
import Foreign.C.Types
import Foreign.C.Error (getErrno, errnoToIOError)
import Foreign.C.String (peekCString)


foreign import ccall "sysctl"
    -- args: name, namelen, oldp, oldlenp, newp, newlenp
    c_sysctl :: Ptr CInt -> CUInt -> Ptr () -> Ptr CSize -> Ptr () -> CSize -> IO CInt

getSysctlSize :: [CInt] -> IO (Either String CSize)
getSysctlSize name = alloca $ \sizePtr -> do
    let len = fromIntegral $ length name
    namePtr <- listToPtr name
    poke sizePtr (0 :: CSize)
    kr <- c_sysctl namePtr len nullPtr sizePtr nullPtr 0
    if kr == -1
        then Left <$> getErrnoStr "getSysctlSize"
        else Right <$> peek sizePtr

getSysctl :: [CInt] -> IO (Either String String)
getSysctl name = alloca $ \bufferPtr -> alloca $ \sizePtr -> do
    let len = fromIntegral $ length name
    namePtr <- listToPtr name
    maybeSize <- getSysctlSize name
    case maybeSize of
        (Right size) -> do
            poke sizePtr size
            kr <- c_sysctl namePtr len (castPtr bufferPtr) sizePtr nullPtr 0
            if kr == 0
                then Right <$> peekCString bufferPtr
                else Left <$> getErrnoStr "getSysctl"
        (Left err) -> return $ Left err

listToPtr :: (Storable a) => [a] -> IO (Ptr a)
listToPtr xs = do
    ptr <- mallocArray (length xs)
    pokeArray ptr xs
    return ptr

getErrnoStr :: String -> IO String
getErrnoStr s = do
    errno <- getErrno
    let err = errnoToIOError s errno Nothing Nothing
    return $ show err