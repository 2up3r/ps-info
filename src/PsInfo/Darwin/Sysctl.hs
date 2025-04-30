{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}
module PsInfo.Darwin.Sysctl
    ( getSysctl
    ) where

import Foreign (Ptr, Storable(peek, poke), alloca, castPtr, mallocArray, nullPtr, pokeArray)
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.C.Types (CInt(..), CSize(..), CUInt(..))

import Control.Monad.Freer (Members, Eff, runM)
import Control.Monad.Freer.Error (Error, runError)

import PsInfo.Util.Effect (eitherToEff)

foreign import ccall unsafe "sysctl"
    -- int sysctl(int *, u_int, void *, size_t *oldlenp, void *, size_t newlen)
    c_sysctl :: Ptr CInt -> CUInt -> Ptr () -> Ptr CSize -> Ptr () -> CSize -> IO CInt

getSysctlSize :: (Members '[Error String, IO] r) => [CInt] -> Eff r CSize
getSysctlSize name = eitherToEff $ alloca $ \sizePtr -> do
    let len = fromIntegral $ length name
    namePtr <- listToPtr name
    poke sizePtr (0 :: CSize)
    kr <- c_sysctl namePtr len nullPtr sizePtr nullPtr 0
    if kr == -1
        then Left <$> getErrnoStr ("getSysctlSize " ++ show kr)
        else Right <$> peek sizePtr

getSysctl' :: (Storable a) => [CInt] -> IO (Either String a)
getSysctl' name = alloca $ \bufferPtr -> alloca $ \sizePtr -> do
    let len = fromIntegral $ length name
    namePtr <- listToPtr name
    maybeSize <- runM $ runError $ getSysctlSize name
    case maybeSize of
        (Left err) -> return $ Left err
        (Right size) -> do
            poke sizePtr size
            kr <- c_sysctl namePtr len (castPtr bufferPtr) sizePtr nullPtr 0
            if kr == 0
                then Right <$> peek bufferPtr
                else Left <$> getErrnoStr ("getSysctl " ++ show kr)

getSysctl :: (Storable a, Members '[Error String, IO] r) => [CInt] -> Eff r a
getSysctl name = eitherToEff $ getSysctl' name

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
