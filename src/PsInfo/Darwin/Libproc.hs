{-# LANGUAGE ForeignFunctionInterface, DataKinds, FlexibleContexts, GADTs #-}
module PsInfo.Darwin.Libproc
    ( getListPids
    , getProcTaskInfo
    , ProcTaskInfo
    ) where
-- https://github.com/phracker/MacOSX-SDKs/blob/041600eda65c6a668f66cb7d56b7d1da3e8bcc93/MacOSX10.5.sdk/usr/include/libproc.h#L85

import Control.Monad.Freer ( send, Eff, Members )
import Control.Monad.Freer.Error ( throwError, Error )
import Foreign
import Foreign.C.Types
import Foreign.C.Error (getErrno, errnoToIOError)

foreign import ccall "proc_pidinfo"
    -- args: pid, flavor, arg, buffer, buffersize
    c_proc_pidinfo :: CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt

data ProcTaskInfo = ProcTaskInfo
    { pti_virtual_size :: CULong
    , pit_resident_size :: CULong
    , pti_total_user :: CULong
    , pti_total_system :: CULong
    , pti_threads_user :: CULong
    , pti_threads_system :: CULong
    , pti_policy :: CInt
    , pti_faults :: CInt
    , pti_pageins :: CInt
    , pti_cow_faults :: CInt
    , pti_messages_sent :: CInt
    , pti_messages_received :: CInt
    , pti_syscalls_mach :: CInt
    , pti_syscalls_unix :: CInt
    , pti_csw :: CInt
    , pti_threadnum :: CInt
    , pti_numrunning :: CInt
    , pti_priority :: CInt
    } deriving (Eq, Show)

instance Storable ProcTaskInfo where
    sizeOf _ = sizeOf (undefined :: CULong) * 6
             + sizeOf (undefined :: CInt) * 12
    alignment _ = alignment (undefined :: CULong)
    peek ptr = ProcTaskInfo
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: CULong))
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 3)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 4)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 5)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt))
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 3)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 4)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 5)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 6)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 7)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 8)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 9)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 10)
        <*> peekByteOff ptr (sizeOf (undefined :: CULong) * 6 + sizeOf (undefined :: CInt) * 11)
    poke _ _ = error "poke not implemented"

foreign import ccall "proc_listpids"
    -- args: type, type_info, buffer, buffer_size
    c_proc_listpids :: CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

_PROC_PIDTASKINFO :: CInt
_PROC_PIDTASKINFO = 4

getProcTaskInfo :: (Members '[Error String, IO] r) => CInt -> Eff r ProcTaskInfo
getProcTaskInfo pid = do
    epti <- send $ alloca $ \ptr -> do
        let ptrSize = fromIntegral (sizeOf (undefined :: ProcTaskInfo))
        br <- c_proc_pidinfo pid _PROC_PIDTASKINFO 0 (castPtr ptr) ptrSize
        if br == -1
            then do
                errno <- getErrno
                let err = errnoToIOError "getProcTaskInfo" errno Nothing Nothing
                return $ Left $ show err
            else Right <$> peek ptr
    case epti of
        (Left err) -> throwError err
        (Right pti) -> pure pti

_PROC_ALL_PIDS :: CInt
_PROC_ALL_PIDS = 1

_PROC_MAX_PID_COUNT :: Int
_PROC_MAX_PID_COUNT = 4096

getListPids :: Members '[Error String, IO] r => Eff r [CInt]
getListPids = do
    epids <- send $ allocaArray _PROC_MAX_PID_COUNT $ \bufferPtr -> do
        let bufferSize = fromIntegral $ _PROC_MAX_PID_COUNT * sizeOf (undefined :: CInt)
        bytes <- c_proc_listpids _PROC_ALL_PIDS 0 bufferPtr bufferSize
        if bytes == -1
            then do
                errno <- getErrno
                let err = errnoToIOError "getListPids" errno Nothing Nothing
                return $ Left $ show err
            else do
                let numPids = fromIntegral bytes `div` sizeOf (undefined :: CInt)
                pids <- ptrToList bufferPtr numPids
                return $ Right pids
    case epids of
        (Left err) -> throwError err
        (Right pids) -> pure pids

ptrToList :: Ptr CInt -> Int -> IO [CInt]
ptrToList bufferPtr len = mapM (peekElemOff bufferPtr) [0 .. (len - 1)]
