{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}
module PsInfo.Darwin.Libproc
    ( getProcTaskInfo
    , ProcTaskInfo (..)
    , getListPids
    ) where

import Foreign (Ptr, Storable(..), alloca, allocaArray, castPtr, peekArray, nullPtr)
import Foreign.C.Types (CInt(..), CUInt(..), CULong)
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.C (peekCString)

import Control.Monad.Freer (Eff, Members, send)
import Control.Monad.Freer.Error (Error)

import PsInfo.Util.Effect (eitherToEff)

data ProcTaskInfo = ProcTaskInfo
    { pti_virtual_size      :: CULong
    , pti_resident_size     :: CULong
    , pti_total_user        :: CULong
    , pti_total_system      :: CULong
    , pti_threads_user      :: CULong
    , pti_threads_system    :: CULong
    , pti_policy            :: CInt
    , pti_faults            :: CInt
    , pti_pageins           :: CInt
    , pti_cow_faults        :: CInt
    , pti_messages_sent     :: CInt
    , pti_messages_received :: CInt
    , pti_syscalls_mach     :: CInt
    , pti_syscalls_unix     :: CInt
    , pti_csw               :: CInt
    , pti_threadnum         :: CInt
    , pti_numrunning        :: CInt
    , pti_priority          :: CInt
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

foreign import ccall unsafe "libproc.h proc_pidinfo"
    -- int proc_pidinfo(int pid, int flavor, uint64_t arg, void *buffer, int buffersize)
    c_proc_pidinfo :: CInt -> CInt -> CUInt -> Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "libproc.h proc_listpids"
    -- int proc_listpids(uint32_t type, uint32_t typeinfo, void *buffer, int buffersize)
    c_proc_listpids :: CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall unsafe "libproc.h proc_pidpath"
    -- int proc_pidpath(int pid, void *buffer, uint32_t buffersize)
    c_proc_pidpath :: CInt -> Ptr () -> CUInt -> IO CInt

_PROC_PIDTASKINFO :: CInt
_PROC_PIDTASKINFO = 4

_PROC_ALL_PIDS :: CInt
_PROC_ALL_PIDS = 1

_PROC_MAX_PID_COUNT :: Int
_PROC_MAX_PID_COUNT = 4096

_PROC_PIDPATHINFO_MAXSIZE :: CUInt
_PROC_PIDPATHINFO_MAXSIZE = 1024 * 4

getProcTaskInfo :: (Members '[Error String, IO] r) => CInt -> Eff r ProcTaskInfo
getProcTaskInfo pid = eitherToEff $ alloca $ \ptr -> do
    let ptrSize = fromIntegral (sizeOf (undefined :: ProcTaskInfo))
    br <- c_proc_pidinfo pid _PROC_PIDTASKINFO 0 (castPtr ptr) ptrSize
    if br == -1
        then Left <$> getErrnoString "getProcTaskInfo"
        else Right <$> peek ptr

getListPids :: Members '[Error String, IO] r => Eff r [CInt]
getListPids = do 
    bufferSize <- send $ c_proc_listpids _PROC_ALL_PIDS 0 nullPtr 0
    eitherToEff $ allocaArray (fromIntegral bufferSize `div` sizeOf (undefined :: CInt)) $ \bufferPtr -> do
        bytes <- c_proc_listpids _PROC_ALL_PIDS 0 bufferPtr bufferSize
        if bytes == -1
            then Left <$> getErrnoString "getListPids"
            else do
                let numPids = fromIntegral bytes `div` sizeOf (undefined :: CInt)
                pids <- peekArray numPids bufferPtr
                return $ Right pids

getErrnoString :: String -> IO String
getErrnoString who = getErrno >>= \err -> pure (show $ errnoToIOError who err Nothing Nothing)
