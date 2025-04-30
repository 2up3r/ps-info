{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}
module PsInfo.Darwin.Mach
    , getTaskInfoForTask
    , TimeValue (..)
    , TaskBasicInfo (..)
    ) where

import Foreign (Ptr, Storable(..), alloca, castPtr)
import Foreign.C.Types (CInt(..), CLong, CUInt(..), CULLong(..))

import Control.Monad.Freer (Eff, Members)
import Control.Monad.Freer.Error (Error)

import PsInfo.Util.Effect (eitherToEff)

data TimeValue = TimeValue
    { tv_seconds :: CInt
    , tv_microseconds :: CInt
    } deriving (Show, Eq)

instance Storable TimeValue where
    sizeOf _ = sizeOf (undefined :: CInt) * 2
    alignment _ = alignment (undefined :: CInt)
    peek ptr = TimeValue
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: CInt))
    poke ptr (TimeValue s ms) = do
        pokeByteOff ptr 0 s
        pokeByteOff ptr (sizeOf (undefined :: CInt)) ms

data TaskBasicInfo = TaskBasicInfo
    { tbi_virtural_memory :: CLong
    , tbi_resident_size :: CLong
    , tbi_resident_size_max :: CLong
    , tbi_user_time :: TimeValue
    , tbi_system_time :: TimeValue
    , tbi_policy :: CInt
    , tbi_suspend_count :: CInt
    } deriving (Show, Eq)

instance Storable TaskBasicInfo where
    sizeOf _ = sizeOf (undefined :: CLong) * 3
             + sizeOf (undefined :: TimeValue) * 2
             + sizeOf (undefined :: CInt) * 2
    alignment _ = alignment (undefined :: CLong)
    peek ptr = TaskBasicInfo
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: CLong))
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3 + sizeOf (undefined :: TimeValue))
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3 + sizeOf (undefined :: TimeValue) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3 + sizeOf (undefined :: TimeValue) * 2 + sizeOf (undefined :: CInt))
    poke _ _ = error "poke not implemented"

foreign import ccall unsafe "sys/mach.h mach_host_self"
    -- mach_port_t mach_host_self(void)
    c_mach_host_self :: IO CUInt

foreign import ccall unsafe "sys/mach.h task_for_pid"
    -- kern_return_t task_for_pid(mach_port_name_t target_tport, int pid, mach_port_name_t *t)
    c_task_for_pid :: CUInt -> CInt -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "sys/mach.h task_info"
    -- kern_return_t task_info(task_name_t target_task, task_flavor_t flavor, task_info_t task_info_out, mach_msg_type_number_t *task_info_outCnt)
    c_task_info :: CUInt -> CUInt -> Ptr () -> Ptr CUInt -> IO CInt

_MACH_TASK_BASIC_INFO :: CUInt
_MACH_TASK_BASIC_INFO = 20

_HOST_CPU_LOAD_INFO :: CInt
_HOST_CPU_LOAD_INFO = 3

_CPU_STATE_MAX :: CUInt
_CPU_STATE_MAX = 4

getTaskForPid :: Members '[Error String, IO] r => CInt -> Eff r CUInt
getTaskForPid pid = eitherToEff $ alloca $ \ptr -> do
    host <- c_mach_host_self
    kr <- c_task_for_pid host pid ptr
    if kr == 0
        then Right <$> peek ptr
        else return $ Left $ "getTaskForPid failed! reason:" <> showKR kr

getTaskInfoForTask :: Members '[Error String, IO] r => CUInt -> Eff r TaskBasicInfo
getTaskInfoForTask task = eitherToEff $ alloca $ \infoPtr -> alloca $ \sizePtr -> do
    poke sizePtr (fromIntegral (sizeOf (undefined :: TaskBasicInfo)) :: CUInt)
    kr <- c_task_info task _MACH_TASK_BASIC_INFO (castPtr infoPtr) sizePtr
    if kr == 0
        then Right <$> peek infoPtr
        else return $ Left $ "getTaskInfoForTask failed! reason:" <> showKR kr

showKR :: CInt -> String
showKR 0 = "KERN_SUCCESS"
showKR 1 = "KERN_INVALID_ADDRESS"
showKR 2 = "KERN_PROTECTION_FAILURE"
showKR 3 = "KERN_NO_SPACE"
showKR 4 = "KERN_INVALID_ARGUMENT"
showKR 5 = "KERN_FAILURE"
showKR 6 = "KERN_RESOURCE_SHORTAGE"
showKR 7 = "KERN_NOT_RECEIVER"
showKR 8 = "KERN_NO_ACCESS"
showKR kr = show kr
