{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}
module PsInfo.Darwin.Mach
    ( getSelfPID
    , getTaskForPid
    , getTaskInfoForTask
    , TimeValue (..)
    , TaskBasicInfo (..)
    , getHostStatistics
    , HostStatistics (..)
    , getMachTimebaseInfo
    , MachTimebaseInfo (..)
    , getMachAbsoluteTime
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

data HostStatistics = HostStatistics
    { hsUser   :: CUInt
    , hsSystem :: CUInt
    , hsIdle   :: CUInt
    , hsNice   :: CUInt
    } deriving (Eq, Show)

instance Storable HostStatistics where
    sizeOf _ = sizeOf (undefined :: CInt) * 4
    alignment _ = alignment (undefined :: CInt)
    peek ptr = HostStatistics
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: CInt))
        <*> peekByteOff ptr (sizeOf (undefined :: CInt) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CInt) * 3)
    poke _ _ = error "poke not implemented"

data MachTimebaseInfo = MachTimebaseInfo
    { mti_denom :: CUInt
    , mti_number :: CUInt
    } deriving (Eq, Show)

instance Storable MachTimebaseInfo where
    sizeOf _ = sizeOf (undefined :: CUInt) * 2
    alignment _ = alignment (undefined :: CUInt)
    peek ptr = MachTimebaseInfo
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: CUInt))
    poke _ _ = error "poke not implemented"

foreign import ccall unsafe "sys/mach.h mach_host_self"
    -- mach_port_t mach_host_self(void)
    c_mach_host_self :: IO CUInt

foreign import ccall unsafe "sys/mach.h pid_for_task"
    -- kern_return_t pid_for_task(mach_port_name_t t, int *x)
    c_pid_for_task :: CUInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "sys/mach.h task_for_pid"
    -- kern_return_t task_for_pid(mach_port_name_t target_tport, int pid, mach_port_name_t *t)
    c_task_for_pid :: CUInt -> CInt -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "sys/mach.h task_info"
    -- kern_return_t task_info(task_name_t target_task, task_flavor_t flavor, task_info_t task_info_out, mach_msg_type_number_t *task_info_outCnt)
    c_task_info :: CUInt -> CUInt -> Ptr () -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "sys/mach.h host_statistics"
    -- kern_return_t host_statistics(host_t host_priv, host_flavor_t flavor, host_info_t host_info_out, mach_msg_number_t host_info_outCnt)
    c_host_statistics :: CUInt -> CInt -> Ptr () -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "mach/mach_time.h mach_timebase_info"
    -- kern_return_t mach_timebase_info(mach_timebase_info_t info)
    c_mach_timebase_info :: Ptr MachTimebaseInfo -> IO CInt

foreign import ccall unsafe "mach/mach_time.h mach_absolute_time"
    -- uint64_t mach_absolute_time(void)
    c_mach_absolute_time :: IO CULLong

_MACH_TASK_BASIC_INFO :: CUInt
_MACH_TASK_BASIC_INFO = 20

_HOST_CPU_LOAD_INFO :: CInt
_HOST_CPU_LOAD_INFO = 3

_CPU_STATE_MAX :: CUInt
_CPU_STATE_MAX = 4

getSelfPID :: Members '[Error String, IO] r => Eff r CInt
getSelfPID = eitherToEff $ alloca $ \pidPtr -> do 
    host <- c_mach_host_self
    kr <- c_pid_for_task host (castPtr pidPtr)
    if kr == 0
        then Right <$> peek pidPtr
        else return $ Left $ "getSelfPID failed! reason: " ++ showKR kr

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

getHostStatistics :: Members '[Error String, IO] r => Eff r HostStatistics
getHostStatistics = eitherToEff $ alloca $ \statsPtr -> alloca $ \sizePtr -> do
    host <- c_mach_host_self
    poke sizePtr _CPU_STATE_MAX
    kr <- c_host_statistics host _HOST_CPU_LOAD_INFO (castPtr statsPtr) sizePtr
    if kr == 0
        then Right <$> peek statsPtr
        else return $ Left $ "getHostStatistics failed! reason: " <> showKR kr

getMachTimebaseInfo :: Members '[Error String, IO] r => Eff r MachTimebaseInfo
getMachTimebaseInfo = eitherToEff $ alloca $ \ptr -> do
    kr <- c_mach_timebase_info ptr
    if kr == 0
        then Right <$> peek ptr
        else return $ Left $ "getMachTimebaseInfo failed! reason: " <> showKR kr

getMachAbsoluteTime :: IO Integer
getMachAbsoluteTime = fromIntegral <$> c_mach_absolute_time

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
