{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs #-}
module PsInfo.Darwin.Mach 
    ( getTaskForPid
    , getTaskInfoForTask
    , TimeValue
    , TaskBasicInfo
    ) where
-- https://github.com/apple-oss-distributions/xnu/

import Foreign
import Foreign.C.Types


foreign import ccall "sys/mach.h task_for_pid"
    -- args: target_port, pid, task
    c_task_for_pid :: CInt -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "sys/mach.h task_info"
    -- args: task, flavor, task_info, task_info_count
    c_task_info :: CInt -> CInt -> Ptr TaskBasicInfo -> Ptr CInt -> IO CInt

foreign import ccall "mach_host_self"
    c_mach_host_self :: IO CInt

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
    { tbi_virtural_memory :: CLong -- (bytes)
    , tbi_resident_size :: CLong -- (bytes)
    , tbi_resident_size_max :: CLong -- (bytes)
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
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 1)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3 + sizeOf (undefined :: TimeValue) * 1)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3 + sizeOf (undefined :: TimeValue) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: CLong) * 3 + sizeOf (undefined :: TimeValue) * 2 + sizeOf (undefined :: CInt) * 1)
    poke _ _ = error "poke not implemented"

_MACH_TASK_BASIC_INFO :: CInt
_MACH_TASK_BASIC_INFO = 20

getTaskForPid :: CInt -> IO (Either String CInt)
getTaskForPid pid = alloca $ \ptr -> do
    host <- c_mach_host_self
    kr <- c_task_for_pid host pid ptr
    if (kr == 0)
        then Right <$> peek ptr
        else return $ Left $ "getTaskForPid failed! reason:" <> showKR kr

getTaskInfoForTask :: CInt -> IO (Either String TaskBasicInfo)
getTaskInfoForTask task = alloca $ \infoPtr -> alloca $ \sizePtr -> do
    poke sizePtr (fromIntegral (sizeOf (undefined :: TaskBasicInfo)) :: CInt)
    kr <- c_task_info task _MACH_TASK_BASIC_INFO infoPtr sizePtr
    if (kr == 0)
        then Right <$> peek infoPtr
        else return $ Left $ "getTaskInfoForTask failed! reason:" <> showKR kr

-- https://github.com/apple-oss-distributions/xnu/blob/main/osfmk/mach/kern_return.h
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
