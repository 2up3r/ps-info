{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}
module PsInfo.Darwin.Mach
    ( getHostStatistics
    , HostStatistics (..)
    , getMachTimebaseInfo
    , MachTimebaseInfo (..)
    , getMachAbsoluteTime
    ) where

import Foreign (Ptr, Storable(..), alloca, castPtr)
import Foreign.C.Types (CInt(..), CUInt(..), CULLong(..))

import Control.Monad.Freer (Eff, Members)
import Control.Monad.Freer.Error (Error)

import PsInfo.Util.Effect (eitherToEff)

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

foreign import ccall unsafe "sys/mach.h host_statistics"
    -- kern_return_t host_statistics(host_t host_priv, host_flavor_t flavor, host_info_t host_info_out, mach_msg_number_t host_info_outCnt)
    c_host_statistics :: CUInt -> CInt -> Ptr () -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "mach/mach_time.h mach_timebase_info"
    -- kern_return_t mach_timebase_info(mach_timebase_info_t info)
    c_mach_timebase_info :: Ptr MachTimebaseInfo -> IO CInt

foreign import ccall unsafe "mach/mach_time.h mach_absolute_time"
    -- uint64_t mach_absolute_time(void)
    c_mach_absolute_time :: IO CULLong

_HOST_CPU_LOAD_INFO :: CInt
_HOST_CPU_LOAD_INFO = 3

_CPU_STATE_MAX :: CUInt
_CPU_STATE_MAX = 4

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
