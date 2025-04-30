{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}
module PsInfo.Darwin.Rusage 
    ( getRusage
    , RUsage (..)
    , Timeval (..)
    , _RUSAGE_SELF
    , _RUSAGE_CHILDREN
    ) where

import Foreign (Ptr, Storable(..), alloca)
import Foreign.C (getErrno, errnoToIOError)
import Foreign.C.Types (CInt(..), CLong)

import Control.Monad.Freer ( Members, Eff )
import Control.Monad.Freer.Error ( Error )

import PsInfo.Util.Effect ( eitherToEff )

data Timeval = Timeval 
    { tv_sec :: CLong
    , tv_usec :: CLong
    } deriving (Show, Eq)

instance Storable Timeval where
    sizeOf _ = sizeOf (undefined :: CLong) * 2
    alignment _ = alignment (undefined :: CLong)
    peek ptr = Timeval
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: CLong))
    poke ptr (Timeval sec usec) = do
        pokeByteOff ptr 0 sec
        pokeByteOff ptr (sizeOf (undefined :: CLong)) usec

data RUsage = RUsage 
    { ru_utime :: Timeval   -- user time used
    , ru_stime :: Timeval   -- system time used
    , ru_maxrrs :: CLong    -- integral max resident set size (kB)
    , ru_ixrrs :: CLong     -- integral shared text memory size (kB * ticks)
    , ru_idrss :: CLong     -- integral unshared data size (kB * ticks)
    , ru_isrrs :: CLong     -- integral unshared stack size (kB * ticks)
    , ru_minflt :: CLong    -- page reclaims
    , ru_majflt :: CLong    -- page faults
    , ru_nswap :: CLong     -- swaps
    , ru_inblock :: CLong   -- block input operations
    , ru_oublock :: CLong   -- block output operations
    , ru_msgsnd :: CLong    -- messages sent
    , ru_msgrcv :: CLong    -- messages received
    , ru_nsignals :: CLong  -- signals received
    , ru_nvcsw :: CLong     -- voluntary context switches
    , ru_nivcsw :: CLong    -- involuntary context switches
    } deriving (Show, Eq)

instance Storable RUsage where
    sizeOf _ = sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 14
    alignment _ = alignment (undefined :: Timeval)
    peek ptr = RUsage
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval))
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong))
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 2)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 3)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 4)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 5)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 6)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 7)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 8)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 9)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 10)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 11)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 12)
        <*> peekByteOff ptr (sizeOf (undefined :: Timeval) * 2 + sizeOf (undefined :: CLong) * 13)
    poke _ _ = error "poke not implemented"

foreign import ccall unsafe "sys/resource.h getrusage"
    -- args: who, output
    c_getrusage :: CInt -> Ptr RUsage -> IO CInt

_RUSAGE_SELF :: CInt
_RUSAGE_SELF = 0

_RUSAGE_CHILDREN :: CInt
_RUSAGE_CHILDREN = -1

getRusage :: Members '[Error String, IO] r => CInt -> Eff r RUsage
getRusage who = eitherToEff $ alloca $ \rusagePtr -> do
    kr <- c_getrusage who rusagePtr
    if kr == 0
        then do
            rusage <- peek rusagePtr
            return $ Right rusage
        else do
            errno <- getErrno
            let err = errnoToIOError "getRusage" errno Nothing Nothing
            return $ Left $ show err
