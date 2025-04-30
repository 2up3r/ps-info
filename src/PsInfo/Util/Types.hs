module PsInfo.Util.Types where

-- General
type Percent = Double

-- ID
newtype PID = PID Int
    deriving (Eq, Show, Ord)

newtype GID = GID Int
    deriving (Eq, Show, Ord)

newtype SID = SID Int
    deriving (Eq, Show, Ord)

newtype TTY = TTY Int
    deriving (Eq, Show, Ord)

-- Time
type Tick = Integer
type Jiffy = Integer
type MicroSecond = Integer
type UnixTimestamp = Integer

-- Memory
type Page = Integer
type Byte = Integer
type KiloByte = Integer
