{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs #-}
module PsInfo.Linux.Proc where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

import Control.Applicative ( (<|>) )
import Control.Monad.Freer ( send, Eff, Members )
import Control.Monad.Freer.Error ( throwError, Error )
import Data.Char ( isDigit )
import Data.Functor ( ($>) )
import System.Directory ( listDirectory )
import System.Process ( readProcess )

---------- Basic Types ----------

type PID = Int
type GID = Integer
type SID = Integer
type TTY = Integer
type Ticks = Integer
type Jiffies = Integer
type UnixTimestamp = Integer
type Bytes = Integer
type KiloBytes = Integer
type Pages = Integer

bytesTokB :: Bytes -> KiloBytes
bytesTokB b = div b 1024

getPageSize :: IO Integer
getPageSize = read <$> readProcess "getconf" ["PAGE_SIZE"] ""

pagesToBytes :: Pages -> IO Bytes
pagesToBytes p = (p *) <$> getPageSize

---------- General Helpers ----------

readAndParse :: (Members '[Error String, IO] r) => FilePath -> A.Parser a -> Eff r a
readAndParse fp p = do
    raw <- send $ B.readFile fp
    case A.parse p raw of
        (A.Done _ res) -> pure res
        (A.Partial pRest) -> case pRest "" of
            (A.Done _ res) -> pure res
            _ -> throwError ("Failed to parse nothing!" :: String)
        (A.Fail _ _ err) -> throwError err

pLineUntil :: A.Parser a -> A.Parser a
pLineUntil p = p <|> (A.takeTill (=='\n') *> A.endOfLine *> pLineUntil p)

---------- /PROC ----------

getPIDs :: IO [PID]
getPIDs = do
    paths <- listDirectory "/proc"
    let pidstrs = filter (all isDigit) paths
        pids = read <$> pidstrs
    pure pids

---------- /PROC/STAT ----------

getStat :: (Members '[Error String, IO] r) => Eff r Stat
getStat = readAndParse "/proc/stat" pStat

getCPUActive :: (Members '[Error String, IO] r) => Eff r Jiffies
getCPUActive = extractCPUActive <$> getStat

getCPUTime :: (Members '[Error String, IO] r) => Eff r Jiffies
getCPUTime = extractCPUTime <$> getStat

extractCPUTime :: Stat -> Jiffies
extractCPUTime stat = let statCPU = statCPUTotal stat
    in statCPUUser statCPU
     + statCPUNice statCPU
     + statCPUSystem statCPU
     + statCPUIdle statCPU
     + statCPUIOWait statCPU
     + statCPUIRQ statCPU
     + statCPUSoftIRQ statCPU
     + statCPUSteal statCPU
     + statCPUGuest statCPU
     + statCPUGuestNice statCPU

extractCPUActive :: Stat -> Jiffies
extractCPUActive stat = let statCPU = statCPUTotal stat
    in statCPUUser statCPU
     + statCPUNice statCPU
     + statCPUSystem statCPU
    -- + statCPUIdle statCPU
    -- + statCPUIOWait statCPU
     + statCPUIRQ statCPU
     + statCPUSoftIRQ statCPU
     + statCPUSteal statCPU
     + statCPUGuest statCPU
     + statCPUGuestNice statCPU

data StatCPU = StatCPU
    { statCPUUser      :: Jiffies
    , statCPUNice      :: Jiffies
    , statCPUSystem    :: Jiffies
    , statCPUIdle      :: Jiffies
    , statCPUIOWait    :: Jiffies
    , statCPUIRQ       :: Jiffies
    , statCPUSoftIRQ   :: Jiffies
    , statCPUSteal     :: Jiffies
    , statCPUGuest     :: Jiffies
    , statCPUGuestNice :: Jiffies
    } deriving (Eq, Show)

data Stat = Stat
    { statCPUTotal  :: StatCPU
    , statCPUs      :: [StatCPU]
    , statCtxt      :: Integer
    , statBTime     :: UnixTimestamp
    , statProcesses :: Integer
    , statRunning   :: Integer
    , statBlocked   :: Integer
    } deriving (Eq, Show)

pStatArg :: A.Parser n -> A.Parser a -> A.Parser a
pStatArg pName pVal = pName *> A.many1 A.space *> pVal

pStatCPU :: A.Parser StatCPU
pStatCPU = pStatArg ("cpu" *> A.many' A.digit) $ StatCPU
    <$> A.decimal
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)
    <*> (A.many1 A.space *> A.decimal)

pStat :: A.Parser Stat
pStat = Stat
    <$> (pStatCPU <* A.endOfLine)
    <*> A.many' (pStatCPU <* A.endOfLine)
    <*> (pLineUntil (pStatArg "ctxt" A.decimal) <* A.endOfLine)
    <*> (pLineUntil (pStatArg "btime" A.decimal) <* A.endOfLine)
    <*> (pLineUntil (pStatArg "processes" A.decimal) <* A.endOfLine)
    <*> (pLineUntil (pStatArg "procs_running" A.decimal) <* A.endOfLine)
    <*> (pLineUntil (pStatArg "procs_blocked" A.decimal) <* A.endOfLine)
    <* A.takeWhile (const True)
    <* A.endOfInput

---------- /PROC/MEMINFO ----------

getMemInfo :: (Members '[Error String, IO] r) => Eff r MemInfo
getMemInfo = readAndParse "/proc/meminfo" pMemInfo

getMemTotal :: (Members '[Error String, IO] r) => Eff r KiloBytes
getMemTotal = memInfoMemTotal <$> getMemInfo

getMemActive :: (Members '[Error String, IO] r) => Eff r KiloBytes
getMemActive = calcMemActive <$> getMemInfo

calcMemActive :: MemInfo -> KiloBytes
calcMemActive mi = memInfoMemTotal mi
                - memInfoMemFree mi
                - memInfoBuffers mi
                - memInfoCached mi

data MemInfo = MemInfo
    { memInfoMemTotal     :: KiloBytes
    , memInfoMemFree      :: KiloBytes
    , memInfoMemAvailable :: KiloBytes
    , memInfoBuffers      :: KiloBytes
    , memInfoCached       :: KiloBytes
    , memInfoSwapCached   :: KiloBytes
    , memInfoActive       :: KiloBytes
    , memInfoInactive     :: KiloBytes
    , memInfoActiveAnon   :: KiloBytes
    , memInfoInactiveAnon :: KiloBytes
    , memInfoActiveFile   :: KiloBytes
    , memInfoInactiveFile :: KiloBytes
    , memInfoUnevictable  :: KiloBytes
    , memInfoMlocked      :: KiloBytes
    , memInfoSwapTotal    :: KiloBytes
    , memInfoSwapFree     :: KiloBytes
    , memInfoDirty        :: KiloBytes
    , memInfoWriteback    :: KiloBytes
    , memInfoAnonPages    :: KiloBytes
    , memInfoMapped       :: KiloBytes
    , memInfoShmem        :: KiloBytes
    , memInfoSlab         :: KiloBytes
    , memInfoSReclaimable :: KiloBytes
    , memInfoSUnreclaim   :: KiloBytes
    , memInfoKernelStack  :: KiloBytes
    , memInfoPageTables   :: KiloBytes
    , memInfoNFSUnstable  :: KiloBytes
    , memInfoBounce       :: KiloBytes
    , memInfoWritebackTmp :: KiloBytes
    , memInfoCommitLimit  :: KiloBytes
    , memInfoCommittedAS  :: KiloBytes
    , memInfoVmallocTotal :: KiloBytes
    , memInfoVmallocUsed  :: KiloBytes
    , memInfoVmallocChunk :: KiloBytes
    , memInfoPercpu       :: KiloBytes
    -- ... may be more fields dependant on system
    }

pMemInfoArg :: A.Parser n -> A.Parser a -> A.Parser a
pMemInfoArg pName pVal = pName *> ":" *> A.many1 A.space *> pVal

pMemInfo :: A.Parser MemInfo
pMemInfo = MemInfo
    <$> pLineUntil (pMemInfoArg "MemTotal"       (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "MemFree"        (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "MemAvailable"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Buffers"        (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Cached"         (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "SwapCached"     (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Active"         (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Inactive"       (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Active(anon)"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Inactive(anon)" (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Active(file)"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Inactive(file)" (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Unevictable"    (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Mlocked"        (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "SwapTotal"      (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "SwapFree"       (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Dirty"          (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Writeback"      (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "AnonPages"      (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Mapped"         (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Shmem"          (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Slab"           (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "SReclaimable"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "SUnreclaim"     (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "KernelStack"    (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "PageTables"     (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "NFS_Unstable"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Bounce"         (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "WritebackTmp"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "CommitLimit"    (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Committed_AS"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "VmallocTotal"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "VmallocUsed"    (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "VmallocChunk"   (A.decimal <* A.space <* "kB"))
    <*> pLineUntil (pMemInfoArg "Percpu"         (A.decimal <* A.space <* "kB"))
    <* A.takeWhile (const True)
    <* A.endOfInput

---------- /PROC/PID/STAT ----------

getPIDStat :: (Members '[Error String, IO] r) => PID -> Eff r PIDStat
getPIDStat pid = readAndParse ("/proc/" ++ show pid ++ "/stat") pPIDStat

getProcessCPUTime :: (Members '[Error String, IO] r) => PID -> Eff r Ticks
getProcessCPUTime = (extractPIDCPUTime <$>) . getPIDStat

extractPIDCPUTime :: PIDStat -> Ticks
extractPIDCPUTime stat = pidStatUTime stat + pidStatSTime stat

data ProcessState = Running
                  | Sleeping
                  | SleepingUninteruptably
                  | Zombie
                  | Terminated
    deriving (Eq, Show)

data PIDStat = PIDStat
    { pidStatPID         :: PID
    , pidStatComm        :: String
    , pidStatState       :: ProcessState
    , pidStatPPID        :: PID
    , pidStatPGRP        :: GID
    , pidStatSession     :: SID
    , pidStatTTY         :: TTY
    , pidStatTPGID       :: GID
    , pidStatFlags       :: Integer
    , pidStatMinFlt      :: Integer
    , pidStatCMinFlt     :: Integer
    , pidStatMajFlt      :: Integer
    , pidStatCMajFlt     :: Integer
    , pidStatUTime       :: Ticks
    , pidStatSTime       :: Ticks
    , pidStatCUTime      :: Ticks
    , pidStatCSTime      :: Ticks
    , pidStatPriority    :: Integer
    , pidStatNice        :: Integer
    , pidStatNumThreads  :: Integer
    , pidStatItRealValue :: Integer
    , pidStatStartTime   :: Ticks
    , pidStatVSize       :: Bytes
    , pidStatRSS         :: Pages
    , pidStatRSSLim      :: Bytes
    -- ... memory/scheduling fields + system spasific
    } deriving (Eq, Show)

pPIDStatComm :: A.Parser String
pPIDStatComm = "(" *> (B.unpack <$> A.takeTill (==')')) <* ")"

pPIDStatState :: A.Parser ProcessState
pPIDStatState = ("R" $> Running)
            <|> ("S" $> Sleeping)
            <|> ("D" $> SleepingUninteruptably)
            <|> ("Z" $> Zombie)
            <|> ("T" $> Terminated)

pPIDStat :: A.Parser PIDStat
pPIDStat = PIDStat
    <$> A.decimal
    <*> (A.space *> pPIDStatComm)
    <*> (A.space *> pPIDStatState)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.signed A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.signed A.decimal)
    <*> (A.space *> A.signed A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <* A.takeWhile (const True)
    <* A.endOfInput

---------- /PROC/PID/STATM ----------

getPIDStatm :: (Members '[Error String, IO] r) => PID -> Eff r PIDStatm
getPIDStatm pid = readAndParse ("/proc/" ++ show pid ++ "/statm") pPIDStatm

getProcessMemUsage :: (Members '[Error String, IO] r) => PID -> Eff r KiloBytes
getProcessMemUsage pid = getPIDStatm pid >>= send . calcPIDMemUsage

calcPIDMemUsage :: PIDStatm -> IO KiloBytes
calcPIDMemUsage statm = bytesTokB <$> pagesToBytes (pidStatmRSS statm)

data PIDStatm = PIDStatm
    { pidStatmSize   :: Pages
    , pidStatmRSS    :: Pages
    , pidStatmShared :: Pages
    , pidStatmText   :: Pages
    , pidStatmLib    :: Pages
    , pidStatmData   :: Pages
    , pidStatmDT     :: Pages
    } deriving (Eq, Show)

pPIDStatm :: A.Parser PIDStatm
pPIDStatm = PIDStatm
    <$> A.decimal
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <*> (A.space *> A.decimal)
    <* A.takeWhile (const True)
    <* A.endOfInput
