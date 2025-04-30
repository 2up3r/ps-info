{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, DataKinds #-}
module PsInfo.Linux.Proc
    ( getPIDs
    , getStat
    , Stat (..)
    , StatCPU (..)
    , getCPUActive
    , getCPUTime
    , getMemInfo
    , getMemTotal
    , getMemActive
    , MemInfo (..)
    , getPIDStat
    , PIDStat (..)
    , ProcessState (..)
    , getProcessCPUTime
    , getPIDStatm
    , PIDStatm (..)
    , getProcessMemUsage
    , getClockTicksPerSecond
    ) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Functor (($>))
import Foreign.C.Types (CInt (..), CLong (..))

import Control.Monad.Freer (Eff, Members, send)
import Control.Monad.Freer.Error (Error, throwError)
import System.Directory (listDirectory)
import System.Directory.Internal.Prelude (tryIOError)
import System.Process (readProcess)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

import PsInfo.Util.Types

---------- Basic Types ----------

bytesTokB :: Byte -> KiloByte
bytesTokB b = div b 1024

getPageSize :: IO Page
getPageSize = read <$> readProcess "getconf" ["PAGE_SIZE"] ""

pagesToBytes :: Page -> IO Byte
pagesToBytes p = (p *) <$> getPageSize

---------- General Helpers ----------

readAndParse :: Members '[Error String, IO] r => FilePath -> A.Parser a -> Eff r a
readAndParse fp p = do
    raw <- send $ B.readFile fp
    case A.parse p raw of
        (A.Done _ res) -> pure res
        (A.Partial pRest) -> case pRest "" of
            (A.Done _ res) -> pure res
            _ -> throwError ("readAndParse: Failed to parse nothing!" :: String)
        (A.Fail _ _ err) -> throwError err

pLineUntil :: A.Parser a -> A.Parser a
pLineUntil p = p <|> (A.takeTill (=='\n') *> A.endOfLine *> pLineUntil p)

---------- /PROC ----------

getPIDs :: Members '[Error String, IO] r => Eff r [PID]
getPIDs = do
    mPaths <- send $ tryIOError $ listDirectory "/proc"
    case mPaths of
        (Left err) -> throwError $ "Failed to get PIDs: " ++ show err
        (Right paths) -> pure $ PID . read <$> filter (all isDigit) paths

---------- /PROC/STAT ----------

getStat :: Members '[Error String, IO] r => Eff r Stat
getStat = readAndParse "/proc/stat" pStat

getCPUActive :: Members '[Error String, IO] r => Eff r Jiffy
getCPUActive = extractCPUActive <$> getStat
    where
        extractCPUActive :: Stat -> Jiffy
        extractCPUActive stat = let statCPU = statCPUTotal stat
            in statCPUUser statCPU
            + statCPUNice statCPU
            + statCPUSystem statCPU
            + statCPUIRQ statCPU
            + statCPUSoftIRQ statCPU
            + statCPUSteal statCPU
            + statCPUGuest statCPU
            + statCPUGuestNice statCPU

getCPUTime :: Members '[Error String, IO] r => Eff r Jiffy
getCPUTime = extractCPUTime <$> getStat
    where
        extractCPUTime :: Stat -> Jiffy
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

data StatCPU = StatCPU
    { statCPUUser      :: Jiffy
    , statCPUNice      :: Jiffy
    , statCPUSystem    :: Jiffy
    , statCPUIdle      :: Jiffy
    , statCPUIOWait    :: Jiffy
    , statCPUIRQ       :: Jiffy
    , statCPUSoftIRQ   :: Jiffy
    , statCPUSteal     :: Jiffy
    , statCPUGuest     :: Jiffy
    , statCPUGuestNice :: Jiffy
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

getMemInfo :: Members '[Error String, IO] r => Eff r MemInfo
getMemInfo = readAndParse "/proc/meminfo" pMemInfo

getMemTotal :: Members '[Error String, IO] r => Eff r KiloByte
getMemTotal = memInfoMemTotal <$> getMemInfo

getMemActive :: Members '[Error String, IO] r => Eff r KiloByte
getMemActive = calcMemActive <$> getMemInfo

calcMemActive :: MemInfo -> KiloByte
calcMemActive mi = memInfoMemTotal mi
                - memInfoMemFree mi
                - memInfoBuffers mi
                - memInfoCached mi

data MemInfo = MemInfo
    { memInfoMemTotal     :: KiloByte
    , memInfoMemFree      :: KiloByte
    , memInfoMemAvailable :: KiloByte
    , memInfoBuffers      :: KiloByte
    , memInfoCached       :: KiloByte
    , memInfoSwapCached   :: KiloByte
    , memInfoActive       :: KiloByte
    , memInfoInactive     :: KiloByte
    , memInfoActiveAnon   :: KiloByte
    , memInfoInactiveAnon :: KiloByte
    , memInfoActiveFile   :: KiloByte
    , memInfoInactiveFile :: KiloByte
    , memInfoUnevictable  :: KiloByte
    , memInfoMlocked      :: KiloByte
    , memInfoSwapTotal    :: KiloByte
    , memInfoSwapFree     :: KiloByte
    , memInfoDirty        :: KiloByte
    , memInfoWriteback    :: KiloByte
    , memInfoAnonPages    :: KiloByte
    , memInfoMapped       :: KiloByte
    , memInfoShmem        :: KiloByte
    , memInfoSlab         :: KiloByte
    , memInfoSReclaimable :: KiloByte
    , memInfoSUnreclaim   :: KiloByte
    , memInfoKernelStack  :: KiloByte
    , memInfoPageTables   :: KiloByte
    , memInfoNFSUnstable  :: KiloByte
    , memInfoBounce       :: KiloByte
    , memInfoWritebackTmp :: KiloByte
    , memInfoCommitLimit  :: KiloByte
    , memInfoCommittedAS  :: KiloByte
    , memInfoVmallocTotal :: KiloByte
    , memInfoVmallocUsed  :: KiloByte
    , memInfoVmallocChunk :: KiloByte
    , memInfoPercpu       :: KiloByte
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

getPIDStat :: Members '[Error String, IO] r => PID -> Eff r PIDStat
getPIDStat pid = readAndParse ("/proc/" ++ show pid ++ "/stat") pPIDStat

getProcessCPUTime :: Members '[Error String, IO] r => PID -> Eff r Jiffy
getProcessCPUTime = (extractPIDCPUTime <$>) . getPIDStat

extractPIDCPUTime :: PIDStat -> Jiffy
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
    , pidStatUTime       :: Tick
    , pidStatSTime       :: Tick
    , pidStatCUTime      :: Tick
    , pidStatCSTime      :: Tick
    , pidStatPriority    :: Integer
    , pidStatNice        :: Integer
    , pidStatNumThreads  :: Integer
    , pidStatItRealValue :: Integer
    , pidStatStartTime   :: Tick
    , pidStatVSize       :: Byte
    , pidStatRSS         :: Page
    , pidStatRSSLim      :: Byte
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
pPIDStat = PIDStat . PID <$> A.decimal
    <*> (A.space *> pPIDStatComm)
    <*> (A.space *> pPIDStatState)
    <*> (PID <$> (A.space *> A.decimal))
    <*> (GID <$> (A.space *> A.signed A.decimal))
    <*> (SID <$> (A.space *> A.decimal))
    <*> (TTY <$> (A.space *> A.signed A.decimal))
    <*> (GID <$> (A.space *> A.signed A.decimal))
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

getPIDStatm :: Members '[Error String, IO] r => PID -> Eff r PIDStatm
getPIDStatm pid = readAndParse ("/proc/" ++ show pid ++ "/statm") pPIDStatm

getProcessMemUsage :: Members '[Error String, IO] r => PID -> Eff r KiloByte
getProcessMemUsage pid = getPIDStatm pid >>= send . calcPIDMemUsage

calcPIDMemUsage :: PIDStatm -> IO KiloByte
calcPIDMemUsage statm = bytesTokB <$> pagesToBytes (pidStatmRSS statm)

data PIDStatm = PIDStatm
    { pidStatmSize   :: Page
    , pidStatmRSS    :: Page
    , pidStatmShared :: Page
    , pidStatmText   :: Page
    , pidStatmLib    :: Page
    , pidStatmData   :: Page
    , pidStatmDT     :: Page
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
