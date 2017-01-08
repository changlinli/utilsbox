{-# LANGUAGE TypeOperators #-}

module System.UtilsBoxSpec.Interpreter where

import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:+:) (Inl, Inr))
import           System.UtilsBoxSpec.Environment (EnvironmentAPI, environmentIOF)
import           System.UtilsBoxSpec.Exit (ExitAPI, exitIOF)
import           System.UtilsBoxSpec.FileSystem (FileSystemAPI, fileSystemIOF)
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, teletypeIOF)

runIOF :: (TeletypeAPI :+: FileSystemAPI :+: EnvironmentAPI :+: ExitAPI) a -> IO a
runIOF (Inl teletype) = teletypeIOF teletype
runIOF (Inr (Inl filesys)) = fileSystemIOF filesys
runIOF (Inr (Inr (Inl environment))) = environmentIOF environment
runIOF (Inr (Inr (Inr exit))) = exitIOF exit

runIO :: F.Free (TeletypeAPI :+: FileSystemAPI :+: EnvironmentAPI :+: ExitAPI) a -> IO a
runIO = F.foldFree runIOF
