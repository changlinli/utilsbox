{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBoxSpec.Exit where

import qualified Control.Monad.Free as F
import qualified System.Exit as SE
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject)

data ExitAPI a = ExitProgram (SE.ExitCode) deriving Functor

exitWith :: (ExitAPI :<: f) => SE.ExitCode ->  F.Free f a
exitWith code = F.liftF . inject $ ExitProgram code

exitFailure :: (ExitAPI :<: f) => F.Free f a
exitFailure = F.liftF . inject $ ExitProgram (SE.ExitFailure 1)

exitSuccess :: (ExitAPI :<: f) => F.Free f a
exitSuccess = F.liftF . inject $ ExitProgram (SE.ExitSuccess)

exitIOF :: ExitAPI a -> IO a
exitIOF (ExitProgram exitCode) = SE.exitWith exitCode
