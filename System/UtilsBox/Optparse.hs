{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBox.Optparse where

import Prelude hiding (getLine, putStrLn, putStr)

import qualified Control.Monad.Free as F
import qualified Options.Applicative as OA
import qualified System.Exit as SE
import           System.UtilsBoxSpec.CoreTypes ((:<:))
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, putStr)
import           System.UtilsBoxSpec.Environment
import           System.UtilsBoxSpec.Exit (ExitAPI, exitWith)

handleParserFree :: (TeletypeAPI :<: f) => String -> OA.ParserResult a -> F.Free f (Either (String, SE.ExitCode) a)
handleParserFree _ (OA.Success a) = return . Right $ a
handleParserFree programName (OA.Failure failure) = return (Left (msg, exit))
  where
    (msg, exit) = OA.renderFailure failure programName
handleParserFree _ (OA.CompletionInvoked _) = return (Left ("We currently do not support tab completions :(.", SE.ExitFailure 2))

execParserFree :: (TeletypeAPI :<: f, EnvironmentAPI :<: f) => String -> OA.ParserInfo a -> F.Free f (Either (String, SE.ExitCode) a)
execParserFree programName info = do
    programArgs <- getArgs
    let result = OA.execParserPure OA.defaultPrefs info programArgs
    handleParserFree programName result

execParserFreeExit :: (TeletypeAPI :<: f, EnvironmentAPI :<: f, ExitAPI :<: f) => String -> OA.ParserInfo a -> F.Free f a
execParserFreeExit programName info = execParserFree programName info >>= exitOnErr
  where
    exitOnErr (Left (errMsg, exitCode)) = do
        putStr errMsg
        exitWith exitCode
    exitOnErr (Right x) = return x
