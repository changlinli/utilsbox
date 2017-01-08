{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:))
import           System.UtilsBoxSpec.Teletype (TeletypeAPI)
import           System.UtilsBoxSpec.Environment
import           System.UtilsBoxSpec.Exit (ExitAPI)
import           System.UtilsBoxSpec.FileSystem (FileSystemAPI)
import           System.UtilsBoxSpec.Interpreter (runIO)
import           System.UtilsBox.Ls
import           System.UtilsBox.Echo
import           System.UtilsBox.Optparse (execParserFreeExit)
import qualified Options.Applicative as OA
import           Data.Monoid

data MainOptions
    = Ls LsOptions
    | Echo EchoOptions

mainOptions :: OA.Parser MainOptions
mainOptions = OA.subparser (mconcat subcommands)
  where
    subcommands =
        [ OA.command "ls" (fmap Ls lsOptionsInfo)
        , OA.command "echo" (fmap Echo echoOptionsInfo)
        ]

mainOptionsInfo :: OA.ParserInfo MainOptions
mainOptionsInfo = OA.info (OA.helper <*> mainOptions) (OA.fullDesc <> OA.progDesc "A collection of many tools inspired by BusyBox" <> OA.header "A collection of many tools")

mainUtilsBoxF :: (TeletypeAPI :<: f, EnvironmentAPI :<: f, FileSystemAPI :<: f, ExitAPI :<: f) => F.Free f ()
mainUtilsBoxF = do
    opts <- execParserFreeExit "utilsbox" mainOptionsInfo
    case opts of
         Ls lsOpts -> lsFWithOpts lsOpts
         Echo echoOpts -> echoFWithOpts echoOpts

mainF :: (TeletypeAPI :<: f, EnvironmentAPI :<: f, FileSystemAPI :<: f, ExitAPI :<: f) => F.Free f ()
mainF = do
    progName <- getProgName
    case progName of
         "ls" -> lsF
         "echo" -> echoF
         _ -> mainUtilsBoxF

main :: IO ()
main = runIO mainF
