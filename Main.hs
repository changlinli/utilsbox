{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (getLine, putStrLn)
import qualified Prelude as P

import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject, (:+:) (Inl, Inr))
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, putStrLn, teletypeIOF)
import           System.UtilsBoxSpec.Environment
import           System.UtilsBox.Ls
import           System.UtilsBox.Echo
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

mainUtilsBoxF :: (TeletypeAPI :<: f, EnvironmentAPI :<: f, FileSystemAPI :<: f) => F.Free f ()
mainUtilsBoxF = do
    optsOrErr <- System.UtilsBox.Ls.execParserTeletype "utilsbox" mainOptionsInfo
    case optsOrErr of
         Left err -> putStrLn err
         Right opts -> case opts of
                            Ls lsOpts -> lsFWithOpts lsOpts
                            Echo echoOpts -> echoFWithOpts echoOpts

mainF :: (TeletypeAPI :<: f, EnvironmentAPI :<: f, FileSystemAPI :<: f) => F.Free f ()
mainF = do
    progName <- getProgName
    case progName of
         "ls" -> lsF
         "echo" -> echoF
         _ -> mainUtilsBoxF

main :: IO ()
main = System.UtilsBox.Ls.runIO mainF
