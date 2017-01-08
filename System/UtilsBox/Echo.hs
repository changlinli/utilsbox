{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBox.Echo where

import Prelude hiding (getLine, putStrLn)
import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:))
import           System.UtilsBoxSpec.Exit (ExitAPI)
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, putStrLn)
import           System.UtilsBoxSpec.Environment
import           System.UtilsBox.Optparse (execParserFreeExit)
import qualified Options.Applicative as OA
import           Data.Monoid

data EchoOptions = EchoOptions
    { echoArgument :: Maybe String
    }

echoOptions :: OA.Parser EchoOptions
echoOptions = EchoOptions
    <$> (OA.optional $ OA.argument OA.str (OA.metavar "STRING"))

echoOptionsInfo :: OA.ParserInfo EchoOptions
echoOptionsInfo = OA.info (OA.helper <*> echoOptions) (OA.fullDesc <> OA.progDesc "Prints STRINGs to standard output" <> OA.header "Echo stuff to stdout")

echoF :: (EnvironmentAPI :<: f, TeletypeAPI :<: f, ExitAPI :<: f) => F.Free f ()
echoF = do
    echoOpts <- execParserFreeExit "echo" echoOptionsInfo
    echoFWithOpts echoOpts

echoFWithOpts :: (TeletypeAPI :<: f) => EchoOptions -> F.Free f ()
echoFWithOpts opts = case echoArgument opts of
                          Nothing -> putStrLn ""
                          Just toEcho -> putStrLn toEcho
