{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBox.Echo where

import Prelude hiding (getLine, putStrLn)
import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:), (:+:) (Inl, Inr))
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, putStrLn, teletypeIOF)
import           System.UtilsBoxSpec.Environment
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

handleParserTeletype :: (TeletypeAPI :<: f) => OA.ParserResult a -> F.Free f (Either String a)
handleParserTeletype (OA.Success a) = return . Right $ a
handleParserTeletype (OA.Failure failure) = do
    let programName = "ls"
    let (msg, exit) = OA.renderFailure failure programName
    return (Left msg)

execParserTeletype :: (TeletypeAPI :<: f, EnvironmentAPI :<: f) => OA.ParserInfo a -> F.Free f (Either String a)
execParserTeletype info = do
    programArgs <- getArgs
    let result = OA.execParserPure OA.defaultPrefs info programArgs
    handleParserTeletype result


echoF :: (EnvironmentAPI :<: f, TeletypeAPI :<: f) => F.Free f ()
echoF = do
    echoOpts <- execParserTeletype echoOptionsInfo
    case echoOpts of
         Left err -> putStrLn err
         Right opts -> echoFWithOpts opts

echoFWithOpts :: (EnvironmentAPI :<: f, TeletypeAPI :<: f) => EchoOptions -> F.Free f ()
echoFWithOpts opts = case echoArgument opts of
                          Nothing -> putStrLn ""
                          Just toEcho -> putStrLn toEcho

runIOF :: (TeletypeAPI :+: EnvironmentAPI) a -> IO a
runIOF (Inl teletype) = teletypeIOF teletype
runIOF (Inr environment) = environmentIOF environment

runIO :: F.Free (TeletypeAPI :+: EnvironmentAPI) a -> IO a
runIO = F.foldFree runIOF
