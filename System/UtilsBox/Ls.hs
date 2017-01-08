{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBox.Ls where

import Prelude hiding (getLine, putStrLn)
import qualified Prelude as P

import qualified Control.Monad.Free as F
import qualified System.Directory as SD
import qualified System.IO.Error as SIE
import qualified Options.Applicative as OA
import           Data.Monoid hiding (Sum)
import           Data.Foldable (find)
import           System.UtilsBox.Optparse (execParserFreeExit)
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject, (:+:) (Inl, Inr))
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, putStrLn, getLine, teletypeIOF)
import           System.UtilsBoxSpec.Environment
import           System.UtilsBoxSpec.Exit (ExitAPI, exitFailure)
import           System.UtilsBoxSpec.FileSystem (FileSystemAPI, getDirectoryContents, getCurrentDirectory, DirError (..))
import           System.UtilsBoxSpec.Interpreter (runIOF, runIO)

data LsOptions = LsOptions
    { verboseFlag :: Bool
    , fileArgument :: Maybe String
    }

lsOptions :: OA.Parser LsOptions
lsOptions = LsOptions 
    <$> OA.switch ( OA.long "verbose" <> OA.short 'v' <> OA.help "Turn on verbose output." )
    <*> (OA.optional $ OA.argument OA.str (OA.metavar "FILE"))

lsOptionsInfo :: OA.ParserInfo LsOptions
lsOptionsInfo = OA.info (OA.helper <*> lsOptions) (OA.fullDesc <> OA.progDesc "List all files in a directory" <> OA.header "ls: A utility for listing files" )

getProgramArgs :: (TeletypeAPI :<: f) => F.Free f [String]
getProgramArgs = fmap return getLine

lsF :: (TeletypeAPI :<: f, FileSystemAPI :<: f, EnvironmentAPI :<: f, ExitAPI :<: f) => F.Free f ()
lsF = do
    lsOpts <- execParserFreeExit "ls" lsOptionsInfo
    lsFWithOpts lsOpts

lsFWithOpts :: (TeletypeAPI :<: f, FileSystemAPI :<: f, EnvironmentAPI :<: f, ExitAPI :<: f) => LsOptions -> F.Free f ()
lsFWithOpts opts = do
    _ <- if (verboseFlag opts) then putStrLn "verbose!" else return ()
    path <- case fileArgument opts of
                 Nothing -> getCurrentDirectory
                 Just pathArg -> return pathArg
    allElemsOrErr <- getDirectoryContents path
    case allElemsOrErr of
         Right allElems -> putStrLn (unlines allElems)
         Left (DoesNotExist nonexistentPath) -> putStrLn (nonexistentPath ++ " does not exist!") *> exitFailure

lsIO :: IO ()
lsIO = runIO lsF
