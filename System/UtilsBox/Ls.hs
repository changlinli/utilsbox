{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module System.UtilsBox.Ls where

import Prelude hiding (getLine, putStrLn)
import qualified Prelude as P

import qualified Control.Monad.Free as F
import           Control.Monad.State
import qualified System.Directory as SD
import qualified System.IO.Error as SIE
import qualified Options.Applicative as OA
import           Data.Monoid hiding (Sum)
import           Data.Foldable (find)
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject, (:+:) (Inl, Inr))
import           System.UtilsBoxSpec.Teletype (TeletypeAPI, putStrLn, getLine, teletypeIOF)
import           System.UtilsBoxSpec.Environment


data FileSystemAPI next
    = GetDirectoryContents String (Either DirError [String] -> next)
    | ReadFile String (String -> next)
    | WriteFile String String next
    | Pwd (String -> next) 
    deriving Functor

data DirError
    = HardwareFault String
    | InvalidArgument String
    | DoesNotExist String

type FileSystemAPIM next = F.Free FileSystemAPI next

getDirectoryContents :: String -> FileSystemAPIM (Either DirError [String])
getDirectoryContents path = F.liftF $ GetDirectoryContents path id

fileSysIOF :: FileSystemAPI a -> IO a
fileSysIOF (GetDirectoryContents path next) = do
    filesOrErr <- SIE.tryIOError $ SD.getDirectoryContents path
    let result = case filesOrErr of
         Right files -> Right files
         Left error -> Left . DoesNotExist $ path
    return $ next result
fileSysIOF (Pwd next) = SD.getCurrentDirectory >>= (fmap return next)

runIOF :: (TeletypeAPI :+: FileSystemAPI :+: EnvironmentAPI) a -> IO a
runIOF (Inl teletype) = teletypeIOF teletype
runIOF (Inr (Inl filesys)) = fileSysIOF filesys
runIOF (Inr (Inr environment)) = environmentIOF environment

runIO :: F.Free (TeletypeAPI :+: FileSystemAPI :+: EnvironmentAPI) a -> IO a
runIO = F.foldFree runIOF


pwd :: (FileSystemAPI :<: f) => F.Free f String
pwd = F.liftF . inject $ Pwd id

data Tree a = Node a [Tree a]

label :: Tree a -> a
label (Node x _) = x

findChild :: Eq a => Tree a -> a -> Maybe (Tree a)
findChild (Node _ children) label = find (\(Node x _) -> label == x) children

fileSysList :: FileSystemAPI a -> State (Tree String) a
fileSysList (GetDirectoryContents path next) = undefined

-- And finally...

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

getProgramArgs :: TeletypeAPI :<: f => F.Free f [String]
getProgramArgs = fmap return getLine

handleParserTeletype :: (TeletypeAPI :<: f) => String -> OA.ParserResult a -> F.Free f (Either String a)
handleParserTeletype _ (OA.Success a) = return . Right $ a
handleParserTeletype programName (OA.Failure failure) = do
    let (msg, exit) = OA.renderFailure failure programName
    return (Left msg)

execParserTeletype :: (TeletypeAPI :<: f, EnvironmentAPI :<: f) => String -> OA.ParserInfo a -> F.Free f (Either String a)
execParserTeletype programName info = do
    programArgs <- getArgs
    let result = OA.execParserPure OA.defaultPrefs info programArgs
    handleParserTeletype programName result

lsF :: (TeletypeAPI :<: f, FileSystemAPI :<: f, EnvironmentAPI :<: f) => F.Free f ()
lsF = do
    lsOpts <- execParserTeletype "ls" lsOptionsInfo
    case lsOpts of
         Left error -> putStrLn error
         Right opts -> lsFWithOpts opts

lsFWithOpts :: (TeletypeAPI :<: f, FileSystemAPI :<: f, EnvironmentAPI :<: f) => LsOptions -> F.Free f ()
lsFWithOpts opts = do
    _ <- if (verboseFlag opts) then putStrLn "verbose!" else return ()
    path <- case fileArgument opts of
                 Nothing -> pwd
                 Just pathArg -> return pathArg
    allElemsOrErr <- F.hoistFree inject (getDirectoryContents path)
    case allElemsOrErr of 
         Right allElems -> putStrLn (unlines allElems)
         Left (DoesNotExist path) -> putStrLn (path ++ " does not exist!")

-- And we can run this with runIOA ls

lsIO :: IO ()
lsIO = runIO lsF
