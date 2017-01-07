{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module System.UtilsBox.Ls where

import Prelude hiding (getLine, print)
import qualified Prelude as P

import qualified Control.Monad.Free as F
import           Control.Monad.State
import qualified System.Directory as SD
import qualified System.IO.Error as SIE
import qualified System.Environment as SE
import qualified Options.Applicative as OA
import           Data.Monoid hiding (Sum)
import           Data.Foldable (find)
import qualified Data.Map as M

data EnvironmentAPI next 
    = GetEnvVariables ((M.Map String String) -> next)
    | GetArgs ([String] -> next)
    | Pwd (String -> next) deriving Functor

data FileSystemAPI next
    = GetDirectoryContents String (Either DirError [String] -> next)
    | ReadFile String (String -> next)
    | WriteFile String String next deriving Functor

data DirError
    = HardwareFault String
    | InvalidArgument String
    | DoesNotExist String

type FileSystemAPIM next = F.Free FileSystemAPI next

getDirectoryContents :: String -> FileSystemAPIM (Either DirError [String])
getDirectoryContents path = F.liftF $ GetDirectoryContents path id

data TeletypeAPI next
    = Print String next 
    | GetChar (Char -> next) deriving Functor

type TeletypeAPIM next = F.Free TeletypeAPI next

print :: String -> TeletypeAPIM ()
print x = F.liftF $ Print x ()

fileSysIOF :: FileSystemAPI a -> IO a
fileSysIOF (GetDirectoryContents path next) = do
    filesOrErr <- SIE.tryIOError $ SD.getDirectoryContents path
    let result = case filesOrErr of
         Right files -> Right files
         Left error -> Left . DoesNotExist $ path
    return $ next result

teletypeIOF :: TeletypeAPI a -> IO a
teletypeIOF (Print x next) = P.putStrLn x >> return next
teletypeIOF (GetChar result) = getChar >>= (fmap return result)

environmentIOF :: EnvironmentAPI a -> IO a
environmentIOF (GetArgs next) = SE.getArgs >>= (fmap return next)

runIOF :: (TeletypeAPI :+: FileSystemAPI :+: EnvironmentAPI) a -> IO a
runIOF (Inl teletype) = teletypeIOF teletype
runIOF (Inr (Inl filesys)) = fileSysIOF filesys
runIOF (Inr (Inr environment)) = environmentIOF environment

runIO :: F.Free (TeletypeAPI :+: FileSystemAPI :+: EnvironmentAPI) a -> IO a
runIO = F.foldFree runIOF

getLine :: (TeletypeAPI :<: f) => F.Free f String
getLine = do
    c <- F.liftF . inject $ (GetChar id)
    if c == '\n'
       then return ""
       else getLine >>= \line -> return (c : line)

getArgs :: (EnvironmentAPI :<: f) => F.Free f [String]
getArgs = F.liftF . inject $ GetArgs id

teletypeString :: String -> TeletypeAPI a -> ([String], a)
teletypeString [] (GetChar result) = ([],  '\0') >>= (fmap return result)
teletypeString (x : xs) (GetChar result) = ([], x) >>= (fmap return result)
teletypeString _ (Print text next) = ([text], next)

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
    , fileArgument :: String
    }

data (f :+: g) e = Inl (f e) | Inr (g e)

infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl x) = Inl $ fmap f x
    fmap f (Inr x) = Inr $ fmap f x

class (Functor f, Functor g) => (:<:) f g where
    inject :: f a -> g a

instance Functor f => f :<: f where
    inject = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inject = Inl

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inject = Inr . inject

lsOptions :: OA.Parser LsOptions
lsOptions = LsOptions 
    <$> OA.switch ( OA.long "verbose" <> OA.short 'v' <> OA.help "Turn on verbose output." )
    <*> OA.argument OA.str (OA.metavar "FILE")

lsOptionsInfo :: OA.ParserInfo LsOptions
lsOptionsInfo = OA.info (OA.helper <*> lsOptions) (OA.fullDesc <> OA.progDesc "List all files in a directory" <> OA.header "Thingies!" )

getProgramArgs :: TeletypeAPI :<: f => F.Free f [String]
getProgramArgs = fmap return getLine

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

lsAlt :: (TeletypeAPI :<: f, FileSystemAPI :<: f, EnvironmentAPI :<: f) => F.Free f ()
lsAlt = do
    lsOpts <- execParserTeletype lsOptionsInfo
    case lsOpts of
         Left error -> F.hoistFree inject $ print error
         Right opts -> lsWithOptsAlt opts

lsWithOptsAlt :: (TeletypeAPI :<: f, FileSystemAPI :<: f, EnvironmentAPI :<: f) => LsOptions -> F.Free f ()
lsWithOptsAlt opts = do
    _ <- if (verboseFlag opts) then F.hoistFree inject $ print "verbose!" else return()
    let path = fileArgument opts
    allElemsOrErr <- F.hoistFree inject (getDirectoryContents path)
    case allElemsOrErr of 
         Right allElems -> F.liftF . inject $ Print (unlines allElems) ()
         Left (DoesNotExist path) -> F.liftF . inject $ Print (path ++ " does not exist!") ()

-- And we can run this with runIOA ls

lsIO :: IO ()
lsIO = runIO lsAlt
