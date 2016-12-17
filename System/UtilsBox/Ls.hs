{-# LANGUAGE DeriveFunctor #-}
module System.UtilsBox.Ls where

import qualified Control.Monad.Free as F
import qualified System.Directory as SD
import qualified System.IO.Error as SIE
import           Test.IOSpec (Executable)
import           Data.Functor.Sum
import qualified Options.Applicative as OA
import           Data.Monoid hiding (Sum)

data FileSystemAPI next
    = GetDirectoryContents String (Either DirError [String] -> next) deriving Functor

data DirError
    = HardwareFault String
    | InvalidArgument String
    | DoesNotExist String

type FileSystemAPIM next = F.Free FileSystemAPI next

getDirectoryContents :: String -> FileSystemAPIM (Either DirError [String])
getDirectoryContents path = F.liftF $ GetDirectoryContents path id

data TeletypeAPI next
    = Print String next deriving Functor

type TeletypeAPIM next = F.Free TeletypeAPI next

print :: String -> TeletypeAPIM ()
print x = F.liftF $ Print x ()

fileSysIOF :: FileSystemAPI (IO a) -> IO a
fileSysIOF (GetDirectoryContents path io) = do
    filesOrErr <- SIE.tryIOError $ SD.getDirectoryContents path
    let result = case filesOrErr of
         Right files -> Right files
         Left error -> Left . DoesNotExist $ path
    io result

teletypeIOF :: TeletypeAPI (IO a) -> IO a
teletypeIOF (Print x io) = Prelude.print x >> io

runIOF :: Sum TeletypeAPI FileSystemAPI (IO a) -> IO a
runIOF (InL teletype) = teletypeIOF teletype
runIOF (InR filesys) = fileSysIOF filesys

runIO :: F.Free (Sum TeletypeAPI FileSystemAPI) a -> IO a
runIO = F.iterM runIOF

-- Or alternatively

fileSysIOFA :: FileSystemAPI a -> IO a
fileSysIOFA (GetDirectoryContents path next) = do
    filesOrErr <- SIE.tryIOError $ SD.getDirectoryContents path
    let result = case filesOrErr of
         Right files -> Right files
         Left error -> Left . DoesNotExist $ path
    return $ next result

teletypeIOFA :: TeletypeAPI a -> IO a
teletypeIOFA (Print x next) = Prelude.print x >> return next

runIOFA :: (Sum TeletypeAPI FileSystemAPI) a -> IO a
runIOFA (InL teletype) = teletypeIOFA teletype
runIOFA (InR filesys) = fileSysIOFA filesys

runIOA :: F.Free (Sum TeletypeAPI FileSystemAPI) a -> IO a
runIOA = F.foldFree runIOFA

-- And finally...

ls :: F.Free (Sum TeletypeAPI FileSystemAPI) ()
ls = do
    allElemsOrErr <- F.liftF $ InR (GetDirectoryContents "hello" id)
    case allElemsOrErr of 
         Right allElems -> F.liftF . InL $ Print (unlines allElems) ()
         Left (DoesNotExist path) -> F.liftF . InL $ Print (path ++ " does not exist!") ()

-- And we can run this with runIOA ls
