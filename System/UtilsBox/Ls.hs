{-# LANGUAGE DeriveFunctor #-}
module System.UtilsBox.Ls where

import qualified Control.Monad.Free as F
import qualified System.Directory as SD

data FileSystemAPI next
    = GetDirectoryContents String ([String] -> next) deriving Functor

type FileSystemAPIM next = F.Free FileSystemAPI next

getDirectoryContents :: String -> FileSystemAPIM [String]
getDirectoryContents path = F.liftF $ GetDirectoryContents path id

ls :: FileSystemAPIM [String]
ls = do
    allElems <- getDirectoryContents "hello"
    return allElems

runIO :: FileSystemAPIM a -> IO a
runIO (F.Pure x) = return x
runIO (F.Free (GetDirectoryContents path next)) = SD.getDirectoryContents path >>= runIO . next
