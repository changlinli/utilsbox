{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBoxSpec.FileSystem where

import qualified Control.Monad.Free as F
import qualified System.Directory as SD
import qualified System.IO.Error as SIE
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject)

data DirError
    = HardwareFault String
    | InvalidArgument String
    | DoesNotExist String

data FileSystemAPI next
    = GetDirectoryContents String (Either DirError [String] -> next)
    | ReadFile String (String -> next)
    | WriteFile String String next
    | RenamePath String String (Either DirError () -> next)
    | GetCurrentDirectory (String -> next) 
    deriving Functor

getDirectoryContents :: (FileSystemAPI :<: f) => String -> F.Free f (Either DirError [String])
getDirectoryContents path = F.liftF . inject $ GetDirectoryContents path id

getCurrentDirectory :: (FileSystemAPI :<: f) => F.Free f String
getCurrentDirectory = F.liftF . inject $ GetCurrentDirectory id

renamePath :: (FileSystemAPI :<: f) => String -> String -> F.Free f (Either DirError ())
renamePath original newname = F.liftF . inject $ RenamePath original newname id

fileSystemIOF :: FileSystemAPI a -> IO a
fileSystemIOF (GetDirectoryContents path next) = do
    filesOrErr <- SIE.tryIOError $ SD.getDirectoryContents path
    let result = case filesOrErr of
         Right files -> Right files
         Left error -> Left . DoesNotExist $ path
    return $ next result
fileSystemIOF (GetCurrentDirectory next) = SD.getCurrentDirectory >>= (fmap return next)
