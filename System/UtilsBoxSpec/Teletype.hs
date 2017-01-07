{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBoxSpec.Teletype where

import Prelude hiding (getLine, putStrLn, putStr)
import qualified Prelude as P

import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject)

data TeletypeAPI next
    = PutStr String next 
    | GetChar (Char -> next) deriving Functor

putStr :: (TeletypeAPI :<: f) => String -> F.Free f ()
putStr x = F.liftF . inject $ PutStr x ()

putStrLn :: (TeletypeAPI :<: f) => String -> F.Free f ()
putStrLn x = putStr (x ++ "\n")

teletypeIOF :: TeletypeAPI a -> IO a
teletypeIOF (PutStr x next) = P.putStr x >> return next
teletypeIOF (GetChar result) = getChar >>= (fmap return result)

getLine :: (TeletypeAPI :<: f) => F.Free f String
getLine = do
    c <- F.liftF . inject $ (GetChar id)
    if c == '\n'
       then return ""
       else getLine >>= \line -> return (c : line)
