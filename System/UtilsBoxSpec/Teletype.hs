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

putStrNew :: (F.MonadFree f g, TeletypeAPI :<: f) => String -> g ()
putStrNew x = F.liftF . inject $ PutStr x ()

getCharNew :: (F.MonadFree f g, TeletypeAPI :<: f) => g Char
getCharNew = F.liftF . inject $ GetChar id

silence :: (F.MonadFree TeletypeAPI g) => TeletypeAPI a -> g a
silence (PutStr _ next) = return next
silence x @ (GetChar _) = F.liftF x

feed :: (F.MonadFree TeletypeAPI g) => Char -> TeletypeAPI a -> g a
feed _ x @ (PutStr _ _) = F.liftF x
feed char (GetChar f) = return (f char)

silenceAndFeed :: Char -> TeletypeAPI a -> a
silenceAndFeed _ (PutStr _ next) = next
silenceAndFeed char (GetChar f) = f char

example :: (F.MonadFree TeletypeAPI g) => g String
example = do
    x <- getCharNew
    putStrNew [x]
    y <- getCharNew
    putStrNew [y]
    return [x, y]

exampleExec = F.iterA (silenceAndFeed 'c') example
