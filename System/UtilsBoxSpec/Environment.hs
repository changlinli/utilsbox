{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBoxSpec.Environment where

import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject)
import           Data.Functor.Identity (Identity (..))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified System.Environment as SE

data EnvironmentAPI next 
    = GetEnvironment ((M.Map String String) -> next)
    | GetArgs ([String] -> next)
    | GetProgName (String -> next)
    deriving Functor

getArgs :: (EnvironmentAPI :<: f) => F.Free f [String]
getArgs = F.liftF . inject $ GetArgs id

getProgName :: (EnvironmentAPI :<: f) => F.Free f String
getProgName = F.liftF . inject $ GetProgName id

environmentIOF :: EnvironmentAPI a -> IO a
environmentIOF (GetArgs next) = SE.getArgs >>= (fmap return next)
environmentIOF (GetProgName next) = SE.getProgName >>= (fmap return next)
environmentIOF (GetEnvironment next) = fmap M.fromList SE.getEnvironment >>= (fmap return next)

environmentPure :: M.Map String String -> EnvironmentAPI a -> Identity a
environmentPure keyValues x = case x of
                                   (GetArgs next) -> Identity arguments >>= (fmap return next)
                                   (GetProgName next) -> Identity programName >>= (fmap return next)
                                   (GetEnvironment next) -> Identity keyValues >>= (fmap return next)
  where
    arguments = (fromMaybe []) . (fmap words) $ M.lookup "arguments" keyValues
    programName = fromMaybe "" $ M.lookup "programName" keyValues
