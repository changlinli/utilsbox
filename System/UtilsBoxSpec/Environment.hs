{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBoxSpec.Environment where

import qualified Control.Monad.Free as F
import           System.UtilsBoxSpec.CoreTypes ((:<:), inject)
import qualified Data.Map as M
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
