{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module System.UtilsBoxSpec.CoreTypes where

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

