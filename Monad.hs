module Monad where

data MyMaybe a = MyJust a | MyNone deriving (Show, Eq)

div :: (Eq a, Fractional a) => a -> a -> MyMaybe a
div _ 0 = MyNone
div x y = MyJust $ x / y

times3 :: Num a => a -> a
times3 = (*) 3

class Functor f where
    map :: (a -> b) -> f a -> f b

instance Monad.Functor MyMaybe where
    map f (MyJust a) = MyJust $ f a
    map _ MyNone = MyNone
    
combine :: (Eq a, Fractional a) => a -> a -> MyMaybe a
combine x y = Monad.map times3 $ Monad.div x y

class Monad f where
    return :: a -> f a
    (>>=) :: (a -> f b) -> f a -> f b

instance Monad.Monad MyMaybe where
    return = MyJust
    (>>=) f (MyJust a) = f a
    (>>=) f MyNone = MyNone

    