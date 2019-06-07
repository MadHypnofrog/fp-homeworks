{-#LANGUAGE RankNTypes#-}
module Lib
    ( set
    , view
    , over
    , _1
    , _2
    , lens
    , choosing
    , (<%~)
    , (<<%~)
    ) where

import Control.Applicative (getConst, Const (..))
import Data.Functor.Identity (Identity (..))

--Task 5
type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a

set :: Lens' s a -> a -> s -> s
set l val = over l (\_ -> val)

view :: Lens' s a -> s -> a
view l = getConst . l Const

over :: Lens' s a -> (a -> a) -> s -> s
over l fun = runIdentity . l (Identity . fun)

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = flip (,) x <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (,) x <$> f a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter fun cont = setter cont <$> fun (getter cont)

choosing :: Lens s1 t1 a b 
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _ fun (Left s1) = Left <$> l1 fun s1
choosing _ l2 fun (Right s2) = Right <$> l2 fun s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f $ getConst $ l Const s, runIdentity $ l (Identity . f) s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (getConst $ l Const s, runIdentity $ l (Identity . f) s)
