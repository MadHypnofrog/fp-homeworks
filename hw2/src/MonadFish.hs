{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MonadFish () where

import Prelude hiding (Monad (..))
import Lib ( MonadFish (..)
           , MonadJoin (..)
           )
import MonadDecl (Monad (..))

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join = id >=> id

instance MonadFish m => Monad m where
  return = returnFish
  (>>=) x f = (id >=> f) x 