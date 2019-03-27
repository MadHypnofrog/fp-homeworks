{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monad () where

import Prelude hiding (Monad (..))
import Lib ( MonadFish (..)
           , MonadJoin (..)
           )
import MonadDecl (Monad (..))

instance Monad m => MonadFish m where
  returnFish = return
  (>=>) f1 f2 a = (return a) >>= f1 >>= f2

instance Monad m => MonadJoin m where
  returnJoin = return
  join x = x >>= id
