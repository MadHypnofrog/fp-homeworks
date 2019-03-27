{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MonadDecl (Monad (..)) where

import Prelude hiding (Monad (..))

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a