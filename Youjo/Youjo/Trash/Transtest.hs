{-# LANGUAGE DeriveFunctor #-}

newtype MonadT2 n m a = MonadT2 (m (n a))
  deriving (Show, Functor)


