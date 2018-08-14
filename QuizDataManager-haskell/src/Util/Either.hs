module Util.Either
  ( readEitherWith
  , validate
  , toExceptT
  ) where

import Text.Read (readMaybe)

import Control.Monad.Trans.Except (ExceptT(ExceptT))

readEitherWith :: Read a => b -> String -> Either b a
readEitherWith err val = maybe (Left err) Right (readMaybe val)

validate :: a -> Bool -> Either a ()
validate err cond = if cond then Right () else Left err

toExceptT :: Monad m => Either e a -> ExceptT e m a
toExceptT = ExceptT . return
