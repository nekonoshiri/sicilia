module QuizData.Register
  ( register
  ) where

import Control.Monad.Trans.Except (ExceptT)

import QuizData.MetaData (MetaData)

register :: MetaData -> ExceptT String IO ()
register metadata = return ()
