{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module QuizData.Persist
  ( Genre
  ) where

import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate)

share [mkPersist sqlSettings, mkMigrate "migrateAll" ] [persistLowerCase|
Genre
  genre String
  deriving Show
|]
