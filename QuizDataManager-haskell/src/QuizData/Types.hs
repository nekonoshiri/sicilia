module QuizData.Types
  (
  ) where

data Multi = Multi { question :: String
                   , answer :: [String]
                   , dummy :: [String]
                   , multitype :: Int
                   -- , metadata :: Metadata
                   }
                   deriving (Show, Eq)

-- construct
-- validate
-- destruct
