module Youjo.List
( groupN
) where

import Data.Maybe

groupN :: Int -> [a] -> [[a]]
groupN n ts = catMaybes $ snd $ mapAccumL g (1, length ts, empty) ts
  where g (ct, len, buf) x
          | ct >= n || len <= 1 = ((1, len - 1, []), Just (buf ++ [x]))
          | otherwise = ((ct + 1, len - 1, buf ++ [x]), Nothing)

-- breakrun (find run and break)
