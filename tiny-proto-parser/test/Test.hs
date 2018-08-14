import System.Exit (die)

import Text.Parsec (parse)

import Text.TinyProto.Parser (proto)
import Text.TinyProto.Language (prottyShow)

main :: IO ()
main = do
  putStrLn ""
  test "test/test.proto" True

test :: FilePath -> Bool -> IO ()
test path isFormal = do
  cont <- readFile path
  case parse proto "" cont of
    Left err -> assert (not isFormal) (show err)
    Right xs -> assert isFormal ("\n" ++ prottyShow 2 "\n" xs)

assert :: Bool -> String -> IO ()
assert True s  = putStr "OK:" >> putStrLn s
assert False s = die s
