module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)

import qualified Graphics.UI.Threepenny.Core as TP
import qualified Graphics.UI.Threepenny.Elements as E
import qualified Graphics.UI.Threepenny.Events as EV

import Graphics.UI.Threepenny.Core (Window, Element, UI, (#), (#+), set)

import qualified TPHelper.Core as TPH
import QuizData.MetaData (readMetaData)
import QuizData.Register (register)
import Util.Either (toExceptT)

main :: IO ()
main = TP.startGUI TP.defaultConfig { TP.jsLog = \_ -> return () } setup

setup :: Window -> UI ()
setup window = do
  setupWindow window
  setupBody =<< TP.getBody window
  return ()

setupWindow :: Window -> UI ()
setupWindow window = do
  pure window # set TP.title "QuizDataManager"
  return ()

setupBody :: Element -> UI ()
setupBody body = do
  genre     <- genreSelect
  subGenre  <- subGenreSelect
  examGenre <- examGenreSelect
  series    <- seriesSelect
  question  <- E.textarea
  answer    <- E.textarea
  dummy     <- E.textarea
  pictureID <- E.textarea
  comment   <- E.textarea
  notice    <- E.span
  (difMin, difMax) <- difficultySelects
  let registerEvent _ = do
        gen     <- TPH.getValue genre
        subGen  <- TPH.getValue subGenre
        examGen <- TPH.getValue examGenre
        sers    <- TPH.getValue series
        que     <- TPH.getValue question
        ans     <- TPH.getValue answer
        dum     <- TPH.getValue dummy
        difmin  <- TPH.getValue difMin
        difmax  <- TPH.getValue difMax
        pictID  <- TPH.getValue pictureID
        cmt     <- TPH.getValue comment
        result <- liftIO . runExceptT $ do
          metadata <- toExceptT $
            readMetaData gen subGen examGen sers difmin difmax pictID cmt
          -- multidata <- toExceptT $ readMultiData a b c
          register metadata -- multidata
        TP.element notice # set TP.text (either id (const "OK!") result)
        return ()

      registerButton = E.button
        # set TP.text "登録"
        # TPH.setonc EV.click registerEvent

      selects    = TPH.elements [genre, subGenre, examGenre, series]
      questions  = TPH.elements [question]
      answers    = TPH.elements [answer, dummy]
      difs       = TPH.elements [difMin, difMax]
      pictureIDs = TPH.elements [pictureID]
      comments   = TPH.elements [comment]
      buttons    = [registerButton]
      notices    = TPH.elements [notice]
      paras = map (E.p #+) [ selects, questions, answers, difs
                           , pictureIDs, comments, buttons, notices]
  pure body #+ paras
  return ()

-- selects

option :: (String, String) -> UI Element
option (value, text) = E.option # set TP.value value # set TP.text text

options :: [(String, String)] -> [UI Element]
options = fmap option

genreSelect :: UI Element
genreSelect = E.select #+ options
  [ ("0", "unknown")
  , ("1", "non")
  , ("2", "ag")
  , ("3", "spo")
  , ("4", "gei")
  , ("5", "lif")
  , ("6", "sya")
  , ("7", "bun")
  , ("8", "ri")
  , ("9", "ken")
  ]

subGenreSelect :: UI Element
subGenreSelect = E.select #+ options
  [ ("0", "unknown")
  , ("1", "non")
  , ("2", "antk")
  , ("3", "mano")
  , ("4", "gech")
  , ("5", "unk")
  , ("6", "bas")
  , ("7", "soc")
  , ("8", "oth")
  ]

examGenreSelect :: UI Element
examGenreSelect = E.select #+ options
  [ ("0", "unk")
  , ("1", "mishu")
  ]

seriesSelect :: UI Element
seriesSelect = E.select #+ options
  [ ("0", "unk")
  , ("1", "1")
  ]

difficultySelects :: UI (Element, Element)
difficultySelects = (,) <$> minSelect <*> maxSelect
  where opts = map (\x -> (show x, "☆" ++ show x)) ([1..5] :: [Int])
        minSelect = E.select #+ options opts
        maxSelect = E.select #+ options opts
