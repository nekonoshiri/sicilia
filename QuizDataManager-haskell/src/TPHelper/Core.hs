module TPHelper.Core
  ( elements
  , getValue
  , onc
  , seton
  , setonc
  ) where

import Control.Monad.IO.Class (MonadIO)

import qualified Graphics.UI.Threepenny.Core as TP

elements :: (Functor f, TP.Widget w, MonadIO m) => f w -> f (m TP.Element)
elements = fmap TP.element

getValue :: TP.Element -> TP.UI String
getValue = TP.get TP.value

onc :: (TP.Element -> TP.Event a) -> TP.Element -> TP.UI () -> TP.UI ()
onc ev e = TP.on ev e . const

seton :: (TP.Element -> TP.Event a)
      -> (TP.Element -> a -> TP.UI ()) -> TP.UI TP.Element -> TP.UI TP.Element
seton ev f ue = do
  e <- ue
  TP.on ev e (f e)
  return e

setonc :: (TP.Element -> TP.Event a)
       -> (TP.Element -> TP.UI ()) -> TP.UI TP.Element -> TP.UI TP.Element
setonc ev f = seton ev (const . f)
