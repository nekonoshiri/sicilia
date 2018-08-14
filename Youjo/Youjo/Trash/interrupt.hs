
import Data.Traversable

interrupt :: (Applicative f1, Applicative f2, Traversable f1, Traversable t)
          => (t a -> f2 b) -> t (f1 a) -> f2 (f1 b)
interrupt f = sequenceA . fmap f . sequenceA
