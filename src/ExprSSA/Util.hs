module ExprSSA.Util where

import qualified Data.Text      as Text
import           Data.Text (Text)

-- | Prepend all elements in the second list to the first.
--
-- Note that this will reverse the order of the second list.
prepend :: [a] -> [a] -> [a]
prepend xs []     = xs
prepend xs (y:ys) = prepend (y:xs) ys

tshow :: Show a => a -> Text
tshow = Text.pack . show
