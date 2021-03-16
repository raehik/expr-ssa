module ExprSSA.Util where

import qualified Data.Text      as Text
import           Data.Text (Text)
import           Data.Foldable (foldl')

-- | Prepend all elements in the second list to the first.
--
-- Note that this will reverse the order of the second list.
--
-- >>> prepend "c" "ba"
-- "abc"
prepend :: [a] -> [a] -> [a]
prepend = foldl' (flip (:))

tshow :: Show a => a -> Text
tshow = Text.pack . show
