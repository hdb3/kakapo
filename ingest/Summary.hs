module Summary where

import Data.Text(Text)
import Control.Arrow(second)
import Data.Map.Strict(empty,alter,insertWith,Map(),toList)

summarise :: [(Text,Text)] -> [(Text,[(Text,Int)])]
summarise = map (second toList) . toList . summariseMap

summariseMap :: [(Text,Text)] -> Map Text (Map Text Int)
summariseMap = foldl outer empty

inner :: Text -> Map Text Int -> Map Text Int
inner v = insertWith (+) v 1

outer :: Map Text (Map Text Int) -> (Text,Text) -> Map Text (Map Text Int)
outer m ( k, v) = alter (f v) k m
    where f :: Text -> Maybe (Map Text Int) -> Maybe (Map Text Int)
          f v Nothing = Just $ inner v empty
          f v (Just m) = Just $ inner v m
