module CompareText where
import qualified Text.Read
import Data.Text(Text())
import qualified Data.Text as Text

compareText :: Text -> Text -> Ordering
-- use to sort lists which _may_ be numeric
-- order numeric before non-numeric
-- sort numeric (low -> high)
-- sort non-numeric lexicographically

compareText txtA txtB = let
    readMaybeInt = Text.Read.readMaybe :: String -> Maybe Int
    stringA = Text.unpack txtA
    stringB = Text.unpack txtB
    in case ( readMaybeInt stringA , readMaybeInt stringB ) of
    ( Nothing,Nothing) -> compare stringA stringB
    ( Just intA , Just intB ) -> compare intA intB
    ( Just _ , _ ) -> GT
    ( _ , _ ) -> LT
