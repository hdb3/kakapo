module KakapoData where
import Data.Text(Text)

data KakapoStart = KakapoStart
    { pid :: Int
    , topic :: Text
    , platform :: Text
    , sut :: Text
    , time :: Int
    , uuid :: Text
    , version :: Text
    , memsize :: Int
    , cores :: Int
    , threads :: Int
    , start :: Text
    , blocksize :: Int
    , groupsize :: Int
    , maxburstcount :: Int
    , cyclecount :: Int
    , cycledelay :: Int
    } deriving Show
