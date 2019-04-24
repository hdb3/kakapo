{-# LANGUAGE TupleSections , RecordWildCards #-}
module Analysis where
import qualified Data.Text as T
import System.IO(stderr,hPutStrLn,hPrint)
import Data.List(nub)
import qualified Data.Map.Strict as Map
import Stages hiding (main)
import Summarise hiding (main)


data KRecV1GraphPoint = KRecV1GraphPoint { desc :: T.Text
                                         , blocksize , groupsize :: Int
                                         , value :: Point
                                         } deriving (Show, Eq)

reduceToKRecV1GraphPoint getter krec@KRecV1{..} = let blocksize = kV1BLOCKSIZE
                                                      groupsize = kv1GROUPSIZE
                                                      value = getter krec  
                                                      desc = kV1DESC
                                                  in KRecV1GraphPoint{..}

main = do 
    produce >>= mapM_ (hPrint stderr <$> (\(t,n,px) -> (T.unpack t , n , length px )))
    hPutStrLn stderr "Done"

graph :: [(T.Text, Int, [KRecV1GraphPoint])] -> IO ()
graph _ = return ()

produce :: IO [(T.Text, Int, [KRecV1GraphPoint])]
produce = do
   hPutStrLn stderr "Analysis"
   (errors,krecs) <- getKRecV1
   mapM_ print errors
   showRange krecs "kV1BLOCKSIZE" kV1BLOCKSIZE
   showRange krecs "kv1GROUPSIZE" kv1GROUPSIZE
   showRange krecs "kV1DESC" kV1DESC
   let platformDistribution = buildPots kV1DESC krecs
       groupsizeDistribution = Map.map ( buildPots kv1GROUPSIZE ) platformDistribution
       rttDistribution = Map.map (Map.map (map ( reduceToKRecV1GraphPoint kv1RTT ))) groupsizeDistribution
       output = unwind rttDistribution
   return output

showRange :: (Show b, Eq b) => [a] -> String -> (a -> b) -> IO ()
showRange recs label p = do
    let vals = map p recs
        uniqVals = nub vals
    hPutStrLn stderr $ "showRange (" ++ label ++ ")"
    hPutStrLn stderr $ show (length vals) ++ " rows"
    hPutStrLn stderr $ show (length uniqVals) ++ " uniqVals"
    mapM_ ( hPrint stderr ) (take 20 uniqVals)

buildPots :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
buildPots getter = foldl (\m r -> addItem r (getter r) m) Map.empty

    where addItem :: Ord k => a -> k -> Map.Map k [a] -> Map.Map k [a]
          addItem v = Map.alter ( Just . maybe [v] (v :) ) 

unwind :: Map.Map a (Map.Map b c) -> [(a, b, c)]
unwind = map flitten . concatMap flatten . Map.toList . Map.map Map.toList
    where flitten (a, (b, c)) = (a, b, c)
          flatten (k,ax) = map (k,) ax 
