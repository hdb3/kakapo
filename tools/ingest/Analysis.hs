{-# LANGUAGE TupleSections , RecordWildCards #-}
module Analysis where
import qualified Data.Text as T
import System.IO(stderr,hPutStrLn,hPrint,stdout)
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(nub)
import qualified Data.Map.Strict as Map
import Stages hiding (main)
import Summarise hiding (main)


data KRecV1GraphPoint = KRecV1GraphPoint { desc :: T.Text
                                         , blocksize , groupsize :: Int
                                         , value :: Point
                                         --} deriving (Show, Eq)
                                         } deriving Eq
instance Show KRecV1GraphPoint
    where
        show KRecV1GraphPoint {..} = "{ " ++ show blocksize ++ ", " ++ show groupsize ++ " , " ++ show value ++ " }" 
aggregateKRecV1GraphPoints :: [ KRecV1GraphPoint ] -> ( Int, Point )
aggregateKRecV1GraphPoints = foldl (\(n,point) krec -> (n+1, value krec `add` point)) (0, ( 0 , 0.0 , 0.0 , 0.0))

reduceToKRecV1GraphPoint getter krec@KRecV1{..} = let blocksize = kV1BLOCKSIZE
                                                      groupsize = kv1GROUPSIZE
                                                      value = getter krec  
                                                      desc = kV1DESC
                                                  in KRecV1GraphPoint{..}

main = do 
    hPutStrLn stdout "Analysis"
    args <- getArgs
    let argc = length args
        p1 (x,_,_) = x
        p2 (_,x,_) = x
        p3 (_,_,x) = x
    if null args then
        die "enter at least a search path"
    else do
        (errors,krecs) <- getKRecV1_  [(args !! 0)]
        let pots = collate krecs
            headers = nub $ map p1 pots
            summary = map (\(t,n,px) -> (T.unpack t , n , length px )) pots
        if argc < 2 then do
            mapM_ print errors
            --showRange krecs "kV1BLOCKSIZE" kV1BLOCKSIZE
            --showRange krecs "kv1GROUPSIZE" kv1GROUPSIZE
            --showRange krecs "kV1DESC" kV1DESC
            mapM_ (hPrint stdout ) headers
            -- mapM_ (hPrint stdout ) summary
        else do
            let selector = args !! 1
                pot = filter ( ( T.pack selector ==) . p1 ) pots
            mapM_ ( hPrint stdout ) pot
        --hPutStrLn stdout $ unlines $ map fst pots
        
    hPutStrLn stdout "Done"

graph :: [(T.Text, Int, [KRecV1GraphPoint])] -> IO ()
graph _ = return ()

collate :: [KRecV1] -> [(T.Text, Int, [KRecV1GraphPoint])]
collate krecs =
    let platformDistribution = buildPots kV1DESC krecs
        groupsizeDistribution = Map.map ( buildPots kv1GROUPSIZE ) platformDistribution
        rttDistribution = Map.map (Map.map (map ( reduceToKRecV1GraphPoint kv1RTT ))) groupsizeDistribution
        output = unwind rttDistribution
    in output

showRange :: (Show b, Eq b) => [a] -> String -> (a -> b) -> IO ()
showRange recs label p = do
    let vals = map p recs
        uniqVals = nub vals
    hPutStrLn stdout $ "showRange (" ++ label ++ ")"
    hPutStrLn stdout $ show (length vals) ++ " rows"
    hPutStrLn stdout $ show (length uniqVals) ++ " uniqVals"
    mapM_ ( hPrint stdout ) (take 20 uniqVals)

buildPots :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
buildPots getter = foldl (\m r -> addItem r (getter r) m) Map.empty

    where addItem :: Ord k => a -> k -> Map.Map k [a] -> Map.Map k [a]
          addItem v = Map.alter ( Just . maybe [v] (v :) ) 

unwind :: Map.Map a (Map.Map b c) -> [(a, b, c)]
unwind = map flitten . concatMap flatten . Map.toList . Map.map Map.toList
    where flitten (a, (b, c)) = (a, b, c)
          flatten (k,ax) = map (k,) ax 
