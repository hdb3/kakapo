{-# LANGUAGE OverloadedStrings , TupleSections , RecordWildCards #-}
module Analysis where
import Data.Text(Text)
import qualified Data.Text as T
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(sortOn,nub)
import Data.Maybe(fromJust)
import qualified Data.Map.Strict as Map
import Stages hiding (main)


data KRecV1GraphPoint = KRecV1GraphPoint { desc :: Text
                                         , blocksize , groupsize :: Int
                                         , value :: [Text]
                                         } deriving Eq
instance Show KRecV1GraphPoint
    where
        show KRecV1GraphPoint {..} = "{ " ++ show blocksize ++ ", " ++ show groupsize ++ " , " ++ show ( length value) ++ " }" 

concatKRecV1GraphPoints :: [ KRecV1GraphPoint ] -> KRecV1GraphPoint
concatKRecV1GraphPoints = foldl1 (\k0 k -> k0 { value = value k0 ++ value k } )

reduceToKRecV1GraphPoint :: Text -> KRecV1 -> KRecV1GraphPoint
reduceToKRecV1GraphPoint selector ( KRecV1 GKRecV1{..} ) = let blocksize = hrV1BLOCKSIZE hrec
                                                               groupsize = hrV1GROUPSIZE hrec
                                                               desc = hrV1DESC hrec
                                                               value = fromJust $ lookup selector values
                                                           in KRecV1GraphPoint{..}

main = do 
    putStrLn "Analysis"
    args <- getArgs
    let argc = length args
        p1 (x,_,_) = x
        p2 (_,x,_) = x
        p3 (_,_,x) = x
    if null args then
        die "enter at least a search path"
    else do
        (errors,krecs) <- getKRecV1_  [ args !! 0 ]
        let pots = collate krecs
            headers = nub $ map p1 pots
            summary = map (\(t,n,px) -> (T.unpack t , n , length px )) pots
        if argc < 2 then do
            mapM_ print errors
            mapM_ putStrLn $ map (\(t,n) -> show n ++ " : " ++ T.unpack t) $ sortOn snd $ Map.toList $ countPots $ map ( hrV1DESC . krecHeader ) krecs 
            --showRange "hrV1DESC" $ map ( hrV1DESC . krecHeader ) krecs
            showRange "hrV1BLOCKSIZE" $ map ( hrV1BLOCKSIZE . krecHeader ) krecs
            showRange "hrV1GROUPSIZE" $ map ( hrV1GROUPSIZE . krecHeader ) krecs
            mapM_ print headers
            -- mapM_ (print ) summary
        else do
            let selector = T.pack $ args !! 1
                pot = filter ( ( selector == ) . p1 ) pots
            putStrLn $ "found " ++ show (length pot) ++ " matches"
        
    putStrLn "Done"

graph :: [(Text, Int, [KRecV1GraphPoint])] -> IO ()
graph _ = return ()

collate :: [KRecV1] -> [(Text, Int, [KRecV1])]
collate krecs =
    let platformDistribution = buildPots ( hrV1DESC . hrec . kRecV1 ) krecs
        groupsizeDistribution = Map.map ( buildPots ( hrV1GROUPSIZE . hrec . kRecV1 )) platformDistribution
        --rttDistribution = Map.map (Map.map (map ( reduceToKRecV1GraphPoint "RTT" ))) groupsizeDistribution
        --output = unwind rttDistribution
        output = unwind groupsizeDistribution
    in output

showRange :: (Show b, Eq b, Ord b) => String -> [b] -> IO ()
showRange label vals = do
    let uniqVals = nub vals
        minVal = minimum uniqVals
        maxVal = maximum uniqVals
    putStrLn $ "showRange (" ++ label ++ ")"
    putStrLn $ show (length uniqVals) ++ " uniqVals"
    if length uniqVals > 10 then
        putStrLn $ "min/max = " ++ show ( minVal , maxVal )
    else
        mapM_ print uniqVals

countPots :: Ord a => [a] -> Map.Map a Int
countPots = foldl (\m a -> Map.insertWith (+) a 1 m) Map.empty

buildPots :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
buildPots getter = foldl (\m r -> addItem r (getter r) m) Map.empty

    where addItem :: Ord k => a -> k -> Map.Map k [a] -> Map.Map k [a]
          addItem v = Map.alter ( Just . maybe [v] (v :) ) 

unwind :: Map.Map a (Map.Map b c) -> [(a, b, c)]
unwind = map flitten . concatMap flatten . Map.toList . Map.map Map.toList
    where flitten (a, (b, c)) = (a, b, c)
          flatten (k,ax) = map (k,) ax 
