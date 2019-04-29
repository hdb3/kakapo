{-# LANGUAGE OverloadedStrings , TupleSections , RecordWildCards #-}
module Analysis where
import Data.Text(Text)
import qualified Data.Text as T
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(sort,sortOn,nub)
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
        fst' (x,_,_) = x
        snd' (_,x,_) = x
        thrd' (_,_,x) = x
    if null args then
        die "enter at least a search path"
    else do
        (errors,krecs) <- getKRecV1_  [ args !! 0 ]
        let pots = collate krecs
            headers = nub $ map fst' pots
            summary = map (\(t,n,px) -> (T.unpack t , n , length px )) pots
        if argc == 1 then do
            mapM_ print errors
            mapM_ putStrLn $ countPotsText $ map ( hrV1DESC . krecHeader ) krecs 
            showRange "hrV1BLOCKSIZE" $ map ( hrV1BLOCKSIZE . krecHeader ) krecs
            showRange "hrV1GROUPSIZE" $ map ( hrV1GROUPSIZE . krecHeader ) krecs
            mapM_ print headers
        else if argc == 2 then do
            let selector = T.pack $ args !! 1
                pot = filter ( ( selector == ) . fst' ) pots
                selected = filter ( ( selector == ) . hrV1DESC . krecHeader ) krecs
            putStrLn $ "found " ++ show (length pot) ++ " matches"
            putStrLn $ "found(2) " ++ show (length selected) ++ " matches"
            showRange "hrV1BLOCKSIZE" $ map ( hrV1BLOCKSIZE . krecHeader ) selected
            showRange "hrV1GROUPSIZE" $ map ( hrV1GROUPSIZE . krecHeader ) selected
            mapM_ putStrLn $ countPotsInt $ map ( hrV1GROUPSIZE . krecHeader ) selected
        else if argc == 3 then do
            let selector1 = T.pack $ args !! 1
                selector2 = read $ args !! 2
                crit1 = ( selector1 == ) . hrV1DESC . krecHeader
                crit2 = ( selector2 == ) . hrV1GROUPSIZE . krecHeader
                --pot = filter ( ( selector == ) . fst' ) pots
                selected = filter crit1 $ filter crit2 krecs
            putStrLn $ "found " ++ show (length selected) ++ " matches"
            showRange "hrV1BLOCKSIZE" $ map ( hrV1BLOCKSIZE . krecHeader ) selected
            mapM_ putStrLn $ countPotsInt $ map ( hrV1GROUPSIZE . krecHeader ) selected
        else putStrLn "tl;dr"
        
    putStrLn "Done"

graph :: [(Text, Int, [KRecV1GraphPoint])] -> IO ()
graph _ = return ()

collate :: [KRecV1] -> [(Text, Int, [KRecV1])]
collate krecs =
    let platformDistribution = buildPots ( hrV1DESC . hrec . kRecV1 ) krecs
        groupsizeDistribution = Map.map ( buildPots ( hrV1GROUPSIZE . hrec . kRecV1 )) platformDistribution
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
        print $ sort uniqVals

countPotsText :: [Text] -> [String]
countPotsText = countPots_ (\t -> "[" ++ T.unpack t ++ "]")

countPotsInt :: [Int] -> [String]
countPotsInt = countPots_ show

countPots_ :: Ord a => ( a -> String) -> [a] -> [String]
countPots_ toString = map (\(t,n) -> show n ++ " : " ++ toString t) . sortOn snd . Map.toList . countPots'
    where countPots'  :: Ord a => [a] -> Map.Map a Int
          countPots' = foldl (\m a -> Map.insertWith (+) a 1 m) Map.empty

--countPots :: [Text] -> [String]
--countPots = map (\(t,n) -> show n ++ " : " ++ T.unpack t) . sortOn snd . Map.toList . countPots'
    --where countPots'  :: Ord a => [a] -> Map.Map a Int
          --countPots' = foldl (\m a -> Map.insertWith (+) a 1 m) Map.empty

buildPots :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
buildPots getter = foldl (\m r -> addItem r (getter r) m) Map.empty

    where addItem :: Ord k => a -> k -> Map.Map k [a] -> Map.Map k [a]
          addItem v = Map.alter ( Just . maybe [v] (v :) ) 

unwind :: Map.Map a (Map.Map b c) -> [(a, b, c)]
unwind = map flitten . concatMap flatten . Map.toList . Map.map Map.toList
    where flitten (a, (b, c)) = (a, b, c)
          flatten (k,ax) = map (k,) ax 
