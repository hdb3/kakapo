module Summary where

import Data.Text(Text)
import qualified Data.Text as T
import Control.Arrow(first,second)
--import Data.Map.Strict(empty,alter,insertWith,Map(),toList)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.List(sortOn)
import Data.Maybe(fromMaybe)
import GenParse(Sample)

summarise :: [(Text,Text)] -> [(Text,[(Text,Int)])]
summarise = map (second Map.toList) . Map.toList . summariseMap

displayMap :: Map Text (Map Text Int) -> String
displayMap m = let
    l1 :: [(Text, Map Text Int)]
    l1 = Map.toAscList m 
    l2 :: [(Text, [(Text,Int)])]
    l2 = map (second Map.toAscList) l1
    l3 :: [(String, [(Text,Int)])]
    l3 = map (first T.unpack) l2
    l4 :: [(String, [(String,Int)])]
    l4 = map ( second ( map (first T.unpack))) l3 
    l5 :: [(String, [(String,String)])]
    l5 = map ( second ( map (second show))) l4 
    l6 :: [(String, [String])]
    l6 = map ( second ( map join1 )) l5 
    l7 :: [(String, String)]
    l7 = map ( second unwords) l6 
    l8 :: [String]
    l8 = map join2 l7 
    l9 :: String
    l9 = unlines l8
    join1 (t0,t1) = t0 ++ "(" ++ t1 ++ ")"
    join2 (t0,t1) = t0 ++ " : " ++ t1
    --tuplelist = map ( second (unwords . map display . Map.toAscList) . first T.unpack ) (Map.toAscList m)
    --display (v,n) = T.unpack v ++ "(" ++ show n ++ ")"
    --in map (\(t0,t1) -> t0 ++ " : " ++ t1) tuplelist 
    in l9

displayMap' :: Map Text (Map Text Int) -> String
displayMap' m = let
    join1 (t0,t1) = t0 ++ "(" ++ t1 ++ ")"
    join2 (t0,t1) = t0 ++ " : " ++ t1
    in unlines (map (join2 .  second ( unwords . map ( join1 . second show . first T.unpack)) . first T.unpack . second Map.toAscList) (Map.toAscList m))

summariseMap :: [(Text,Text)] -> Map Text (Map Text Int)
summariseMap = foldl outer Map.empty

    where

    outer :: Map Text (Map Text Int) -> (Text,Text) -> Map Text (Map Text Int)
    outer m ( k, v) = Map.alter (Just . Map.insertWith (+) v 1 . fromMaybe Map.empty) k m

    outer' m ( k, v) = Map.alter (f v) k m
        where f :: Text -> Maybe (Map Text Int) -> Maybe (Map Text Int)
              f v Nothing = Just $ inner v Map.empty
              f v (Just m) = Just $ inner v m
              inner :: Text -> Map Text Int -> Map Text Int
              inner v = Map.insertWith (+) v 1


shortReport :: String -> [Sample] -> IO ()
shortReport title samples = do
    putStrLn $ "\nshortReport for " ++ title 
    let hdrs = Summary.summarise $ concatMap fst samples
        count = length samples
    putStrLn $ show count ++ " samples found"
    let nlabels = length hdrs
    putStrLn $ show nlabels ++ " metadata labels found"
    let pInvariant = (1 ==) . length . snd
        pUnassociated = ((count `div` 2) < ) . length . snd
        pVariable x = not ( pInvariant x) && not ( pUnassociated x)
        invariants = filter pInvariant hdrs
        unassociated = filter pUnassociated hdrs
        variable = filter pVariable hdrs
        showCount (t,ax) = T.unpack t ++ "(" ++ show ( length ax ) ++ ")"
    putStrLn $ show (length invariants) ++ " invariants found: " ++ unwords ( map showCount invariants)
    putStrLn $ show (length unassociated) ++ " unassociated found: " ++ unwords ( map showCount unassociated)
    putStrLn $ show (length variable) ++ " variable found: " ++ unwords ( map showCount variable)

fullReport :: String -> [Text] -> [Sample] -> IO () 
fullReport title excludes samples = do
    putStrLn $ "\nfullReport for " ++ title
    let hdrs = Summary.summarise $ concatMap fst samples
        sortedHeaders = map (second (reverse . sortOn snd)) $ sortOn fst $ filter (not . flip elem excludes. fst) hdrs 
    mapM_ (putStrLn . display) sortedHeaders

    where
        display (k,vs) = T.unpack k ++ " : " ++ show (length vs) ++ " { " ++ unwords ( map show' vs) ++ " }"
        show' (t,i) = T.unpack t ++ "[" ++ show i ++ "]"
