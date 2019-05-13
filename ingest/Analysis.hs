{-
  Copyright 2019 Nicholas Hart
-}

{-# LANGUAGE OverloadedStrings , TupleSections , RecordWildCards #-}
module Main where
import Data.Text(Text)
import qualified Data.Text as T
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(sort,sortOn,nub,partition)
import Data.Maybe(fromJust)
import qualified Data.Map.Strict as Map
import Text.Read(readMaybe)
import Control.Arrow (second)
import Data.Char (isSpace)
import Control.Monad(unless)

import Stages hiding (main)
import Mean
import GPlot hiding (main)

data KRecV1GraphPoint = KRecV1GraphPoint { desc :: Text
                                         , blocksize , groupsize :: Int
                                         , value :: [Text]
                                         } deriving Eq
instance Show KRecV1GraphPoint
    where
        show KRecV1GraphPoint {..} = "{ " ++ show blocksize ++ ", " ++ show groupsize ++ " , " ++ show ( length value) ++ " }" 

concatKRecV1GraphPoints :: [ KRecV1GraphPoint ] -> KRecV1GraphPoint
concatKRecV1GraphPoints = foldl1 (\k0 k -> k0 { value = value k0 ++ value k } )

main :: IO ()
main = do 
    putStrLn "Analysis"
    args <- getArgs
    let argc = length args
        fst3 (x,_,_) = x
    if null args then
        die "enter at least a search path"
    else do
        (errors,krecs) <- getKRecV1_  [ args !! 0 ]
        let pots = collate krecs
            selection = countPotsWithIndex $ map ( hrV1DESC . krecHeader ) krecs
            selectionText = map (\(t,x,y) -> "(" ++ show y ++ " , " ++ T.unpack t ++ " , " ++ show x ++ " )") selection
            -- only works for single parameters here...., but we may not care....
            selector1 = maybe ( T.pack $ args !! 1)
                              ( fst3 . (selection !!))
                              ( readMaybe (args !! 1))
        if argc == 1 then do
            unless (null errors) 
                   ( do putStrLn "*** Errors ***"
                        mapM_ putStrLn errors )
            putStrLn "\n*** Headers ***"
            mapM_ putStrLn selectionText
            putStrLn ""
            showRange "hrV1BLOCKSIZE" $ map ( hrV1BLOCKSIZE . krecHeader ) krecs
            showRange "hrV1GROUPSIZE" $ map ( hrV1GROUPSIZE . krecHeader ) krecs
        else if argc == 2 then do
            let
                pot = filter ( ( selector1 == ) . fst3 ) pots
                selected = filter ( ( selector1 == ) . hrV1DESC . krecHeader ) krecs
            putStrLn $ "found " ++ show (length pot) ++ " matches"
            putStrLn $ "found(2) " ++ show (length selected) ++ " matches"
            showRange "hrV1BLOCKSIZE" $ map ( hrV1BLOCKSIZE . krecHeader ) selected
            showRange "hrV1GROUPSIZE" $ map ( hrV1GROUPSIZE . krecHeader ) selected
            mapM_ putStrLn $ countPotsInt $ map ( hrV1GROUPSIZE . krecHeader ) selected

        else do
            let getArgFields n dflt = if n < argc then  qFields (args !! n) else dflt
                getArgFields' n = getArgFields n []
                res = [ getGraph krecs a b c d e | a <- getArgFields' 1 , -- the header selector
                                                   b <- getArgFields' 2 , -- the control parameter to fix
                                                   c <- getArgFields 3 ["RTT"] ,
                                                   d <- getArgFields 4 ["MEAN"] ,
                                                   e <- getArgFields 5 ["LINEAR"] ]
                (linearGraphs,logGraphs) = partition isLinear res
                isLinear (_,_,_,_,_,s) = s == "LINEAR"
            putStrLn $ show (length res) ++ " graphs returned"
            plotLinear linearGraphs
            plotLog logGraphs

            return ()
    putStrLn "Done"

    where

    -- copied for simplicity and independence from tools/Sections.hs
    qFields :: String -> [String]
    qFields s = let
        trim = dropWhile isSpace
        backTrim = takeWhile (not . isSpace) . trim
        isComma c = ',' == c
        in case (trim . dropWhile isComma . trim) s of
                   "" -> []
                   ('"' : s') -> w : qFields (tail s'') where (w, s'') = break ('"' ==) s'
                   s' -> backTrim w : qFields s'' where (w, s'') = break isComma s'


    getGraph krecs headerDesc selector2 metric reduce linlog =
    -- scope for validation: selector1 is either an index (Int) or full descriptor
    --                                 in future should allow to specify a field selection like UUID
    --                       selector is for now only the blocksize, but shortly alternatively groupsize, Int in either case
        let 
            fst3 (x,_,_) = x
            selection = countPotsWithIndex $ map ( hrV1DESC . krecHeader ) krecs
            selector1 = maybe ( T.pack headerDesc )
                              ( fst3 . (selection !!) )
                              ( readMaybe headerDesc )

            crit1 = ( selector1 == ) . hrV1DESC . krecHeader
            crit2 = ( read selector2 == ) . hrV1GROUPSIZE . krecHeader

            -- 'selected' is unrefined list of krecs
            selected = filter crit1 $ filter crit2 krecs

            -- for simple plots we need - just one parameter (say, BLOCKSIZE) from the header -- we assume all others are fixed!
            --                          - and, some data points, for a single metric/observable, e.g. RTT, etc.
            -- this constitutes a single graph, albeit plottable in different way and with different summations (means, etc)
            -- Regradless, the data structure is [(Int,[Double])]


            readFloat :: T.Text -> Double
            readFloat s = read (T.unpack s) :: Double

            getObservable' :: String -> KRecV1 -> [Double]
            getObservable' s kx = map readFloat ( getObservable s kx )

            getObservable :: String -> KRecV1 -> [Text]
            getObservable selector = fromJust . lookup (T.pack selector ) . krecValues


            -- graph is the fully extracted raw sample set:  [(Int,[Double])]
            graph = sortOn fst $ map (\krec -> ( hrV1BLOCKSIZE $ krecHeader krec , getObservable' metric krec )) selected
            title = metric ++ " for dataset [" ++ T.unpack selector1 ++ "]/[" ++ show selector2 ++ "]"

            graphMean = map (second ( mean. tail) )
            graphMin = map (second minimum)
            graphMax = map (second maximum)

            path = case reduce of
                "MEAN" -> graphMean graph 
                "MIN" -> graphMin graph 
                "MAX" -> graphMax graph 
       in (path, T.unpack selector1, selector2, metric, reduce, linlog)


loglog :: [(Int,Double)] -> [(Double,Double)]
loglog = map (\(a,x) -> (logBase 10 (fromIntegral a) , logBase 10 x))

plotLog :: [([(Int,Double)], String, String, String, String, String)] -> IO ()
plotLog gx | null gx = return()
           | otherwise = do
    putStrLn $ "plotLog: " ++ show (length gx ) ++ " paths"
    renderCurves " (log plot)" "block size" "seconds" $ map getCurve gx
    where
    getCurve (g, s1, s2 ,s3, s4, s5 ) = makeCurve ( s2 ++ "/" ++ s3 ++ "/" ++ s4 ) ( loglog g)

plotLinear :: [([(Int,Double)], String, String, String, String, String)] -> IO ()
plotLinear gx | null gx = return()
              | otherwise = do
    putStrLn $ "plotLinear: " ++ show (length gx ) ++ " paths"
    mapM_ plotLinear1 gx
    putStrLn ""
    renderCurves " (linear plot)" "block size" "seconds" $ map getCurve gx
    where
    plotLinear1 (g, s1, s2 ,s3, s4, s5 ) = putStrLn $ "          : " ++ show (length g) ++ " points " ++ s1 ++ "/" ++ s2 ++ "/" ++ s3 ++ "/" ++ s4 ++ "/" ++ s5
    getCurve (g, s1, s2 ,s3, s4, s5 ) = makeCurve ( s2 ++ "/" ++ s3 ++ "/" ++ s4 ) g


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
countPots_ toString = map (\(t,n) -> toString t ++ " : " ++ show n ) . sortOn fst . countPots

countPots :: Ord a => [a] -> [(a,Int)]
countPots = Map.toList . foldl (\m a -> Map.insertWith (+) a 1 m) Map.empty

countPotsWithIndex :: Ord a => [a] -> [(a,Int,Int)]
countPotsWithIndex = map (\((a,b),c) -> (a,b,c)) . flip zip [0..] . countPots 

buildPots :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
buildPots getter = foldl (\m r -> addItem r (getter r) m) Map.empty

    where addItem :: Ord k => a -> k -> Map.Map k [a] -> Map.Map k [a]
          addItem v = Map.alter ( Just . maybe [v] (v :) ) 

unwind :: Map.Map a (Map.Map b c) -> [(a, b, c)]
unwind = map flitten . concatMap flatten . Map.toList . Map.map Map.toList
    where flitten (a, (b, c)) = (a, b, c)
          flatten (k,ax) = map (k,) ax 
