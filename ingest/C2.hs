{-# LANGUAGE OverloadedStrings #-}
module C2 where
import Constraints
import GenParse
import qualified Data.Map.Strict as Map
import Data.Text(Text)
import Data.Maybe(mapMaybe)

-- this is the first pass
-- its primary role is to partition the results based on the index constraint
-- it also implements definite exclusions (failed matches)
-- the threded accumulator is outer Map which contains lists of accepted samples
-- in addition to partitioning the function should remove the headers which have been matched, including both match and index constraints
-- this is accomplished most easily by building the new header as the fold proceeds
--
-- more specifically, the processingfor each sample proceeds as follows:
-- fold over the headers, copying unmatched headers.  Matched hedaers are dropped, mismatched headers terminate the chain.
-- A mtahced index value is prpagated as a just value.  Where an index constraint is present but not matched then the sample is discarded.
-- The eventual value of the fold is a headrr list and Maybe index value.
--
-- strict vs loose processing
-- in a strict mode all of the constraints should be satisfied (matching headers found).
-- this can most easily be detected by accumulating the matched constraints as they fire, in the accumulator, or, by removing from the Map contining them
--
--  Note:  it is implicit that there should be at most one constraint on a given hedaer key
--
-- the accumulator is a tuple of the processed hedaers, a found index, the shrinking map and header list.  the initial value is (Nothing,[],map,heasers)
-- here is the kernel

-- it is assumed that the Constraints are already filtered for just match and index

prove = do
    sample <- readData "var/webdav/frr/1557764247"
    print sample
    putStrLn "\nTOPIC BASIC"
    putStrLn $ showResult $ kernel ( Map.fromList [("TOPIC", Equality "BASIC")] ) sample
    putStrLn "\nTOPIC BASSIC"
    putStrLn $ showResult $ kernel ( Map.fromList [("TOPIC", Equality "BASSIC")] ) sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,"
    putStrLn $ showResult $ kernel ( Map.fromList [("TOPIC", Equality "BASIC"),("PLATFORM", Index [])] ) sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,PID ANY, START ANY"
    putStrLn $ showResult $ kernel ( Map.fromList [("TOPIC", Equality "BASIC"),("PLATFORM", Index []),("PID",Any),("START",Any)] ) sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,PAD ANY, STIRT ANY"
    putStrLn $ showResult $ kernel ( Map.fromList [("TOPIC", Equality "BASIC"),("PLATFORM", Index []),("PAD",Any),("STIRT",Any)] ) sample
    where
        showResult Nothing = "Nothing"
        showResult ( Just (mt,(dict,_),tcx)) = "Maybe index=" ++ show mt ++ "\nnew dict\n" ++ unlines (map show dict) ++ "unused constraints\n" ++ unlines (map show tcx)

-- Constraints: type Selector =  Map.Map Text Constraint
-- this outer loop accumulates valid samples and assigns them to the correct bucket based on the result of index extraction
-- the result is empty in cases where: no index constraint was given; no sample has index parameter in header; no sample has index parameter in given non empty index list
-- for a non-indexed 'select' use 'select_'

-- the pattern in which a tuple value is added to a list contained in a Map is so common+useful that it needs its own name....
-- insert_
insert_ :: Ord k => Map.Map k [a] -> (k,a) -> Map.Map k [a]
insert_ m (k,a) = Map.alter (Just . maybe [a] (a :)) k m
-- and the pattern in which insert_ is applied to a list of tuples is also so common that...
fromList_:: Ord k => [(k,a)] -> Map.Map k [a]
fromList_ = foldl insert_ Map.empty

-- this version of partition/select is permissive -- only explicit exclusions are executed -- wildcards merely remove corresponding header fields
partition :: Selector-> [Sample] -> Map.Map Text [Sample]
partition selector = fromList_ . mapMaybe (f . kernel selector)
    where
    f (Just (Just i,s,_)) = Just (i,s)
    f _ = Nothing

-- this version of select is also permissive, but it does not  partition for subgraphs.
-- it does this by remving and Index constraint, and discarding all but the actual returned sample from the analysis
-- this has the desired result of not removing any potential Indexed header

-- TODO - integrate with Constraints.hs, rename select preselect
-- TODO - rename third second
select :: Selector-> [Sample] -> [Sample]
select selector = map third . mapMaybe (kernel (Map.filter (not . isIndex) selector))
    where
    third (_,s,_) = s

kernel :: Selector -> Sample -> Maybe (Maybe Text,Sample,[(Text,Constraint)])
kernel selector (headers,metrics) = let
    selector' = Map.filter (\a -> isAny a || isIndex a || isEquality a ) selector
    in fmap (\(newHeaders,index,remMap,[]) -> (index,(newHeaders,metrics),Map.toList remMap)) ( go ([],Nothing,selector',headers))

    where

        go :: ( [(Text,Text)] , Maybe Text , Map.Map Text Constraint , [(Text,Text)] ) -> Maybe ( [(Text,Text)] , Maybe Text , Map.Map Text Constraint , [(Text,Text)] ) 
        go (a,b,c,[]) = Just (a,b,c,[])
        go (nh,mi,m, kv@(k,v):kvs) = maybe ( go (kv:nh,mi,m, kvs) ) f (Map.lookup k m)
            where
                  f Any  = go (nh,mi,Map.delete k m,kvs)
                  f ( Equality t) = if t==v then go (nh,mi,Map.delete k m,kvs) else Nothing
                  f (Index [])  = go (nh,Just v,Map.delete k m,kvs)
                  f (Index indices)  = if v `elem` indices then go (nh, Just v, Map.delete k m,kvs) else Nothing
                  f _ = error "contsraints were not filtered"
