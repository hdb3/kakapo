{-# LANGUAGE OverloadedStrings #-}
module C2 where
import Constraints
import GenParse
import qualified Data.Map.Strict as Map
import Data.Text(Text)

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
    putStrLn $ showResult $ kernel [("TOPIC", Equality "BASIC")] sample
    putStrLn "\nTOPIC BASSIC"
    putStrLn $ showResult $ kernel [("TOPIC", Equality "BASSIC")] sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,"
    putStrLn $ showResult $ kernel [("TOPIC", Equality "BASIC"),("PLATFORM", Index [])] sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,PID ANY, START ANY"
    putStrLn $ showResult $ kernel [("TOPIC", Equality "BASIC"),("PLATFORM", Index []),("PID",Any),("START",Any)] sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,PAD ANY, STIRT ANY"
    putStrLn $ showResult $ kernel [("TOPIC", Equality "BASIC"),("PLATFORM", Index []),("PAD",Any),("STIRT",Any)] sample
    where
        showResult Nothing = "Nothing"
        showResult ( Just (mt,(dict,_),tcx)) = "Maybe index=" ++ show mt ++ "\nnew dict\n" ++ unlines (map show dict) ++ "unused constraints\n" ++ unlines (map show tcx)

kernel :: [(Text,Constraint)] -> Sample -> Maybe (Maybe Text,Sample,[(Text,Constraint)])
kernel selector (headers,metrics) = let
    map = Map.fromList selector
    in fmap (\(newHeaders,index,remMap,[]) -> (index,(newHeaders,metrics),Map.toList remMap)) ( go ([],Nothing,map,headers))

go :: ( [(Text,Text)] , Maybe Text , Map.Map Text Constraint , [(Text,Text)] ) -> Maybe ( [(Text,Text)] , Maybe Text , Map.Map Text Constraint , [(Text,Text)] ) 
go (a,b,c,[]) = Just (a,b,c,[])
go (nh,mi,m, kv@(k,v):kvs) = maybe ( go (kv:nh,mi,m, kvs) ) f (Map.lookup k m)
    where f ( Equality t) = if t==v then go (nh,mi,Map.delete k m,kvs) else Nothing
          f ( Any ) = go (nh,mi,Map.delete k m,kvs)
          f ( (Index []) ) = go (nh,Just v,Map.delete k m,kvs)
          f ( (Index indices) ) = if elem v indices then go (nh, Just v, Map.delete k m,kvs) else Nothing
          f _ = error "contsraints were not filtered"
