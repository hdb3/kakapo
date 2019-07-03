{-# LANGUAGE OverloadedStrings #-}
module Constraints where
import Data.Attoparsec.Text
import Data.Text(Text)
import qualified Data.Text as T
import Data.Maybe(isNothing,mapMaybe)
import Control.Applicative((<|>))
import Data.Either(partitionEithers)
import qualified Data.Map.Strict as Map
import System.Exit(die)
import Data.List(sortBy,elem)
import GenParse
import CompareText(compareText)

constraintsHelp = unlines [ "MATCH=B"
                          , "RANGE=1-9"
                          , "INDEX=you,me,both"
                          , "EMPTYINDEX=,"
                          , "WILDCARD=*"
                          , "CONTROL=?"
                          , "SELECT=>"
                          , "AGGREGATE=+"
                          ]

prove = do
    print $ getConstraint "MATCH=B"
    print $ getConstraint "RANGE=1-9"
    print $ getConstraint "INDEX=you,me,both"
    print $ getConstraint "EMPTYINDEX=,"
    print $ getConstraint "WILDCARD=*"
    print $ getConstraint "CONTROL=?"
    print $ getConstraint "SELECT=>"
    print $ getConstraint "AGGREGATE=+"
    putStrLn "Done"

data Constraint = Control | Any | Equality Text | Range Int Int | Index [ Text ] | Select deriving ( Eq, Show )

isAny Any = True
isAny _ = False

isControl Control = True
isControl _ = False

isIndex ( Index _ ) = True
isIndex _ = False

isEquality ( Equality _ ) = True
isEquality _ = False

isVariable c = isIndex c || isControl c
isFixedPoint = not . isVariable

type Selector =  Map.Map Text Constraint

showSelector :: Selector -> String
showSelector = unwords . map showConstraint . Map.toList

showConstraint :: (Text, Constraint) -> String
showConstraint (t,Control) = "dependent variable:" ++ T.unpack t
showConstraint (t,Any) = T.unpack t ++ "=*"
showConstraint (t,Equality v) = T.unpack t ++ "==" ++ T.unpack v
showConstraint (t,Range l h) = T.unpack t ++ "in [" ++ show l ++ "-" ++ show h ++ "]"
showConstraint (t,Index []) = "multiplot over:" ++ T.unpack t
showConstraint (t,Index gx) = "multiplot over:" ++ T.unpack t ++ " [" ++ unwords ( map T.unpack gx) ++ "]"

selectorVariables :: Selector -> [Text]
selectorVariables = map fst . filter ( isVariable . snd ) . Map.toList

selectorFixedPoints :: Selector -> [Text]
selectorFixedPoints = map fst . filter ( isFixedPoint . snd ) . Map.toList

type SelectResult = Map.Map Text ( Map.Map Text Sample )
map2list :: SelectResult -> [(Text, [(Text, Sample)])]
map2list = textSort . Map.toAscList . Map.map ( textSort . Map.toAscList )
    where textSort = sortBy (\(a,_) (b,_) -> compareText a b)
--map2list = Map.toAscList . . Map.map Map.toAscList

flatten :: [(a, [(b, c)])] -> [(a, b, c)]
flatten = concat . map (\(a,ax) -> map (\(b,c) -> (a,b,c)) ax)

map2FlatList :: SelectResult -> [(Text, Text, Sample)]
map2FlatList = flatten . map2list

querySelector :: (Constraint -> Bool) -> Selector -> [Text]
-- get the constrained headers of a given sort, e.g. all of the Control constraints
querySelector p = Map.keys . Map.filter p

-- constraint application: transform an accumulator depending on the found constraint and given value
-- logic: get the constraint (or Nothing)
-- by case apply the constraint to the acc
-- the acc is simply a Maybe tuple of Maybe Text, set initially to Just (Nothing,Nothing)
-- match failure returns Nothing
-- Control and Index matches update fst or snd respectively with Just.
-- unmatched constraints are not detected, except for Control and Index

emptySelectResult :: SelectResult
emptySelectResult = Map.empty

select :: Selector -> [Sample] -> SelectResult
select selector = foldl (inner selector) emptySelectResult

inner :: Selector -> SelectResult -> Sample -> SelectResult
inner selector base sample@(header,content) = let
    indexes = filter isIndex $ Map.elems selector
    indexNotRequested = null indexes
    openIndexRequested = not indexNotRequested && head indexes == Index []
    Index enumeratedIndexes = head indexes
    acc0 = Just (Nothing,Nothing)
    accFinal = foldl (\acc (k,v) -> f (Map.lookup k selector) acc v) acc0 header

    --f Nothing                   _                  _ = Nothing -- lookup failed -> unexpected header parameter
    f Nothing                   x                  _ = x -- ignore lookup failed
    f _                         Nothing            _ = Nothing -- propagate an earlier failure in the chain
    f ( Just Any )              x                  _ = x
    f ( Just Control )          (Just (Nothing,x)) v = Just (Just v,x)
    f ( Just (Index _) )        (Just (x,Nothing)) v = Just (x,Just v)
    f ( Just (Equality t) )     x                  v = if t == v then x else Nothing
    f ( Just (Range low high) ) x                  v = if read ( T.unpack v) < low || read ( T.unpack v) > high then Nothing else x
    f c                         x                  v = error $ show (c,x,v)

    --g (control,sample) Nothing = Just $ Map.singleton control sample
    --g (control,sample) (Just m) = Just $ Map.insertWith h control sample m
    g (control,sample) = Just . maybe (Map.singleton control sample) (Map.insertWith h control sample)
-- CRUCIAL functional explanation:
-- when adding a new sample to the result map in a slot which is NOT empty......
--      .... the _metrics_ are concatenated, but the older headers (dictionary) is discarded (replaced entirely by the new header)
-- !!!!!!
    h (newH,newM) (oldH,oldM) =  (newH,newM++oldM)

    in case accFinal of
        Nothing                         -> base
        Just (Nothing,_)                -> base
        Just (Just control, Nothing)    -> if indexNotRequested then Map.alter (g (control,sample)) "" base else base
        Just (Just control, Just index) -> if openIndexRequested || elem index enumeratedIndexes then Map.alter (g (control,sample)) index base else base


type RawConstraint = Either String (Text, Constraint)

getConstraint :: String -> RawConstraint
getConstraint s = parseOnly parseConstraint $ T.pack s

parseConstraint :: Parser (Text,Constraint)
parseConstraint = do
    key <- takeTill1 ('='==)
    char '='
    pred <- select <|> control <|> wildcard <|> single <|> range <|> emptyIndex <|> index
    return (key,pred)

    where

        select = do
            char '>'
            requireEOT
            return Select

        control = do
            char '?'
            requireEOT
            return Control

        wildcard = do
            char '*'
            requireEOT
            return Any

        single = do
            match <- Data.Attoparsec.Text.takeWhile (notInClass "-,")
            requireEOT
            return $ Equality match

        range = do
            low <- decimal
            char '-'
            high <- decimal
            requireEOT
            return $ Range low high

        emptyIndex = do
            char ','
            requireEOT
            return $ Index []

        index = do
            vx <- takeTill1 (','==) `sepBy1` char ','
            requireEOT
            return $ Index vx

        takeTill1 p = takeWhile1 (not . p)
        requireEOT :: Parser ()
        requireEOT = do
            m <- peekChar
            if isNothing m then return() else fail "eot"

-- TODO  add some constraint checkers for different contexts
buildSelector :: [ RawConstraint ] -> IO Selector
buildSelector rawConstraints = do
    let (fails,constraints) = partitionEithers rawConstraints
    if
        not (null fails)
    then do
        putStrLn "Couldn't read selection constraints:"
        mapM_ putStrLn fails
        die ""
    else do
        --when ( 1 /= length ( filter ( isControl . snd ) constraints))
             --(die "exactly one control parameter required")
        --when ( 1 < length ( filter ( isIndex . snd ) constraints))
             --(die "at most one index parameter required")
        return $ Map.fromList constraints

-- C2.hs below the line


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

c2Prove = do
    sample <- readData "var/webdav/frr/1557764247"
    print sample
    putStrLn "\nTOPIC BASIC"
    putStrLn $ showResult $ c2Kernel ( Map.fromList [("TOPIC", Equality "BASIC")] ) sample
    putStrLn "\nTOPIC BASSIC"
    putStrLn $ showResult $ c2Kernel ( Map.fromList [("TOPIC", Equality "BASSIC")] ) sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,"
    putStrLn $ showResult $ c2Kernel ( Map.fromList [("TOPIC", Equality "BASIC"),("PLATFORM", Index [])] ) sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,PID ANY, START ANY"
    putStrLn $ showResult $ c2Kernel ( Map.fromList [("TOPIC", Equality "BASIC"),("PLATFORM", Index []),("PID",Any),("START",Any)] ) sample
    putStrLn "\nTOPIC BASIC, PLATFORM*,PAD ANY, STIRT ANY"
    putStrLn $ showResult $ c2Kernel ( Map.fromList [("TOPIC", Equality "BASIC"),("PLATFORM", Index []),("PAD",Any),("STIRT",Any)] ) sample
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
partition selector = fromList_ . mapMaybe (f . c2Kernel selector)
    where
    f (Just (Just i,s,_)) = Just (i,s)
    f _ = Nothing

-- this version of select is also permissive, but it does not  partition for subgraphs.
-- it does this by remving and Index constraint, and discarding all but the actual returned sample from the analysis
-- this has the desired result of not removing any potential Indexed header

-- TODO - integrate with Constraints.hs, rename select preselect
preSelect :: Selector-> [Sample] -> [Sample]
preSelect selector = map second . mapMaybe (c2Kernel (Map.filter (not . isIndex) selector))
    where
    second (_,s,_) = s

c2Kernel :: Selector -> Sample -> Maybe (Maybe Text,Sample,[(Text,Constraint)])
c2Kernel selector (headers,metrics) = let
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
