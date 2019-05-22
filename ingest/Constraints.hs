{-# LANGUAGE OverloadedStrings #-}
module Constraints where
import Control.Arrow(first,second)
import Control.Monad(when)
import Data.Attoparsec.Text
import Data.Text(Text)
import qualified Data.Text as T
import Data.List(elem)
import Data.Maybe(isNothing)
import Control.Applicative((<|>))
import Data.Either(fromRight, partitionEithers)
import qualified Data.Map.Strict as Map
import System.Exit(die)
import GenParse(Metrics,Dict,Sample)

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
-- should be oblivious to the type of Metrics
showSelector :: Selector -> String
--showSelector = intercalate " , " . map showConstraint . Map.toList
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
--type SelectResult = Map.Map Text ( Map.Map Text [ Sample ] )
--type SelectResult = Map.Map Text [(Text,Sample)]

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

    g (control,sample) Nothing = Just $ Map.singleton control sample
    g (control,sample) (Just m) = Just $ Map.insertWith h control sample m
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
