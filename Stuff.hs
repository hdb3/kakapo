module Stuff where

delta :: (Num a) => [a] -> [a]
delta ax = delta' ax []
  where
    delta' [] _ = undefined
    delta' [_] acc = reverse acc
    delta' (a : b : ax) acc = delta' (b : ax) ((b - a) : acc)

readData :: FilePath -> IO [Int]
readData path = do
  let readInt t = read t :: Int
  raw <- readFile path
  return $ map readInt (lines raw)

mean :: (Integral a) => [a] -> a
mean [] = undefined
mean ax = sum ax `div` fromIntegral (length ax)

-- mmax :: (Integral a) => [a] -> a
-- mmax [] = undefined
-- mmax [a] = a
-- mmax (a : ax) = max a (mmax ax)

mf :: (t -> t -> t) -> [t] -> t
mf f [] = undefined
mf f [a] = a
mf f (a : ax) = f a (mf f ax)

mmin :: (Integral a) => [a] -> a
mmin = mf min

mmax :: (Integral a) => [a] -> a
mmax = mf max
