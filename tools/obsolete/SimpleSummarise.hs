module Main where

main = do
   infile <- getContents
   let file = lines infile
       endline = last file
       startline = head file
       lineone = head $ tail file
       separatedlines = map fields file
       datalines = filter isValidData separatedlines
       isValidData line = (length line > 1) && ("DATA" == head line)
       output = unlines $ map ( concat . tail ) datalines
       selectedTextOutput = unlines $ tail $ map ( concat . drop 2 ) datalines
       selectedFloatOutput = unlines $ tail $ map ( concat . drop 2 ) datalines
       selectedFields = drop 1 $ map ( drop 2 ) datalines
       selectedFloats = map ( map getFloat ) selectedFields
       getFloat :: String -> Double
       getFloat = read
       columnSums = foldl ( zipWith (+)) [ 0.0, 0.0, 0.0 , 0.0 ]
       columnSquareSums = columnSums . map (map (^^ 2) )
       columnMeans records = let c = fromIntegral $ length records in map (/ c) $ columnSums records
       selectedSums = columnSums selectedFloats
       runningSumsAndSqSums = foldl f' (replicate 4 ( 0.0, 0.0))
           where f (s1,s2) x = (s1+x,s2+x^^2)
                 f' = zipWith f
       calcSDfromSumAndSqSum n (s1,s2) = sqrt ( n * s2 - s1^^2 ) / n
       calcMeanAndSDfromSumAndSqSum n (s1,s2) = ( s1 / n , sqrt ( n * s2 - s1^^2 ) / n)
       calcMeanAndRSDfromSumAndSqSum n (s1,s2) = ( s1 / n , sqrt ( n * s2 - s1^^2 ) / s1 )
       calcSD rx = map ( calcSDfromSumAndSqSum (fromIntegral $ length rx) ) ( runningSumsAndSqSums rx)
       calcMeanAndSD rx = map ( calcMeanAndSDfromSumAndSqSum (fromIntegral $ length rx) ) ( runningSumsAndSqSums rx)
       calcMeanAndRSD rx = map ( calcMeanAndRSDfromSumAndSqSum (fromIntegral $ length rx) ) ( runningSumsAndSqSums rx)
 
   putStrLn $ "header is \"" ++ startline ++ "\" type: " ++ head ( fields startline)
   putStrLn $ "trailer is \"" ++ endline ++ "\" type: " ++ head ( fields endline)
   putStrLn $ "line one is \"" ++ lineone ++ "\" type: " ++ head ( fields lineone)
   putStrLn output
   putStrLn selectedTextOutput
   print selectedFloats
   print selectedSums
   print $ columnMeans selectedFloats
   putStrLn ""
   print $ calcSD selectedFloats
   putStrLn ""
   print $ calcMeanAndSD selectedFloats
   putStrLn ""
   print $ calcMeanAndRSD selectedFloats

isComma c = ',' == c

fields :: String -> [String]
fields s = case dropWhile isComma s of
               "" -> []
               s' -> w : fields s'' where (w, s'') = break isComma s'
