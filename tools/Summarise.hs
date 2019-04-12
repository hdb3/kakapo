module Main where

main = do
   infile <- getContents
   let file = lines infile
       endline = last file
       startline = head file
       lineone = head $ tail file
       separatedlines = map fields file
   putStrLn $ "header is " ++ startline ++ "type: " ++ (head $ fields startline)
   putStrLn $ "trailer is " ++ endline ++ "type: " ++ (head $ fields endline)
   putStrLn $ "line one is " ++ lineone ++ "type: " ++ (head $ fields lineone)

isComma c = ',' == c

fields :: String -> [String]
fields s = case dropWhile isComma s of
               "" -> []
               s' -> w : fields s'' where (w, s'') = break isComma s'
