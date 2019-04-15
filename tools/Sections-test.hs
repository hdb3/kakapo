module Main where

import Sections(getSection)

main :: IO ()
main = do
   infile <- getContents
   print $ getSection infile
