module Main where

import System.IO(hPutStr,stderr,hClose,hPutStrLn,openFile,IOMode(WriteMode))
import Control.Exception
import Sections(emptySection,Section,getSection)
import Control.DeepSeq

{- various versions of execution exception catchers
   the simplets one catces just one class of error - choose yoir specific error handler to select which

   the forcing of eceptions in pure code is somewhat magical, however the below code using 'evaluate $ force'
   does the trick for my test cases.   The alternative kludge is to force IO over the entire structure by wrint a representtaion to a file.

   It is clear that this code is much less generic than it could be - it should be possible to remove the explicit return value
   and create a simple wrapper over a value, like Maybe, and that is what I should like to do next.  The magic over and above the content of Control.Exception documentation
   and example is the combination of the evaluate/force and wrappers....
-}

main :: IO ()
main = do
    res <- catchAssertFail parse'
    -- res <- catchErrorFail parse'
    -- res <- catchAll parse'
    print res

main'' = do
    res <- catchAll parse'
    print res

main' = do
    res <- wrap parse''
    print res

wrap :: (IO Section) -> IO (Either ErrorCall Section)
wrap f = try f

parse'' :: IO Section
parse'' = do
    infile <- getContents
    let section = getSection infile
    h <- openFile "/dev/null" WriteMode
    hPutStrLn h $ show section
    return section

parse' :: IO Section
parse' = do
    infile <- getContents
    section <- evaluate $ force $ getSection infile
    return section

catchErrorFail :: IO Section -> IO Section
catchErrorFail f = catch f errorFail

catchAssertFail :: IO Section -> IO Section
catchAssertFail f = catch f assertFail

catchPatternFail :: IO Section -> IO Section
catchPatternFail f = catch f patternMatchFail

catchAll f = 
    catches f
            [ Handler errorFail
            , Handler assertFail
            , Handler patternMatchFail
            ]

errorFail :: ErrorCall -> IO Section
errorFail e = do
    let msgs = lines $ show e
    hPutStrLn stderr $ "errorFail, description: " ++ msgs !! 0 ++ ", context: " ++ msgs !! 2
    return emptySection
    

assertFail :: AssertionFailed -> IO Section
assertFail e = do
    let msgs = lines $ show e
    hPutStrLn stderr $ "assertFail, context: " ++ msgs !! 2
    return emptySection

patternMatchFail :: PatternMatchFail -> IO Section
patternMatchFail e = do
    let msgs = lines $ show e
    hPutStrLn stderr $ "*** THIS MESSGAE MAY NEED FIXING!!!! patternMatchFail, description: " ++ msgs !! 0 ++ ", context: " ++ msgs !! 2
    return emptySection
