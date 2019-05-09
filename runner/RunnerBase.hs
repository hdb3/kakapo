module RunnerBase where
import System.Process
import System.Directory(createDirectoryIfMissing)
import Control.Concurrent(MVar,putMVar,forkFinally,newEmptyMVar,takeMVar)
import System.Exit
import System.IO(openTempFile,Handle,hIsEOF,hSetBuffering,BufferMode(NoBuffering),hClose,hFlush,stdout,hPutStr,openFile,IOMode(WriteMode))
import Data.ByteString(hGet,hPut)
import LogFileNames
import System.FilePath.Posix(takeFileName)


logRunner :: String -> [(String, String)] -> [String] -> String -> IO ExitCode
logRunner executable environment arguments stdin = do
    ((stdoutName,hStdout),(stderrName,hStderr)) <- getLogFiles (takeFileName executable)
    let show_ = map (\(a,b) -> a ++ "=\"" ++ b ++"\"") 
        cl = unwords $ show_ environment ++ [ executable ] ++ arguments ++ [ " < [" , stdin , "]" ]
    putStrLn $ " executing " ++ cl
    putStrLn $ " saving output in " ++ stdoutName ++ "(stdout) and " ++ stderrName ++ "(stderr)"
    tRunner (hStdout,hStderr) executable environment arguments stdin

stdoutRunner :: String -> [(String, String)] -> [String] -> String -> IO (Maybe String)
stdoutRunner executable environment arguments stdin = do
    createDirectoryIfMissing False "/dev/shm/kakapo"
    (stdoutName,hStdout) <- openTempFile "/dev/shm/kakapo" "stdout"
    (stderrName,hStderr) <- openTempFile "/dev/shm/kakapo" "stderr"
    let show_ = map (\(a,b) -> a ++ "=\"" ++ b ++"\"") 
        cl = unwords $ show_ environment ++ [ executable ] ++ arguments ++ [ " < [" , stdin , "]" ]
    putStrLn $ " executing " ++ cl
    putStrLn $ " writing output in " ++ stdoutName ++ " and " ++ stderrName
    exitCode <- tRunner (hStdout,hStderr) executable environment arguments stdin
    --exitCode <- qRunner (hStdout,hStderr) executable environment arguments stdin
    hClose hStdout
    if ExitSuccess == exitCode then
        Just <$> readFile stdoutName
    else
        return Nothing

qRunner :: (Handle,Handle) -> String -> [(String,String)] -> [String] -> String -> IO ExitCode
qRunner (hStdout,hStderr) executable environment arguments stdin = do
    (Just stdinHandle , Nothing, Nothing, ph) <- createProcess $ (proc executable arguments) { env = Just environment }
        { std_in = CreatePipe
        , std_out = UseHandle hStdout
        , std_err = UseHandle hStderr
        }
    hPutStr stdinHandle stdin
    hClose stdinHandle
    exitCode <- waitForProcess ph
    return exitCode

tRunner :: (Handle,Handle) -> String -> [(String,String)] -> [String] -> String -> IO ExitCode
tRunner (hStdout,hStderr) executable environment arguments stdin = do
    (Just stdinHandle , Just stdoutHandle, Just stderrHandle, ph) <- createProcess $ (proc executable arguments) { env = Just environment }
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    mvA <- forkIO' $ tee stdoutHandle hStdout
    mvB <- forkIO' $ tee stderrHandle hStderr
    hPutStr stdinHandle stdin
    hClose stdinHandle
    takeMVar mvA
    takeMVar mvB
    waitForProcess ph

    where

    tee :: Handle -> Handle -> IO()
    tee hIn hOut = do
        hSetBuffering hIn NoBuffering
        hSetBuffering hOut NoBuffering
        hSetBuffering stdout NoBuffering
        loop
        where
        loop = hIsEOF hIn >>= \p -> if p then return () else do
            b <- hGet hIn ( 1024 * 1024 )
            hPut stdout b >> hFlush stdout
            hPut hOut b >> hFlush hOut
            loop

    forkIO' :: IO () -> IO (MVar ())
    forkIO' io = do
        mvar <- newEmptyMVar
        forkFinally io (\_ -> putMVar mvar ())
        return mvar

getLogFiles logRootName = do
    logDir <- getLogDir
    let logPath = logDir ++ "/" ++ logRootName
    ext <- getNextFileBaseName logDir ( logRootName ++ ".stderr" )
    let stderrName = logPath ++ ".stderr" ++ ext 
        stdoutName = logPath ++ ".stdout" ++ ext 
    hStderr <- openFile stderrName WriteMode
    hStdout <- openFile stdoutName WriteMode
    return ((stdoutName,hStdout),(stderrName,hStderr))
