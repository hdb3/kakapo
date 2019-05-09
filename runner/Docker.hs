module Docker where
import System.Process
import Control.Concurrent(forkIO)
import System.Exit
import System.IO(Handle,hIsEOF,hSetBuffering,BufferMode(NoBuffering),hClose,hFlush,stdout,stderr,hPutStr,hPutStrLn,openFile,IOMode(WriteMode))
import Data.ByteString(hGet,hPut)
import Control.Monad
import Data.Time.Clock.System (getSystemTime,SystemTime,systemSeconds,systemNanoseconds)
import Text.Printf
import LogFileNames
import System.FilePath.Posix(takeFileName)
import RunnerBase (stdoutRunner)

{-
    Docker is a replacement for Runner, using remote Docker sessions rather than ssh to execute remote actions
    Runner is a repeat execution scheduler for kakapo
    See the description of Runner for more information about this

    ### It runs kakapo with a varying set of test parameters
    ### It monitors the exit status and ruuning time of each invocation
    ### It can repeat failed tests
    ### It will eventually give up if it cannot complete commands at all.
    ### It captures application output on stdout in a file for further processing.
    ### It relays stderr to ????

    ### It requires a command line generator which can produce as many instances of the required command as needed.

    ### Special attention is required for handling of environment varaiables because they are extensively used by kakapo to configure its behavioir:
    ### in order to enable seamless conversion for remote operation the System.Process environment control is not used.
    ### Instead, the command executed is a shell, which allows the shell to perform the required transfer of text into the environment.
    ### This faciltates simple conversion to a remote shell execution.

    ### Note: bash and ssh are not interchangeable
        ### - bash _requires_ command strings to be read from files, never from positional parameters
        ### - ssh _requires_ at least one positional parameter in order to suppress 'interactive' style 
          ### responses from the remote host
    ### However: by invoking ssh with the single command parameter "/bin/bash" it is possible to achive parity between 
          ### local and remote execution behaviour
-}

{- docker wraps docker_:
   docker_ executes the command and returns the various results as Strings
   docker returns just IO(), writing its results to the console
-}

dockerCMD :: String -> [String] -> String -> IO ( Maybe String )
dockerCMD host = stdoutRunner "/usr/bin/docker" [("DOCKER_HOST",host)]

docker :: String -> [String] -> IO ()
docker host commands = void $ runner "/usr/bin/docker" [("DOCKER_HOST",host)] commands ""

runner :: String -> [(String,String)] -> [String] -> String -> IO Int
runner executable environment arguments stdin = do
    ( exitStatus , cmdName, stdoutName , stderrName ) <- runner_ executable environment arguments stdin
    if 0 == exitStatus then hPutStrLn stderr "complete" else hPutStrLn stderr $ "completed with errors, exit status = " ++ show exitStatus
    readFile cmdName >>= hPutStr stderr
    hPutStrLn stderr $ "output in " ++ stdoutName ++ " and " ++ stderrName
    return exitStatus

runner_ :: String -> [(String,String)] -> [String] -> String -> IO ( Int , String, String , String )
runner_ executable environment arguments stdin = do
    logDir <- getLogDir
    let logRootName = takeFileName executable
        logPath = logDir ++ "/" ++ logRootName
    ext <- getNextFileBaseName logDir ( logRootName ++ ".cmd" )
    let cmdName = logPath ++ ".cmd" ++ ext 
        stderrName = logPath ++ ".stderr" ++ ext 
        stdoutName = logPath ++ ".stdout" ++ ext 
    cmdh <- openFile cmdName WriteMode
    let show_ = map (\(a,b) -> a ++ "=\"" ++ b ++"\"") 
        cl = unwords $ show_ environment ++ [ executable ] ++ arguments
    --hPutStrLn cmdh $ unwords $ executable : (( show_ environment) ++ arguments)
    hPutStrLn cmdh cl
    hStderr <- openFile stderrName WriteMode
    hStdout <- openFile stdoutName WriteMode
    now <- getSystemTime
    let cp = proc executable arguments
        -- never allow the environment of the caller to leak into this context
        cp' = cp { env = Just environment }
    (Just stdinHandle , Just stdoutHandle, Just stderrHandle, ph) <- createProcess $ cp'
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    void $ forkIO $ tee stdoutHandle hStdout
    void $ forkIO $ tee stderrHandle hStderr
    hPutStr stdinHandle stdin
    hClose stdinHandle
    hPutStrLn stderr $ "process started with: " ++ cl
    unless (null stdin)
            ( hPutStrLn stderr $ "process input: " ++ stdin )
    hFlush stderr
    code <- waitForProcess ph
    later <- getSystemTime
    let exitStatus = case code of
                         ExitSuccess -> 0
                         ExitFailure n -> n
    hPutStrLn cmdh $ "Complete, exit code " ++ show exitStatus ++ ", elapsed time " ++ prettyDuration (stToFloat later - stToFloat now)
    hClose cmdh
    return ( exitStatus , cmdName, stdoutName , stderrName )

stToFloat :: SystemTime -> Double
stToFloat s = (fromIntegral (systemSeconds s) * 1000000000 + fromIntegral (systemNanoseconds s)) / 1000000000.0

prettyDuration dT = if 1.0 > dT then printf "%.3f ms" (1000*dT) else printf "%.3f s" dT

tee :: Handle -> Handle -> IO()
tee hIn hOut = do
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut NoBuffering
    hSetBuffering stdout NoBuffering
    loop
    where
    loop = hIsEOF hIn >>= \p -> if p then return () else do
        b <- hGet hIn ( 1024 * 1024 )
        hPut hOut b >> hFlush hOut
        hPut stdout b >> hFlush stdout
        loop
