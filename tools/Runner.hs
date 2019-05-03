module Runner where
import System.Process
import Control.Concurrent(forkIO)
import System.Exit
import System.IO(Handle,hIsEOF,hSetBuffering,BufferMode(NoBuffering),hClose,hFlush,stdout,stderr,hPutStr,hPutStrLn,openFile,IOMode(WriteMode))
import Data.ByteString(hGet,hPut)
import Control.Monad
import Data.Time.Clock.System (getSystemTime,SystemTime,systemSeconds,systemNanoseconds)
import Text.Printf
import LogFileNames

{-
    Runner is a repeat execution scheduler for kakapo
    It runs kakapo with a varying set of test parameters
    It monitors the exit status and ruuning time of each invocation
    It can repeat failed tests
    It will eventually give up if it cannot complete commands at all.
    It captures application output on stdout in a file for further processing.
    It relays stderr to ????

    It requires a command line generator which can produce as many instances of the required command as needed.

    Special attention is required for handling of environment varaiables because they are extensively used by kakapo to configure its behavioir:
    in order to enable seamless conversion for remote operation the System.Process environment control is not used.
    Instead, the command executed is a shell, which allows the shell to perform the required transfer of text into the environment.
    This faciltates simple conversion to a remote shell execution.

    Note: bash and ssh are not interchangeable
        - bash _requires_ command strings to be read from files, never from positional parameters
        - ssh _requires_ at least one positional parameter in order to suppress 'interactive' style 
          responses from the remote host
    However: by invoking ssh with the single command parameter "/bin/bash" it is possible to achive parity between 
          local and remote execution behaviour
-}

defaultSSHparams = [ "-t" , "-q" , "-o" , "UserKnownHostsFile=/dev/null" , "-o" , "StrictHostKeyChecking=no" ]
sshd params = ("/usr/bin/ssh" , defaultSSHparams ++ params ++ ["/bin/bash"])
bashd = ("/bin/bash" , [])
_stderr = return stderr
_devnull = openFile "/dev/null" WriteMode

bash :: String -> IO ()
bash = run_ _stderr bashd
bashQ = run_ _devnull bashd

ssh :: [String] -> String -> IO ()
ssh = run_ _stderr . sshd

sshLog :: String -> [String] -> String -> IO ()
sshLog n p c = do
    ( exitStatus , cmdName, stdoutName , stderrName ) <- sshLog_ n p c
    readFile cmdName >>= hPutStr stderr
    hPutStrLn stderr $ "output in " ++ stdoutName ++ " and " ++ stderrName

sshLog_ :: String -> [String] -> String -> IO ( Int , String, String , String )
sshLog_ logRootName params command = do
    logDir <- getLogDir
    let logPath = logDir ++ "/" ++ logRootName
    let (shell,parameters) = sshd params
    ext <- getNextFileBaseName logDir ( logRootName ++ ".cmd" )
    let cmdName = logPath ++ ".cmd" ++ ext 
        stderrName = logPath ++ ".stderr" ++ ext 
        stdoutName = logPath ++ ".stdout" ++ ext 
    cmdh <- openFile cmdName WriteMode
    hPutStrLn cmdh $ unwords ( shell : parameters)
    hPutStrLn cmdh command
    hStderr <- openFile stderrName WriteMode
    hStdout <- openFile stdoutName WriteMode
    now <- getSystemTime
    let cp = proc shell parameters
    (Just stdinHandle , Just stdoutHandle, Just stderrHandle, ph) <- createProcess $ cp
        { std_in = CreatePipe
        , std_out =  CreatePipe
        , std_err = CreatePipe
        }
    _ <- forkIO $ tee stdoutHandle hStdout
    _ <- forkIO $ tee stderrHandle hStderr
    hPutStr stdinHandle command
    hClose stdinHandle
    hPutStrLn stderr $ "remote shell started with: " ++ unwords ( shell : parameters)
    hPutStrLn stderr $ "remote shell command: " ++ command
    hFlush stderr
    code <- waitForProcess ph
    later <- getSystemTime
    let exitStatus = case code of
                         ExitSuccess -> 0
                         ExitFailure n -> n
    hPutStrLn cmdh $ "Complete, exit code " ++ show exitStatus ++ ", elapsed time " ++ prettyDuration (stToFloat later - stToFloat now)
    hClose cmdh
    return ( exitStatus , cmdName, stdoutName , stderrName )

sshQ = run_ _devnull . sshd

getBash :: String -> IO (Maybe String)
getBash  = getRun bashd

getSSH :: [String] -> String -> IO (Maybe String)
getSSH params = getRun (sshd params)

getRun (shell,parameters) command = do
    (code, out, err) <- readProcessWithExitCode shell parameters command
    unless ( ExitSuccess == code )
           ( do hPutStrLn System.IO.stderr $ "exit code=" ++ show code
                unless (null out)
                       ( hPutStrLn System.IO.stderr $ "stdout: \"" ++ out ++"...\"" )
                unless (null err)
                       ( hPutStrLn System.IO.stderr $ "stderr: \"" ++ err ++"...\"" )
           )
    return $ if ExitSuccess == code
             then Just out
             else Nothing

run_ _handle (shell,parameters) command = do
    handle <- _handle
    hPutStr handle $ "using " ++ shell ++ " to execute \"" ++ command ++ "\""
    hFlush handle
    now <- getSystemTime
    (code, out, err) <- readProcessWithExitCode shell parameters command
    later <- getSystemTime
    hPutStrLn handle $ " done, in " ++ prettyDuration (stToFloat later - stToFloat now)
    if ExitSuccess == code then
        unless (null out)
               ( hPutStrLn handle $ "stdout: \"" ++ take 1000 out ++"...\"" )
    else do
        hPutStrLn handle $ "exit code=" ++ show code
        unless (null out)
               ( hPutStrLn handle $ "stdout: \"" ++ take 1000 out ++"...\"" )
        unless (null err)
               ( hPutStrLn handle $ "stderr: \"" ++ take 1000 err ++"...\"" )

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
    loop = hIsEOF hIn >>= \p -> if p then return ()  else do
        b <- hGet hIn 1024
        hPut hOut b >> hFlush hOut
        hPut stdout b >> hFlush stdout
        loop
