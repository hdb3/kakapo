module Runner where
import System.Process
import System.Exit
import System.IO(hClose,stdout,hFlush,stderr,hPutStr,hPutStrLn,openFile,IOMode(WriteMode))
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

sshLog :: String -> [String] -> String -> IO ( Int , String , String )
sshLog logRootName params command = do
    let (shell,parameters) = sshd params
    ext <- getNextFileBaseName "." ( logRootName ++ ".cmd" )
    let cmdName = logRootName ++ ".cmd" ++ ext 
        stderrName = logRootName ++ ".stderr" ++ ext 
        stdoutName = logRootName ++ ".stdout" ++ ext 
    cmdh <- openFile cmdName WriteMode
    hPutStrLn cmdh $ unwords ( shell : parameters)
    hPutStrLn cmdh command
    hStderr <- openFile stderrName WriteMode
    hStdout <- openFile stdoutName WriteMode
    now <- getSystemTime
    let cp = proc shell parameters
    (Just stdinHandle , _, _, ph) <- createProcess $ cp
        { std_in = CreatePipe
        , std_out = UseHandle hStdout
        , std_err = UseHandle hStderr
        }
    hPutStr stdinHandle command
    hClose stdinHandle
    code <- waitForProcess ph
    later <- getSystemTime
    let exitStatus = case code of
                         ExitSuccess -> 0
                         ExitFailure n -> n
    hPutStrLn cmdh $ "Complete, exit code " ++ show exitStatus ++ ", elapsed time " ++ prettyDuration (stToFloat later - stToFloat now)
    hClose cmdh
    return ( exitStatus , stdoutName , stderrName )

sshQ = run_ _devnull . sshd

getBash :: String -> IO (Maybe String)
getBash  = getRun bashd

getSSH :: [String] -> String -> IO (Maybe String)
getSSH params = getRun (sshd params)

getRun (shell,parameters) command = do
    (code, stdout, stderr) <- readProcessWithExitCode shell parameters command
    unless ( ExitSuccess == code )
           ( do hPutStrLn System.IO.stderr $ "exit code=" ++ show code
                unless (null stdout)
                       ( hPutStrLn System.IO.stderr $ "stdout: \"" ++ stdout ++"...\"" )
                unless (null stderr)
                       ( hPutStrLn System.IO.stderr $ "stderr: \"" ++ stderr ++"...\"" )
           )
    return $ if ExitSuccess == code
             then Just stdout
             else Nothing

run_ _handle (shell,parameters) command = do
    handle <- _handle
    hPutStr handle $ "using " ++ shell ++ " to execute \"" ++ command ++ "\""
    hFlush handle
    now <- getSystemTime
    (code, stdout, stderr) <- readProcessWithExitCode shell parameters command
    later <- getSystemTime
    hPutStrLn handle $ " done, in " ++ prettyDuration (stToFloat later - stToFloat now)
    if ExitSuccess == code then
        unless (null stdout)
               ( hPutStrLn handle $ "stdout: \"" ++ take 1000 stdout ++"...\"" )
    else do
        hPutStrLn handle $ "exit code=" ++ show code
        unless (null stdout)
               ( hPutStrLn handle $ "stdout: \"" ++ take 1000 stdout ++"...\"" )
        unless (null stderr)
               ( hPutStrLn handle $ "stderr: \"" ++ take 1000 stderr ++"...\"" )

stToFloat :: SystemTime -> Double
stToFloat s = (fromIntegral (systemSeconds s) * 1000000000 + fromIntegral (systemNanoseconds s)) / 1000000000.0

prettyDuration dT = if 1.0 > dT then printf "%.3f ms" (1000*dT) else printf "%.3f s" dT
