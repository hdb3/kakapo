module Runner where
import System.Process
import System.Exit
import System.IO(hFlush,stdout)
import Control.Monad
import qualified Data.Time.Clock.System as DT
import Data.Time.Clock.System (getSystemTime,SystemTime,systemSeconds,systemNanoseconds)
import Text.Printf

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

bash = run ("/bin/bash" , [])
ssh host = run ("/usr/bin/ssh" , [host, "/bin/bash"]) 
run (shell,parameters) command = do
    putStr $ "using " ++ show (shell,parameters) ++ " to execute \"" ++ command ++ "\""
    hFlush stdout
    now <- getSystemTime
    (code, stdout, stderr) <- readProcessWithExitCode shell parameters command
    later <- getSystemTime
    putStrLn $ " done, in " ++ prettyDuration (stToFloat later - stToFloat now)
    if ExitSuccess == code then
        unless (null stdout)
               ( putStrLn $ "stdout: \"" ++ take 100 stdout ++"...\"" )
    else do
        putStrLn $ "exit code=" ++ show code
        unless (null stdout)
               ( putStrLn $ "stdout: \"" ++ take 100 stdout ++"...\"" )
        unless (null stderr)
               ( putStrLn $ "stderr: \"" ++ take 100 stderr ++"...\"" )

stToFloat :: SystemTime -> Double
stToFloat s = (fromIntegral (systemSeconds s) * 1000000000 + fromIntegral (systemNanoseconds s)) / 1000000000.0

prettyDuration dT = if 1.0 > dT then printf "%.3f ms" (1000*dT) else printf "%.3f s" dT
