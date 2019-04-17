module Main where
import Runner

main = do
    let command = "find ~"
    bash command
    ssh "localhost" command
    ssh "big" command
    ssh "dell0" command
