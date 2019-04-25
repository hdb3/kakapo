module Main where
import Runner

--main = sshLog "psaux" [ "localhost" ] "ps aux"
main = do
    let command = "ps aux"
    bash command
    ssh [ "localhost" ] command
    ssh [ "big" ] command
    ssh [ "dell0" ] command

    sshLog "psaux" [ "localhost" ] command
    sshLog "psaux" [ "big" ] command
    sshLog "psaux" [ "dell0" ] command
