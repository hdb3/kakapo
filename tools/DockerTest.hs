module Main where
import Docker

main = do
    docker "localhost" ["info"]
    docker "big" ["info"]
    docker "docker01" ["info"]
    docker "docker02" ["info"]
