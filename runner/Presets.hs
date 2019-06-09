module Presets where

registry = "big:5000/"

frr = ( registry ++ "frr", "frr")
bird = (registry ++ "bird","bird")
hbgp = (registry ++ "hbgp","hbgp")
relay = (registry ++ "relay","relay")

presetPlatforms = [("bird",bird),("frr",frr),("hbgp",hbgp),("relay",relay)]

-- presetTopic :: (String, ([Integer], [Integer], Integer,[Integer])) 
-- where
-- presetTopic (name, (blockSizeRange, groupSizeRange, cycleCount,burstCountRange)) 
-- overall table size (in prefixes) is groupSize * blockSize * burstCount
-- overall table size (in paths) is blockSize * burstCount

presetTopics :: [(String, ([Integer], [Integer], Integer,[Integer]))]
presetTopics = [
                 ("ONESHOT", ( [1] , [1] , 1,[1]))
               , ("SMOKETEST", ( [1..2] , [1..2] , 2,[1]))
               , ("SIMPLE", ( [1,10,100] , [1,10,100] , 20,[1]))
               , ("BIG", ( [1000000,100000] , [1,10] , 10,[1]))
               , ("BASIC", ( [1..10] ++ [20,30..100] ++[200,300..1000] ++[2000,3000..10000] ++[20000,30000..100000] , [1,2,5,10] , 10,[1]))
               , ("EXPBS", ( expPlimited 10 1000000 , [1] , 20,[1]))
               , ("EXPBSLARGE", ( expPlimited 10 100000 , [10] , 20,[1]))
               , ("EXPBSVLARGE", ( expPlimited 10 10000 , [100] , 20,[1]))
               ]

expPlimited pow limit = takeWhile ( limit+1 > ) $ map floor $ go 1.0
    where go n = n : go (n * (10 ** (1/pow)))

