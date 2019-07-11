module Presets where

registry = "localrepo:5000/"

presetPlatforms = ["bgpd","bird2","bird","frr","hbgp","relay","hbgpdev"]

-- presetTopic :: (String, ([Integer], [Integer], Integer,[Integer])) 
-- where
-- presetTopic (name, (blockSizeRange, groupSizeRange, cycleCount,burstCountRange)) 
-- overall table size (in prefixes) is groupSize * blockSize * burstCount
-- overall table size (in paths) is blockSize * burstCount

presetTopics :: [(String, ([Integer], [Integer], Integer,[Integer]))]
presetTopics = [
                 ("ONESHOT", ( [10] , [1] , 11,[1]))
               , ("TENSHOT", ( [10] , [10] , 11,[10]))
               , ("BIGSHOT", ( [1000] , [10] , 11,[100]))
               , ("LONGSHOT", ( [1000] , [10] , 1001,[100]))
               , ("SOAK", ( [100,200..] , [1] , 21,[10]))
               , ("SIMPLE", ( [10] , [1] , 11,[10,20]))
               , ("SKIMSHORT", ( [100] , [1] , 11,[10,20..100]))
               , ("FULLSHORT", ( [100] , [1] , 51,[1..100]))
               , ("SKIM", ( [1000] , [1] , 11,[100,200..1000]))
               , ("FULL", ( [1000] , [1] , 51,[10,20..1000]))
               , ("BASE1M", ( [20000] , [5] , 21,[1..10]))
               , ("BASE1Mng", ( [20000] , [5] , 21,[1..10]))
               , ("BIG", ( [1000000,100000] , [1,10] , 10,[1]))
               , ("BASIC", ( [1..10] ++ [20,30..100] ++[200,300..1000] ++[2000,3000..10000] ++[20000,30000..100000] , [1,2,5,10] , 10,[1]))
               , ("EXPBS", ( expPlimited 10 1000000 , [1] , 20,[1]))
               , ("EXPBSLARGE", ( expPlimited 10 100000 , [10] , 20,[1]))
               , ("EXPBSVLARGE", ( expPlimited 10 10000 , [100] , 20,[1]))
               ]

oldPresetTopics :: [(String, ([Integer], [Integer], Integer,[Integer]))]
oldPresetTopics = [
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

