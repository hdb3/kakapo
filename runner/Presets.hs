module Presets where

registry = "big:5000/"

frr = ( registry ++ "frr", "frr")
bird = (registry ++ "bird","bird")
hbgp = (registry ++ "hbgp","hbgp")
relay = (registry ++ "relay","relay")

presetPlatforms = [("bird",bird),("frr",frr),("hbgp",hbgp),("relay",relay)]

presetTopics = [
                 ("SMOKETEST", ( [1..2] , [1..2] , 2))
               , ("SIMPLE", ( [1,10,100] , [1,10,100] , 50))
               , ("BASIC", ( [1..10] ++ [20,30..100] ++[200,300..1000] ++[2000,3000..10000] ++[20000,30000..100000] , [1,2,5,10] , 10))
               , ("EXPBS", ( expPlimited 10 1000000 , [1] , 20))
               , ("EXPBSLARGE", ( expPlimited 10 100000 , [10] , 20))
               , ("EXPBSVLARGE", ( expPlimited 10 10000 , [100] , 20))
               ]

expPlimited pow limit = takeWhile ( limit+1 > ) $ map floor $ go 1.0
    where go n = n : go (n * (10 ** (1/pow)))

