module Test where

import Data.Function

multSecond :: Num b => (a,b) -> (a,b) -> b
multSecond = g `on` h
g :: Num b => b -> b  -> b
g x y = * 
h :: (a,b) -> b
h p = snd 

-- Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую в качестве первого аргумента трехместную функцию
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z) 
