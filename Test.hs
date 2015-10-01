module Test where

-- 2.1.7, 2.1.9
import Data.Function
-- 1.4.9
import Data.Char 

-- 1.2.7
--Реализуйте функцию трех аргументов lenVec3, которая вычисляет длину трехмерного вектора. 
-- Аргументы функции задают декартовы координаты конца вектора, его начало подразумевается находящимся в начале координат. 
lenVec3 x y z =  sqrt (x^2 + y^2 + z^2)

-- 1.2.10
-- Напишите реализацию функции sign, которая возвращает 1, если ей передано положительное число, (-1), если отрицательное, и 0 в случае, когда передан 0.
sign x = if (x > 0) then 1 else if (x < 0) then -1 else 0

-- 1.3.8
-- Реализуйте оператор |-|, который возвращает модуль разности переданных ему аргументов:
x |-| y = abs (x - y)

-- 1.4.6
-- Вспомним функцию discount, которая возвращала итоговую сумму покупки с возможной скидкой. 
-- В качестве параметров ей передавались сумма без скидки sum, процент скидки proc, причем скидка начислялась, если переданная сумма превышает порог limit. 
-- discount :: Double -> Double -> Double -> Double
-- discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
-- Запишите тип функции standardDiscount, определенной как частичное применение функции discount:
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

-- 1.4.9
-- Реализуйте функцию twoDigits2Int, которая принимает два символа и возвращает число, 
-- составленное из этих символов, если оба символа числовые, и 100 в противном случае. 
-- (Первый символ рассматривается как количество десятков, второй — единиц.)
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y) then digitToInt x*10 + digitToInt y else 100

-- 1.4.11
-- Будем задавать точки на плоскости парами типа (Double, Double). 
-- Реализуйте функцию dist, которая возвращает расстояние между двумя точками, передаваемыми ей в качестве аргументов.
dist :: (Double, Double) -> (Double, Double) -> Double
dist x y = sqrt ((fst y - fst x)^2 + (snd y - snd x)^2)

-- 1.5.4
-- Определите функцию, вычисляющую двойной факториал, то есть произведение натуральных чисел, не превосходящих заданного числа и имеющих ту же четность. 
-- Например: 7!!=7*5*3*1,  8!!=8*6*4*2. Предполагается, что аргумент функции могут принимать только неотрицательные значения.
doubleFact :: Integer -> Integer
doubleFact n = if (n == 1) then 1 else if (n == 2) then 2 else n * doubleFact (n-2)

-- 1.5.8
-- Последовательность чисел Фибоначчи 0,1,1,2,3,5,8,13,21,… легко определить рекурсивно, задав два первых терминирующих значения 
-- и определив любое последующее как сумму двух непосредственно предыдущих: F0=0 F1=1 Fn=Fn−1+Fn−2 
-- На Haskell данное определение задаётся следующей функцией:
-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-- Эта функция определена лишь для неотрицательных чисел. 
-- Однако, из данного выше определения можно вывести формулу для вычисления чисел Фибоначчи при отрицательных индексах, при этом последовательность будет следующей:
-- F−1=1,F−2=−1,…,F−10=−55,… 
-- Измените определение функции fibonacci так, чтобы она была определена для всех целых чисел и порождала при отрицательных аргументах указанную последовательность.
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = (-1)^(-n+1) * fibonacci(-n)

-- 1.5.10
-- Реализация функции для вычисления числа Фибоначчи, основанная на прямом рекурсивном определении, крайне неэффективна - 
-- количество вызовов функции растет экспоненциально с ростом значения аргумента. 
-- С помощью механизма аккумуляторов попробуйте написать более эффективную реализацию, имеющую линейную сложность. 
-- Как и в предыдущем задании, функция должна быть определена для всех целых чисел.            
fibonacci' :: Integer -> Integer
fibonacci' n  | n == 0 = 0
	          | n == 1 = 1
	          | n >= 0 = f' 1 1 n 
	          | n < 0 = (-1)^(-n+1) * fibonacci'(-n)

f' :: Integer -> Integer -> Integer -> Integer 
f' acc acc' i | i == 2 = acc
			  | i > 2 = f' (acc + acc') acc (i - 1) 
-- Вариант Кирилла более логичен, надо над этим подумать
{-fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0 
           | n == 1 = 1
           | n == -1 = 1
           | n > 0 = helper 0 1 n
           | n < 0 = (helper 0 1 (abs n)) * (-1)^((-n)-1)

helper :: Integer -> Integer -> Integer -> Integer 
helper prev cur n | n == 1 = cur
                 | otherwise = helper cur (prev+cur) (n-1) -}
			  

-- 1.6.6
-- Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
-- a0=1;a1=2;a2=3;ak+3=ak+2+ak+1−2ak.
-- Попытайтесь найти эффективное решение.
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = s' 3 3 2 1 n

s' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 
s' i a b c n | (i < n) = s' (i + 1) res a b n
			 | (i == n) = res 
			 where res = a + b - 2*c

-- 1.6.8
-- Реализуйте функцию, находящую сумму и количество цифр заданного целого числа.
-- sum'n'count :: Integer -> (Integer, Integer)
-- GHCi> sum'n'count (-39)
-- (12,2)		
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
			  | otherwise = (sum n, toInteger (length n)) 
  				where n = digs x

digs :: Integer -> [Integer]
digs 0 = []
digs i | i < 0 = digs (-i) 
	   | otherwise = digs (i `div` 10) ++ [i `mod` 10]	

-- 1.6.9
-- Реализуйте функцию, находящую значение определённого интеграла от заданной функции f на заданном интервале [a,b] методом трапеций. 
-- (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
-- Результат может отличаться от -2.0, но не более чем на 1e-4.	    	  
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h*( (f a + f b)/2 + sum) where 
	h = (b - a)/1000
	sum = s' 0 f 1
	s' acc f n | n == 1000 = acc
		  	   | otherwise = s' (acc + f (a + n*h)) f (n + 1) 
-- Решение Кирилла, по моему, более логично
intg f a b = let h = (b-a)/1000
			 in h * ((f a + f b)/2 + helper 0 f a h 1)
helper s f a h n | n == 1000 = s
				 | otherwise = helper (s + f (a + n * h)) f a h (n + 1)

-- 2.1.3		  		 
-- Напишите функцию трех аргументов getSecondFrom, полиморфную по каждому из них, 
-- которая полностью игнорирует первый и третий аргумент, а возвращает второй. Укажите ее тип.
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom x y z = y

-- 2.1.7
-- В модуле Data.Function определена полезная функция высшего порядка
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on op f x y = f x `op` f y
-- Она принимает четыре аргумента: бинарный оператор с однотипными аргументами (типа b), функцию f :: a -> b, 
-- возвращающую значение типа b, и два значения типа a. Функция on применяет f дважды к двум значениям типа a и передает результат в бинарный оператор.
-- Используя on можно, например, записать функцию суммирования квадратов аргументов так:
-- sumSquares = (+) `on` (^2)
-- Функция multSecond, перемножающая вторые элементы пар, реализована следующим образом
-- multSecond = g `on` h
-- Напишите реализацию функций g и h.
{-multSecond :: Num b => (a,b) -> (a,b) -> b
multSecond = g `on` h
g :: Num b => b -> b  -> b
g x y = x * y 
h :: (a,b) -> b
h p = snd p -}

-- 2.1.9
-- Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую в качестве первого аргумента трехместную функцию:
-- on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
-- Например, сумма квадратов трех чисел может быть записана с использованием on3 так
-- GHCi> let sum3squares = (\x y z -> x+y+z) `on3` (^2)
-- GHCi> sum3squares 1 2 3
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z) 

-- 2.2.3
-- Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42, 
-- затем возводит результат выбора в куб и, наконец, вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована в виде:
-- doItYourself = f . g . h
-- Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.
doItYourself = f . g . h
h = (max 42)
g = (^3)
f = (logBase 2)

-- 2.3.7
-- Реализуйте класс типов Printable, предоставляющий один метод toString - функцию одной переменной, 
-- которая преобразует значение типа, являющегося представителем Printable, в строковое представление.
-- Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение:
-- GHCi> toString True
-- "true"
-- GHCi> toString False
-- "false"
-- GHCi> toString ()
-- "unit type"
class Printable a where
	toString :: a -> [Char]

instance Printable Bool where
	toString True = "true"
	toString False = "false"

instance Printable () where
	toString () = "unit type"

-- 2.3.9
-- Сделайте тип пары представителем класса типов Printable, реализованного вами в предыдущей задаче, обеспечив следующее поведение:
-- GHCi> toString (False,())
-- "(false,unit type)"
-- GHCi> toString (True,False)
-- "(true,false)"
instance (Printable a, Printable b) => Printable (a, b) where
	toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")" 

-- 2.4.3
-- Пусть существуют два класса типов KnownToGork и KnownToMork, которые предоставляют 
-- методы stomp (stab) и doesEnrageGork (doesEnrageMork) соответственно:
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool
class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
-- Класса типов KnownToGorkAndMork является расширением обоих этих классов, выставляя дополнительно метод stompOrStab:
-- class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
--    stompOrStab :: a -> a
-- Задайте реализацию по умолчанию метода stompOrStab, которая вызывает метод stomp, 
-- если переданное ему значение приводит в ярость Морка, вызывает stab, 
-- если оно приводит в ярость Горка и вызывает сначала stab, а потом stomp, 
-- если оно приводит в ярость их обоих. 
-- Если не происходит ничего из вышеперечисленного, метод должен возвращать переданный ему аргумент.
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab smth  = if doesEnrageMork smth && not (doesEnrageGork smth)
    						then stomp smth
    				   		else if doesEnrageGork smth && not (doesEnrageMork smth)
    				   			then stab smth
    				   			else if doesEnrageMork smth && doesEnrageGork smth 
    				   				then  stomp $ stab smth
    				   				else smth

-- 2.4.5
-- Имея функцию ip = show a ++ show b ++ show c ++ show d определите значения a, b, c, d так, чтобы добиться следующего поведения:
-- GHCi> ip
-- "127.224.120.12"
{-class (Int x, Show x) => MyInt x where
	show :: x -> [Char]
	show x = "."-}
--a, b, c :: MyInt 
data MyIp = MyIp Int
instance Show MyIp where
  show (MyIp a) = show a ++ "."
a = MyIp 127
b = MyIp 224
c = MyIp 120
d = 12
ip = show a ++ show b ++ show c ++ show d