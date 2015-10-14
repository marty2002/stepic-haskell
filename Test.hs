module Test where

-- 2.1.7, 2.1.9
import Data.Function
-- 1.4.9
import Data.Char 
{--
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
data MyIp = MyIp Int
instance Show MyIp where
  show (MyIp a) = show a ++ "."
a = MyIp 127
b = MyIp 224
c = MyIp 120
d = 12
ip = show a ++ show b ++ show c ++ show d

-- 2.4.7
-- Реализуйте класс типов
-- class SafeEnum a where
--  ssucc :: a -> a
 -- spred :: a -> a
-- обе функции которого ведут себя как succ и pred стандартного класса Enum, однако являются тотальными, 
-- то есть не останавливаются с ошибкой на наибольшем и наименьшем значениях типа-перечисления соответственно, 
-- а обеспечивают циклическое поведение. Ваш класс должен быть расширением ряда классов типов стандартной библиотеки, 
-- так чтобы можно было написать реализацию по умолчанию его методов, позволяющую объявлять его представителей без необходимости писать какой бы то ни было код. 
-- Например, для типа Bool должно быть достаточно написать строку
-- instance SafeEnum Bool
-- и получить возможность вызывать
-- GHCi> ssucc False
-- True
-- GHCi> ssucc True
-- False

class (Ord t, Enum t, Bounded t) => SafeEnum t where
  ssucc :: t -> t
  ssucc x = if x < maxBound then succ x
  			else minBound 

  spred :: t -> t
  spred x = if x > minBound then pred x
  			else maxBound

instance SafeEnum Bool where 

-- 2.4.9
-- Напишите функцию с сигнатурой типа
-- avg :: Int -> Int -> Int -> Double
-- вычисляющей среднее значение переданных в нее аргументов:
-- GHCi> avg 3 4 5
-- 4.0	
avg :: Int -> Int -> Int -> Double
avg x y z = fromIntegral (x + y + z) / 3

-- 3.1.2
-- Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.
-- GHCi> addTwoElements 2 12 [85,0,6]
-- [2,12,85,0,6]
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y z = x : y : z 

-- 3.1.3
-- Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента. 
-- Количество повторов определяется значением второго аргумента этой функции.
-- GHCi> nTimes 42 3
-- [42,42,42]
-- GHCi> nTimes 'z' 5
-- "zzzzz"
nTimes:: a -> Int -> [a]
nTimes x n = nTimes' x n []
nTimes' :: a -> Int -> [a] -> [a]
nTimes' x n acc | n == 0 = acc
                | otherwise = nTimes' x (n-1) (x : acc)

-- 3.1.8
-- Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.
-- GHCi> oddsOnly [2,5,7,10,11,12]
-- [5,7,11]
-- Для анализа четности можно использовать функции odd и even стандартной библиотеки.
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) = if odd x then x : oddsOnly xs 
                    else oddsOnly xs


-- 3.1.10
-- Реализуйте функцию isPalindrome, которая определеяет, является ли переданный ей список палиндромом.
-- GHCi> isPalindrome []
-- True
-- GHCi> isPalindrome [1]
-- True
-- GHCi> isPalindrome [1, 2]
-- False
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 3.1.12
-- Составьте список сумм соответствующих элементов трех заданных списков. 
-- Длина результирующего списка должна быть равна длине самого длинного из заданных списков, 
-- при этом "закончившиеся" списки не должны давать вклада в суммы.
-- GHCi> sum3 [1,2,3] [4,5] [6]
-- [11,7,3]
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 a [] [] = a
sum3 [] b [] = b
sum3 [] [] c = c
sum3 [] (b : bs) (c : cs) = (b + c) : sum2 bs cs
sum3 (a : as) [] (c : cs) = (a + c) : sum2 as cs
sum3 (a : as) (b : bs) [] = (a + b) : sum2 as bs
sum3 (a : as) (b : bs) (c : cs) = (a + b + c) : sum3 as bs cs

sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] []  = []
sum2 a [] = a
sum2 [] b = b
sum2 (a : as) (b : bs) = (a + b) : sum2 as bs


-- Решение Кирилла, подумаю об этом позже
{-sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] [] (z:zs) = z : sum3 [] [] zs
sum3 [] (y:ys) [] = y : sum3 [] ys []
sum3 [] (y:ys) (z:zs) = (y+z) : sum3 [] ys zs

sum3 (x:xs) [] [] = x : sum3 xs [] []
sum3 (x:xs) [] (z:zs) = (x+z) : sum3 xs [] zs
sum3 (x:xs) (y:ys) [] = (x+y) : sum3 xs ys []
sum3 (x:xs) (y:ys) (z:zs) = (x+y+z) : sum3 xs ys zs -}

-- 3.1.13
-- Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) 
-- и возвращает список таких групп.
-- GHCi> groupElems []
-- []
-- GHCi> groupElems [1,2]
-- [[1],[2]]
-- GHCi> groupElems [1,2,2,4]
-- [[1],[2,2],[4]]
-- GHCi> groupElems [1,2,3,2,4]
-- [[1],[2],[3],[2],[4]]
-- Разрешается использовать только функции, доступные из библиотеки Prelude.
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems a  =  gr a []
  where gr [] as = reverse as
        gr (x : xs) [] = gr xs ((x : []) : [])
        gr (x : xs) ys = if (x == headOfHead ys) then gr xs ((x : (head ys)) : (tail ys)) 
                         else gr xs ((x : []) : ys)

headOfHead = head . head

-- 3.2.3
-- Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
-- Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
--- GHCi> readDigits "365ads"
-- ("365","ads")
-- GHCi> readDigits "365"
-- ("365","")
-- В решении вам поможет функция isDigit из модуля Data.Char.
readDigits :: String -> (String, String)
readDigits xs = span isDigit xs

-- 3.2.4
-- Реализуйте функцию filterDisj, принимающую два унарных предиката и список, 
-- и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.
-- GHCi> filterDisj (< 10) odd [7,8,10,11,12]
-- [7,8,11]
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x : xs) = if (p1 x || p2 x) then x : filterDisj p1 p2 xs
                            else filterDisj p1 p2 xs

-- 3.2.5
-- Напишите реализацию функции qsort. 
-- Функция qsort должная принимать на вход список элементов и сортировать его в порядке возрастания 
-- c помощью сортировки Хоара: для какого-то элемента x изначального списка (обычно выбирают первый) 
-- делить список на элементы меньше и не меньше x, и потом запускаться рекурсивно на обеих частях.
-- GHCi> qsort [1,3,2,5]
-- [1,2,3,5]
-- Разрешается использовать только функции, доступные из библиотеки Prelude.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort l =  qsort' (filter (< h) l) (filter (>= h) l)
            where h = head l

qsort' :: Ord a => [a] -> [a] -> [a]
qsort' [] (x : xs) = qsort' [x] xs
qsort' fs ss = qsort fs ++ qsort ss

-- 3.2.7 
-- Напишите функцию squares'n'cubes, принимающую список чисел, 
-- и возвращающую список квадратов и кубов элементов исходного списка.
-- GHCi> squares'n'cubes [3,4,5]
-- [9,27,16,64,25,125]
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes a = concat map (^2) a ++ map (^3) a--}

-- 3.2.10
-- Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре. 
-- Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.
-- GHCi> delAllUpper "Abc IS not ABC"
-- "Abc not"
-- Постарайтесь реализовать эту функцию как цепочку композиций, аналогично revWords из предыдущего видео.
delAllUpper :: String -> String
delAllUpper str =  unwords (filter (any isLower) str1)
  where str1 = words str