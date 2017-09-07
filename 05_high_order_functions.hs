multiThree :: Int -> Int -> Int -> Int
multiThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

devideByTen :: (Floating a) => a -> a
devideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith _ [] _ = []
myzipWith _ _ [] = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys

myflip :: (a -> b -> c) -> (b -> a -> c)
myflip f = g
    where g x y = f y x

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs)
    | f x = x : myfilter f xs
    | otherwise = myfilter f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort largerEqual
    where smaller = myfilter (<x) xs
          largerEqual = myfilter (>=x) xs

largestDivisible :: Integer
largestDivisible = head $ filter f [100000,99999..]
    where f x = mod x 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (div n 2)
    | odd n = n : chain (3 * n + 1)

numLongChains :: Int
numLongChains = length $ filter (>15) $ map (length . chain) [1..100]

numLongChains2 :: Int
numLongChains2 = length $ filter (\xs -> length xs > 15) $ map chain [1..100]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

myflip' :: (a -> b -> c) -> (b -> a -> c)
myflip' = \f x y -> f y x

mysum :: (Num a) => [a] -> a
mysum xs = foldl (+) 0 xs

mymap2 :: (a -> b) -> [a] -> [b]
mymap2 f = foldr ((:) . f) []

badmap :: (a -> b) -> [a] -> [b]
badmap f = foldl (\acc x -> acc ++ [f x]) []

myelem :: (Eq a) => a -> [a] -> [Bool]
myelem y = scanr (\x acc -> if x == y then True else acc) False

mymaximum :: (Ord a) => [a] -> a
mymaximum = foldl1 max

myreverse :: [a] -> [a]
myreverse = foldl (\acc x -> x:acc) []

myproduct :: (Num a) => [a] -> a
myproduct = foldr (*) 1

myfilter2 :: (a -> Bool) -> [a] -> [a]
myfilter2 f = foldr (\x acc -> if f x then x : acc else acc) []

mylast :: [a] -> a
mylast = foldl1 (\_ x -> x)

myand :: [Bool] -> Bool
myand = foldr (&&) True

myor :: [Bool] -> Bool
myor = foldr (||) False

sqrtSum :: Int
sqrtSum = 1 + (length . takeWhile (<1000) . scanl1 (+) $ map sqrt [1..])

mysum2 :: (Num a) => [a] -> a
mysum2 = foldl (+) 0

fn :: Double -> Integer
fn = ceiling . negate . tan . cos . max 50

oddSquareSum :: Int
oddSquareSum = sum . takeWhile (<10000) . filter odd $  map (^2) [1..]

