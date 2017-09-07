mymin :: (Ord a) => a -> a -> a
mymin x y
    | x < y = x
    | otherwise = y

mymax :: (Ord a) => a -> a -> a
mymax x y
    | x > y = x
    | otherwise = y

myminimum :: (Ord a) => [a] -> a
myminimum [] = error "No minimum"
myminimum [x] = x
myminimum (x:xs) = mymin x $ minimum xs

mymaximum :: (Ord a) => [a] -> a
mymaximum [] = error "No maximum"
mymaximum [x] = x
mymaximum (x:xs) = mymax x $ mymaximum xs

myreplicate :: Int -> a -> [a]
myreplicate n _
    | n <= 0 = []
myreplicate n a = a : myreplicate (n-1) a

mytake :: Int -> [a] -> [a]
mytake n _
    | n <= 0 = []
mytake n (x:xs) = x : mytake (n-1) xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

myrepeat :: a -> [a]
myrepeat x = x : myrepeat x

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

myelem :: (Eq a) => a -> [a] -> Bool
myelem _ [] = False
myelem x (y:ys)
    | x == y = True
    | otherwise = myelem x ys

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort largerOrEqual
    where smaller = [y | y <- xs, y < x]
          largerOrEqual = [y | y <- xs, y >= x]


