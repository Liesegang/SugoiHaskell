luckey :: Int -> String
luckey 7 = "Oh, you're luckey!"
luckey n = "Sorry, you're out of luckey, pal!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe n = "It's not between 1 and 5"

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

charName :: Char -> String
charName 'A' = "Albert"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1+ y2)

first (x, _, _) = x
second (_, y, _) = y
third (_, _, z) = z

length' xs = sum [1 | _ <- xs]

head' :: [a] -> a
head' [] = error "Can't tell head onn ann empty list!!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "empty"
tell (x:[]) = "one: " ++ show x
tell (x:y:[]) = "two: " ++ show x ++ " : " ++ show y
tell (x:y:ys) = "many: " ++ show x ++ " : " ++ show y

firstLetter all@(a:_) = "The first letter of " ++ all ++ " is " ++ [a]

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're too thin"
    | bmi <= 25.0 = "You're normal"
    | bmi <= 30 = "You're little bit fat"
    | otherwise = "You're a whale, congratulation!"

bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
    | bmi <= 18.5 = "You're too thin"
    | bmi <= 25.0 = "You're normal"
    | bmi <= 30 = "You're little bit fat"
    | otherwise = "You're a whale, congratulation!"
    where bmi = weight / height ^ 2

initial :: String -> String -> String
initial firstname lastname = [f, '.', l]
    where (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + topArea

calcBmis2 :: [(Double, Double)] -> [(Double)]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^2]

describeList xs = "This list has " ++ 
    case xs of [] -> "No element"
               [x] -> "One element"
               [x, y] -> "Two elements"
               ls -> "Many elements"
