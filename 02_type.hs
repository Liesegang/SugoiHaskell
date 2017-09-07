removeNonUpperCase :: String -> String
removeNonUpperCase xs = [c | c <- xs, elem c ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product[1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
