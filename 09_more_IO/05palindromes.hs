main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\a -> if isPal a then "Palindrome" else "Not palindrome") . lines

isPal :: String -> Bool
isPal a = a == reverse a
