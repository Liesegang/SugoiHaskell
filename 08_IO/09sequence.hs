main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

    rs <- sequence [getLine, getLine, getLine]
    print rs

    sequence $ map print [1,2,3,4,5]
