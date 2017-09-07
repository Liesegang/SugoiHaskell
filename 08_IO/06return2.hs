main = do
    a <- return "hey"
    b <- return "say"
    putStrLn $ a ++ " " ++ b ++ " jump!"
