import Control.Monad

main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

    input2 <- getLine
    if (input2 == "FUJI")
        then putStrLn input2
        else return ()
