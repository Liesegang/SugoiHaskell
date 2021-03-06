import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

    putStrLn "There are your TO-DO items."
    mapM_ putStrLn numberedTasks
    putStrLn "Wich one do you want to remove?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
