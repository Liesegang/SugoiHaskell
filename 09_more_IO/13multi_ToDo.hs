import System.Environment
import System.Directory
import System.IO
import Control.Exception
import Data.List

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesntExist command

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n"
add _ = putStrLn "Unknown format"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "Unknown format"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
    
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
    
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")
remove _ = putStrLn "Unknown format"

doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
    putStrLn $ "The " ++ command ++ " comand doesn't exist"

main = do
    args <- getArgs
    if args == []
        then putStrLn "Unknwon format"
        else do
            let (command:argList) = args
            dispatch command argList

