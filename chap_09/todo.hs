{-# OPTIONS -Wall -Werror #-}

import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesntExists command

doesntExists :: String -> [String] -> IO ()
doesntExists command _ = putStrLn $ "The " ++ command ++ " command dosen't exist"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn $ "The add command takes exactly filename and task"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show (n:: Integer) ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn $ "The view command takes exactly filename"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show (n:: Integer) ++ " - " ++ line) [0..] todoTasks
    putStrLn "There are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _ = putStrLn $ "The remove command takes exactly filename and task number"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        bumpTodoItem = todoTasks !! number
        newTodoTasks = bumpTodoItem : (delete bumpTodoItem todoTasks)
        numberedTasks = zipWith (\n line -> show (n:: Integer) ++ " - " ++ line) [0..] newTodoTasks
        newTodoItems = unlines newTodoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
    putStrLn "Bumped TO-DO items:"
    mapM_ putStrLn numberedTasks
bump _ = putStrLn $ "The bump command takes exactly filename and task number"

main :: IO ()
main = do
    (command:argList) <- getArgs
    dispatch command argList
