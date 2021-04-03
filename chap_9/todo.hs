--{-# OPTIONS -Wall -Werror #-}

import System.Environment
import System.Directory
import System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
--dispatch "remove" = remove

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show (n:: Integer) ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

main = do
    (command:argList) <- getArgs
    dispatch command argList
