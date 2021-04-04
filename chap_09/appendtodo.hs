{-# OPTIONS -Wall -Werror #-}

main :: IO ()
main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
