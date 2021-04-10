{-# OPTIONS -Wall -Werror #-}

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main :: IO ()
main = do
  a <- myAction
  putStrLn $ "The two lines concatenated turn out to be: " ++ a
