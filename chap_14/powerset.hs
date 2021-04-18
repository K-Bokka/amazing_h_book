{-# OPTIONS -Wall -Werror #-}

import Control.Monad.Writer

powerset :: [a] -> [[a]]
powerset xs = filterM (\_ -> [True, False]) xs
