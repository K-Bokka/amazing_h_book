{-# OPTIONS -Wall -Werror #-}

import FileSystem

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show
type FSZipper = (FSItem, [FSCrumb])

