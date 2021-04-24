{-# OPTIONS -Wall -Werror #-}

import FileSystem

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show
type FSZipper = (FSItem, [FSCrumb])

(-:) :: a -> (a -> b) -> b
x -: f = f x

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs):bs) = (Folder name (ls ++ [item] ++ rs), bs)
fsUp (_, []) = error "Top!"

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)
fsTo _ _ = error "Bottom!"
