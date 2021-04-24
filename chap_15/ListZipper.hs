{-# OPTIONS -Wall -Werror #-}

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
goForward ([], _) = error "End of List"

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)
goBack (_, []) = error "Start of List"
