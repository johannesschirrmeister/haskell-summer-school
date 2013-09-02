module Trees where

import StateMonad

data Tree a = Leaf | Node (Tree a) a (Tree a)
        deriving Show
        
single :: a -> Tree a
single e = Node Leaf e Leaf

size :: Tree a -> Int
size Leaf         = 0 
size (Node l _ r) = size(l) + size(r) + 1

height :: Tree a -> Int
height Leaf         = 1
height (Node l _ r) = max ((height l) + 1) ((height r) + 1)

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node l e r) = (flatten l) ++ (e : (flatten r))

reverseTree :: Tree a -> Tree a
reverseTree Leaf = Leaf
reverseTree (Node l e r) = (Node (reverseTree r) e (reverseTree l))

treesort :: Ord a => [a] -> Tree a
treesort [] = Leaf
treesort l = Node (treesort [ x | x <- (tail l), x <= (head l)]) 
        (head l)
        (treesort [ x | x <- (tail l), x > (head l)])
        
bst :: Ord a => Tree a -> Bool
bst Leaf = True
bst t = (flatten t) == flatten (treesort (flatten t))

labelTree :: Tree a -> State Int (Tree Int)
labelTree Leaf = return Leaf
labelTree (Node l _ r) = do
                                n <- get
                                incState
                                left <- labelTree l
                                right <- labelTree r                              
                                return (Node left n right)

incState :: State Int ()
incState = do
                n <- get
                put (n + 1)
                
