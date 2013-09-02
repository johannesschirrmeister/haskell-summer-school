module Lists where

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

insert :: Int -> [Int] -> [Int]
insert n [] = [n] 
insert n l = if n <= head l then n : l else head l : (insert n (tail l))

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insert x (sort xs)

flattenFoldr :: [[a]] -> [a]
flattenFoldr [] = []
flattenFoldr l = foldr (++) [] l

sortFoldr :: [Int] -> [Int]
sortFoldr [] = []
sortFoldr l = foldr (insert) [] l