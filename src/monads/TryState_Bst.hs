module TryState_Bst where

-- this is a simple implementation of binary search tree
-- todo: apply state monad on it later
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

addValue :: Ord a => a -> Tree a -> Tree a
addValue x Leaf = Node Leaf x Leaf
addValue x (Node left y right)
    | x == y = Node left y right -- already has value, not change
    | x < y = Node (addValue x left) y right
    | x > y = Node left y (addValue x right)

removeValue :: Ord a => a -> Tree a -> Tree a
removeValue x Leaf = Leaf
removeValue x (Node left y right)
    | x < y = Node (removeValue x left) y right
    | x > y = Node left y (removeValue x right)
    | otherwise = case (left, right) of
            (Leaf, Leaf) -> Leaf
            (Leaf, _) -> right
            (_, Leaf) -> left
            (_,_) ->
                let right' = removeValue (findMin right) right
                in Node left (findMin right) right'

findMin :: Tree a -> a
findMin tree = 
    case tree of
        Leaf -> error "findMin: empty tree"
        Node Leaf x _ -> x
        Node left _ _ -> findMin left