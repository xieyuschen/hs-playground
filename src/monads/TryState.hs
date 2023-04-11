{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use guards" #-}
module TryState where
import Control.Monad.State

-- Global state or variables are typically not-pure.
-- However, some computations rely on states, for example, a state machanine, a data structure to hold something, etc.
-- ThinkMore: how to implement a stack in haskell? It contains state natively
-- a dull way: return the result and left state every time. However, it's quite a tedious way and we need a syntax sugar to get free from this.
-- so monad state is defiend as this:
-- newtype State s a = State { runState :: s -> (a, s) }

type Stack = [Int]
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)
push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)
top:: Stack -> (Int, Stack)
top (x:xs) = (x, x:xs)

-- input [5,8,2,1], output (5,[8,2,1])
-- this function has a deficient, what if you want to call it multiple times? It's not convenient.
push1NumAndPop2 :: Stack -> (Int, Stack)
push1NumAndPop2 stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    (b, newStack3) = pop newStack2
    in (b, newStack3)

-- ThinkMore: if you want to hold a state, using State monad is a good choice when you want to operate the data structure.
popVState :: State Stack Int
-- the state constructor receive a function indicating how to operate the state
popVState = state $ \(x:xs) -> (x, xs)

pushVState :: Int -> State Stack ()
pushVState a = state $ \xs -> ((), a:xs)

push1NumAndPop2VState :: State Stack Int
push1NumAndPop2VState = do
    pushVState 3
    a <- popVState
    b <- popVState
    return b


-- ThinkMore: how to implement a binary search tree in haskell? It contains state natively
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

addValue :: Ord a => a -> Tree a -> Tree a
addValue x tree =
    case tree of
        Leaf -> Node Leaf x Leaf
        Node left y right ->
            if x < y
                then Node (addValue x left) y right
                else Node left y (addValue x right)

-- ThinkMore: the removeValue function is bad implementation. Maybe appears everywhere.
removeValue :: Ord a => a -> Tree a -> (Maybe a,Tree a)
removeValue x Leaf = (Nothing, Leaf)
removeValue x (Node left y right)
    | x < y = 
        case removeValue x left of
            (_, left) -> (Nothing, Node left y right)
     | x > y =
        case removeValue x right of
            (_, right) -> (Nothing, Node left y right)
     | otherwise = case (left, right) of
                (Leaf, Leaf) -> (Just x, Leaf)
                (Leaf, _) -> (Just x, right)
                (_, Leaf) -> (Just x, left)
                (_, _) ->
                    case removeValue (findMin right) right of
                        -- after removing, the right node disappear.
                        (_, Leaf) -> (Just x, Node left (findMin right) Leaf)
                        (Just x, right) -> (Just x, Node left (findMin right) right)

findMin :: Tree a -> a
findMin tree = 
    case tree of
        Leaf -> error "findMin: empty tree"
        Node Leaf x _ -> x
        Node left _ _ -> findMin left