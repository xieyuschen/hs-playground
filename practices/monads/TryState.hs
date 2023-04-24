{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use guards" #-}

module TryState where

import Control.Monad.State
import System.Random (Random (random), RandomGen, StdGen, mkStdGen)
import Text.ParserCombinators.ReadP (count)
import Text.Printf (vFmt)

-- Global state or variables are typically not-pure.
-- However, some computations rely on states, for example, a state machanine, a data structure to hold something, etc.
-- ThinkMore: how to implement a stack in haskell? It contains state natively
-- a dull way: return the result and left state every time. However, it's quite a tedious way and we need a syntax sugar to get free from this.
-- so monad state is defiend as this:
-- newtype State s a = State { runState :: s -> (a, s) }

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

top :: Stack -> (Int, Stack)
top (x : xs) = (x, x : xs)

-- input [5,8,2,1], output (5,[8,2,1])
-- this function has a deficient, what if you want to call it multiple times? It's not convenient.
push1NumAndPop2 :: Stack -> (Int, Stack)
push1NumAndPop2 stack =
  let ((), newStack1) = push 3 stack
      (a, newStack2) = pop newStack1
      (b, newStack3) = pop newStack2
   in (b, newStack3)

-- ThinkMore: if you want to hold a state, using State monad is a good choice when you want to operate the data structure.
popVState :: State Stack Int
-- the state constructor receive a function indicating how to operate the state
popVState = state $ \(x : xs) -> (x, xs)

pushVState :: Int -> State Stack ()
pushVState a = state $ \xs -> ((), a : xs)

push1NumAndPop2VState :: State Stack Int
-- in the scope of push1NumAndPop2VState, we can ignore the state value, and only focus on the result
-- It's a syntax sugar to benefit such cases, so the operation logic never changes.
push1NumAndPop2VState = do
  pushVState 3
  a <- popVState
  b <- popVState
  return b

-- using a structure with states, we need to construct a new one and use it each time
genAndPrintRandomTwice :: IO ()
genAndPrintRandomTwice = do
  -- random is a polymorphic function that can generate random values of different types, and the compiler doesn't know which type to choose.
  let (num, generator) = random genSeed :: (Int, StdGen)
  print num
  let (num2, g2) = random generator :: (Int, StdGen)
  print num2

-- newtype State s a = State { runState :: s -> (a, s) }, random:: (RandomGen g, Random a) => g -> (a, g)
-- randomSt constructs a state monad with the random function
randomSt :: (RandomGen g, Random a) => State g a
-- if you want to operate the value wrapped inside the state monad, you should hold a fn instead it
-- the <- is a syntax sugar of operator ">>="
-- It will modify the value hold inside a state monad, and return the new state monad
-- >>= :: State s a -> (a -> State s b) -> State s b,
-- newtype State s a = State { runState :: s -> (a, s) }
-- pure :: a -> State s a, this means the s could be intepreted to StdGen.
-- the s in randomSt is a functor(also a reader monad),
randomSt = state random

genSeed :: StdGen
genSeed = mkStdGen 100

-- the StdGen will be passed when we runState
gen2RandomValues :: State StdGen (Int, Int)
gen2RandomValues = do
  a <- randomSt
  b <- randomSt
  return (a, b)

-- unfold the above code will look like this:
-- randomSt >>= \a ->
-- randomSt >>= \b ->
-- pure (a, b)
genAndPrintRandomTwiceByState :: IO ()
genAndPrintRandomTwiceByState = do
  let v = runState gen2RandomValues genSeed
  print $ fst v

-- ThinkMore: practice1, use a state to implement a counter, but it only have one state
type MyCounter = Int

-- we should have a function with a pre-defined state, and then we could use a State monad
-- count add the counter 1, return current value and then return the new counter
mycount :: State MyCounter Int
mycount = state $ \counter -> (counter, counter + 1)

callCounter3Times :: State MyCounter [Int]
callCounter3Times = do
  a <- mycount
  b <- mycount
  c <- mycount
  return [a, b, c]

-- ThinkMore: practice2, use a state to implement a counter, which could pass a BaseNumber to setup the interval.
type MyCounter2 = Int

mycountS :: Int -> State MyCounter Int
mycountS n = state $ \counter -> (counter, counter + n)

callCounter3Times2 :: State MyCounter [Int]
callCounter3Times2 = do
  a <- mycountS 1
  b <- mycountS 2
  c <- mycountS 3
  return [a, b, c]