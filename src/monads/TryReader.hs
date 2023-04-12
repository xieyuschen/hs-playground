{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module TryReader where

import Control.Monad.Reader (Reader)

-- this topic review the operator <$>, especially for the functor
-- ((+3) <$> (*2)) 2 =?
-- the result is 7=(+3 (*2 2))
-- this should align the understanding of the definition of <$>
-- <$> :: (a -> b) -> f a -> f b, the result of `f a` is still a, and then pass it to (a -> b), and then the result is f b
-- fmap g f = g <$> f = g . f = g (f x)

-- ThinkMore: the (-> r) is also a monad, for some reason it's called Reader

combineMulti5AndPlus3 :: (Int -> Int -> Int) -> Int -> Int
combineMulti5AndPlus3 f x =
  let a = (+ 3) x
      b = (* 5) x
   in f a b
