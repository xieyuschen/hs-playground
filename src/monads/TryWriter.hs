module TryWriter where

import Control.Monad.Writer ( MonadWriter(writer, tell), Writer ) 

-- Writer:: (a, w) -> Writer w a, it requires a monoid on w because we always focus on calculating a
-- the w is a context with less importance, if it's a monoid we needn't to consider more about it.

logNumber :: Int -> Writer [String] Int  
-- As there is no constructor for Writter to benefit WriterT, we use writer to construct a Writer
-- Refer to this link: https://stackoverflow.com/questions/26415122/unable-to-compile-writer-monad-example-from-learn-you-a-haskell
logNumber x = writer (x, ["Got number: " ++ show x])  

-- print $ runWriter combine2WithLog has a result of:
-- (15,["Got number: 3","Got number: 5"])
-- ThinkMore: this function we combine two Writer type together, what could we do to combine more than two?
combine2WithLog :: Writer [String] Int  
combine2WithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    -- a*b indicates how the values are computed, and the contexts are combined together based on the mappend of the monoid.
    -- I could also write the script    
    -- <$> :: (a -> b) -> f a -> f b
    -- return (a*b) is an easier way to express the same thing
    (+) <$> logNumber a <*> logNumber b

-- mySum 10 will return (55,[1,3,6,10,15,21,28,36,45])
-- it's pretty good to use a writter to record something, also `tell` syntax sugar is good
-- however, it is in-effiective because it appends the list in ths way:
-- (([1]++[3]))++[6])++[10]...
-- every time it wants to add the right part to the left part, it has to construct the left part all the way from the beginning
mySum :: Int -> Writer [Int] Int
mySum n 
    | n ==1 = return 1
    | otherwise = do
        a <- mySum $ n-1
        tell [a]
        return $ a+n

-- fabnacci calls like a tree, so the log doesn't make sense because we need to make it readable manually
fabnacci:: Int -> Writer [String] Int
fabnacci n
    | n <= 1 = return n
    | otherwise = do
        a <- fabnacci $ n-1
        b <- fabnacci $ n-2 
        -- as it contains two recursive calls when n > 1, so the log looks like a tree Pre-Order Traversal
        tell ["last values are:" ++ show a ++ " "++ show b]
        return $ a+b