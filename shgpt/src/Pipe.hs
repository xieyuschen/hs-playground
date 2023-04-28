{-# LANGUAGE BlockArguments #-}
module Pipe where

import Pipes
import Control.Monad (unless)
import System.IO (isEOF)

--         +--------+-- A 'Producer' that yields 'String's
--         |        |
--         |        |      +-- Every monad transformer has a base monad.
--         |        |      |   This time the base monad is 'IO'.
--         |        |      |  
--         |        |      |  +-- Every monadic action has a return value.
--         |        |      |  |   This action returns '()' when finished
--         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String', it's the constructor of Producer.
        stdinLn              

-- stdioLn stores the str(like the writer)
-- yield emits a value, suspending the current Producer until the value is consumed.
-- Like a unbuffered channel in golang.

loop :: Effect IO ()
loop = for stdinLn $ \str -> do  -- Read this like: "for str in stdinLn"
    lift $ putStrLn str          -- The body of the 'for' loop

