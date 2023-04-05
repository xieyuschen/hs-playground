
module TsDemo (execute) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Cont ( guard )
import Text.Read ( readMaybe )
import Data.Maybe ( isNothing )

isValid :: String -> Bool
isValid s = length s >= 8

validate:: String -> Maybe String
validate s = do
    guard (isValid s)
    return s

-- <$> :: Functor f => (a -> b) -> f a -> f b
convertStrToIntPlus10 :: String -> Maybe Int
convertStrToIntPlus10 x = 
    case readMaybe x of
        Nothing -> Just 0
        Just y -> Just $ y + 10

inputAndValidate :: IO (Maybe String)
inputAndValidate = do
    validate <$> getLine

execute:: IO()
execute = do
    v <- inputAndValidate
    if isNothing v
    then do
        execute
    else
        print $ v >>= convertStrToIntPlus10

       