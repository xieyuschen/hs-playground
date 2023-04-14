module TryMaybeT where
import Control.Monad.Trans.Maybe
import Data.Text.Internal.Fusion (Stream)
import Control.Monad.Cont

tryBindOnMaybeT :: IO ()
tryBindOnMaybeT = do
  putStrLn "Input a number, and return a Just one if >3"
  val <- getLine
  -- we MUST get the value from the runMaybeT result.
  -- >>= will unwrap the value from the monad, that's why it receive a simple value.
  -- and then apply the function to the value, and wrap it back.
  x <- runMaybeT $ constructMaybeTIOInt (read val) >>= fReturnMaybeT
  print x

-- the function must return Maybe m b
-- this gives abilities to the function manupulates the value inside the monad.
fReturnMaybeT :: Int -> MaybeT IO String
fReturnMaybeT x
  | x < 3 = return "less than 3"
  | otherwise = return $ show x

-- return could help to construct,
constructMaybeTIOInt :: Int -> MaybeT IO Int
-- A -> M(A?) = a -> return(Just a), the return will intepret the monad based on the context.
constructMaybeTIOInt = return