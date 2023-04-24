module TryCombineMonads where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

isValid :: String -> Bool
isValid s = length s >= 8

validate :: String -> Maybe String
validate s = do
  guard (isValid s)
  return s

-- inputAndValidate return value marks it does IO, and it manupulates the result to
-- avoid an invliad input.
-- so we need to wrap the String in a Maybe. That's the thing we want to express.
-- we cannot use Maybe (IO String), because IO always happen and we actually check the
-- validation of input.
inputAndValidateInNativeWay :: IO (Maybe String)
inputAndValidateInNativeWay = do
  putStrLn "Insert your new passphrase:"
  validate <$> getLine

askPassphrase :: IO ()
askPassphrase = do
  -- here there is a unboxing, we get the value from IO
  v <- inputAndValidateInNativeWay
  putStrLn $ "we receive a value: " ++ show v

askPassphraseAndCheck :: IO ()
askPassphraseAndCheck = do
  askPassphrase

-- ThinkMore: IO with stdio always succeed, but how about the socket? Let's mock it.
-- Mock a socket write, but without return a length 'n' of bytes.
-- Mock fail condition is that when the input is "fail", it will fail.
write :: String -> MaybeT IO ()
write s = do
  guard (s /= "fail")
  return ()

-- ThinkMore: leap it, also return the length of bytes.
write2 :: String -> Either (String, Int) Int
write2 s
  | s == "fail" = Left ("fail", 0)
  | s == "fail3" = Left ("fail3", 3) -- mock fail to IO but write 3 bytes to the peer
  | otherwise = Right $ length s

-- ThinkMore: no matter fail or not, the Int always return. Our use implementation is not good. Use Except instead
write2ButExcept :: String -> Except (String, Int) Int
write2ButExcept s
  | s == "fail" = throwE ("fail", 0)
  | s == "fail3" = throwE ("fail3", 3) -- mock fail to IO but write 3 bytes to the peer
  | otherwise = return $ length s

-- ThinkMore: write2 doesn't write to the socket, but in fact it will, so we write to stdout to simulate it.
write3 :: String -> IO (Except (String, Int) Int)
write3 s
  | s == "fail" = do return $ throwE ("fail", 0)
  | s == "fail3" =
      do
        putStrLn "write 3 bytes to the peer"
        return $ throwE ("fail3", 3)
  | otherwise =
      do
        putStrLn "write to the peer"
        -- first return construct a Except (String,Int) Int, second one construct a IO (Except (String,Int) Int)
        return $ return $ length s
