module TryExceptTAndIO where

import Control.Exception (catch)
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)

-- 1. IO 2. return a value 3. IO might fail, now return value and error message
-- Either (String, Int) Int -> refers to the send fail/succeed
-- ThinkMore: in this demo, I will still try to mock a socket write, but this time I will use moand transformer.
write :: String -> ExceptT (String, Int) IO Int
write s
  | s == "fail" =
      throwE ("fail", 0)
  | s == "fail3" = do
      -- cannot directly perform an IO operation within an ExceptT context, because the signature doesn't expect directly IO
      liftIO $ putStrLn "write fail3"
      throwE ("fail3", 3) -- mock fail to IO but write 3 bytes to the peer
  | otherwise = do
      liftIO $ putStrLn s
      return $ length s

mockWriteToSocket :: IO ()
mockWriteToSocket = do
  -- case 1, fail
  runExceptT (write "fail") >>= logErrorIfAny
  -- case 2, fail but with 3
  runExceptT (write "fail3") >>= logErrorIfAny
  -- case 3, succeed
  runExceptT (write "succeed") >>= logErrorIfAny

logErrorIfAny :: Either (String, Int) Int -> IO ()
logErrorIfAny (Right n) = putStrLn $ "case 1, succeed: " ++ show n
logErrorIfAny (Left (err, n)) = putStrLn $ "case 2, fail: " ++ err ++ ", " ++ show n