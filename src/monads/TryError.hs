module TryError where

-- Maybe is good, but Nothing happens we don't know anything about it.
-- Either could stores e on the left and value on the right.

-- check whether the password is safe(length>8), if not, return the actually length
askPasswordAndCheck :: IO ()
askPasswordAndCheck = do
  print "input your password please"
  v <- readAndCheck
  case v of
    Left len -> putStrLn $ "your password is too short, length is " ++ show len
    Right s -> putStrLn $ "your password is safe, it is " ++ show s

isSafe :: String -> Bool
isSafe s = length s >= 8

readAndCheck :: IO (Either Int String)
readAndCheck = do
  s <- getLine
  if isSafe s
    then return $ Right s
    else return $ Left $ length s
