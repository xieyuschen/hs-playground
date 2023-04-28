{-# LANGUAGE BlockArguments #-}
import System.Process
import Data.Text.IO (hGetLine)
import GHC.IO.Handle (isEOF, hGetLine, hIsEOF, hIsClosed)
import GHC.IO.Handle.Types (Handle)
import Data.Type.Equality (outer)
import Control.Exception
import System.IO

main :: IO ()
main = do
  -- create a named pipe if not exsit
  result <- try' $ createProcess (proc "mkfifo" ["try"]){ std_out = CreatePipe }
  case result of
    Left e -> do
      print e
    Right (_, out , _, p) -> do
      myLoop out
  print "end"

try' :: IO a -> IO (Either IOException a)
try' = try

nothingIfEmtpy :: String -> Maybe String
nothingIfEmtpy "" = Nothing
nothingIfEmtpy x = Just x


myLoop :: Maybe Handle -> IO ()
myLoop input = do
    -- get line will fail because there is no line to read
    tmp <- case input of
        Nothing -> return $ Left $ userError "encounter the end of line"
        Just h -> do
          eof <- hIsEOF h
          if eof
            then return $ Right Nothing
            else do
              result <- try' $ GHC.IO.Handle.hGetLine h
              return $ case result of
                Left e -> Left e
                Right x -> Right $ Just x

    case tmp of
      Left e -> do
        putStrLn $ "encounter user error:" ++ show e
        pure ()
      Right Nothing -> pure ()
      Right (Just x) -> do
        putStrLn $ "command output:" ++ x
        myLoop input
        