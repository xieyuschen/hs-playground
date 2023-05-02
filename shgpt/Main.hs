{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
import GHC.Generics
import System.Process
import Data.Text.IO (hGetLine)
import GHC.IO.Handle (isEOF, hGetLine, hIsEOF, hIsClosed)
import GHC.IO.Handle.Types (Handle, Handle__ (Handle__))
import Data.Type.Equality (outer)
import Control.Exception
import System.IO
import Data.Aeson
    ( defaultOptions, genericToEncoding, ToJSON(toEncoding), FromJSON, decode )
import Control.Monad
import Data.ByteString.Lazy.UTF8 as BLU (fromString)

tmp :: (Show a ) => Maybe a -> IO ()
tmp x = case x of
  Nothing -> print "nothing"
  Just x -> print x

main :: IO ()
main = do
  contents <- readFile "demo.json"
  case Data.Aeson.decode $ BLU.fromString contents :: Maybe SimpleResponse of
    Nothing -> print "nothing"
    Just x -> do
      print $ "read json - status: " ++ show (status x)
      print $ "read json - message: " ++ message x

  
data SimpleResponse = SimpleResponse
  { status :: Int,
    message :: String
  }
  deriving (Generic, Show)

instance ToJSON SimpleResponse where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON SimpleResponse















































------------------ read and write to a named pipe(unfinished)------------------


createAndRead :: IO ()
createAndRead = do
  -- create a named pipe if not exsit
  result <- try' $ createProcess (proc "mkfifo" ["try"]){ std_out = CreatePipe }
  case result of
    Left e -> do
      print e
    Right (_, out , _, p) -> do
      myLoop out
      waitForProcess p >>= \x -> return ()

  handle <- openFile "try" ReadMode
  readAndOutput handle 

readAndOutput :: Handle -> IO ()
readAndOutput handle = do
  eof <- hIsEOF handle
  if eof
    then print "exit due to encounter eof"
    else do
      readable <- hIsReadable handle
      when readable $ do
        line <- System.IO.hGetLine handle
        putStrLn line
      readAndOutput handle
      
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
        