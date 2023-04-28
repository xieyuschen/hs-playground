{-# LANGUAGE BlockArguments #-}
import System.Process
import Data.Text.IO (hGetLine)
import GHC.IO.Handle (isEOF, hGetLine)
import GHC.IO.Handle.Types (Handle)
import Data.Type.Equality (outer)

main :: IO ()
main = do
  (_, out , _, _) <- createProcess (proc "ls" []){ std_out = CreatePipe }
  myLoop out

myLoop :: Maybe Handle -> IO ()
myLoop input = do 
  done <- isEOF
  if done
    then return ()
    else do 
      let tmp = GHC.IO.Handle.hGetLine <$> input
      case tmp of
        Nothing -> return ()
        Just x -> do
            x >>= putStrLn
            myLoop input
-- a -> m b -> m a 