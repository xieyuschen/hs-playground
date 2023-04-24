module ParseFlags where
import System.Console.GetOpt
import System.Environment
import Data.List
import Control.Monad.Reader
import Control.Monad.Trans.Writer

options :: [OptDescr (Options -> Options)]
options = 
    [Option ['v'] ["verbose"] (NoArg (\opts -> opts { verbose = True })) "whether output log"
    ,Option ['d'] ["debug"] (NoArg (\opts -> opts { debug = True })) "whether to print debug log information"
    ,Option ['f'] ["factor"] (OptArg (\f opts -> opts { factor = maybe 1 read f }) "3") "factor to multiply with, defualt is 1"
    ]

data Options = Options 
    { verbose :: Bool
    , debug :: Bool
    , factor :: Int
    } deriving (Show)  

defaultOptions = Options 
    { verbose = False
    , debug = False
    , factor = 1
    }

parseFlag :: IO ()
parseFlag = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    if not $ null errors
        then error $ concat errors ++ usageInfo header options
        else do
            let opts = foldl (flip id) defaultOptions actions
            let (val, infos) = runWriter $ fac 5
            print $ factor opts * val 
    where header = "Usage: logdemo [OPTION...] files..."
    
fac :: Int -> Writer [Int] Int
fac n
    | n == 0 = return 1
    | otherwise = do
        v <- fac (n - 1)
        tell [n]
        return $ n * v