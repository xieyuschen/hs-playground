module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

func :: (Num a) => a -> a -> a
func x y = x * y
