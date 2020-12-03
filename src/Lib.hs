module Lib
    ( someFunc
    ) where

import           API    ()
import           Server ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
