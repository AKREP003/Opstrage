module Lib
    ( someFunc
     
    ) where

import System.IO
import Data.Char

charToNum :: Char -> Int
charToNum n = ord n

someFunc :: String -> IO String
someFunc filePath = readFile filePath
  
               
  
