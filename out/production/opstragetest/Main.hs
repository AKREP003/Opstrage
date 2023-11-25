import Numeric
import Data.Char

newtype Byte = Byte (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) deriving Show

newtype TruthTable = TruthTable [(Byte, Byte)] deriving Show

newtype Expression = Expression (Byte, Byte) deriving Show

newtype Statement = Statement [Expression] deriving Show


strToNum :: FilePath -> IO TruthTable
strToNum filePath =  do
 fileContents <- readFile filePath
 return $ TruthTable (addIndex (map (intToBinaryTuple . ord) fileContents))

intToBinaryTuple :: Int -> Byte
intToBinaryTuple x =
    let binaryStr = showIntAtBase 2 intToDigit x ""
        paddedBinaryStr = replicate (8 - length binaryStr) '0' ++ binaryStr
        bits = map (== '1') paddedBinaryStr
    in case bits of
        [a, b, c, d, e, f, g, h] -> Byte (a, b, c, d, e, f, g, h)
        _ -> error "Invalid binary representation size"

addIndex :: [Byte] -> [(Byte, Byte)]
addIndex = zip (map intToBinaryTuple [0..])







main :: IO ()
main = do
 numericValues <- strToNum "C:/Users/aliek/Desktop/hello.txt"
 print numericValues




