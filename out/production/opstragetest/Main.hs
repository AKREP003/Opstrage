import Numeric
import Data.Char
import Prelude
import Data.Hashable
import Data.HashMap.Strict hiding (map)

newtype Byte = Byte [Bool] deriving (Show, Eq, Ord)

newtype BooleanFileContents = BooleanFileContents [(Byte, Byte)] deriving Show

newtype TruthTable = TruthTable [(Byte, Bool)] deriving Show

newtype Expression = Expression (Byte, Byte) deriving Show

newtype Statement = Statement [Expression] deriving Show

byte_len :: Int
byte_len = 3

-- Parse
strToNum :: FilePath -> IO BooleanFileContents
strToNum filePath =  do
 fileContents <- readFile filePath
 return $ BooleanFileContents (addIndex (map (intToBinaryTuple . ord) fileContents))

intToBinaryTuple :: Int -> Byte
intToBinaryTuple x =
    let binaryStr = showIntAtBase 2 intToDigit x ""
        paddedBinaryStr = replicate (byte_len - length binaryStr) '0' ++ binaryStr
        bits = map (== '1') paddedBinaryStr
    in Byte (reverse bits)  -- Reverse the bits to match the expected representation


addIndex :: [Byte] -> [(Byte, Byte)]
addIndex = zip (map intToBinaryTuple [0..])

-- create truthtables
createTruthTableForBit :: Int -> BooleanFileContents -> TruthTable
createTruthTableForBit bitPos (BooleanFileContents content) =
  TruthTable [(input, getBit output !! bitPos) | (input, output) <- content]

-- Function to extract the list of booleans from a Byte
getBit :: Byte -> [Bool]
getBit (Byte bits) = bits

-- Function to create a list of TruthTables for each bit in a Byte
createTruthTablesForByte :: BooleanFileContents -> [TruthTable]
createTruthTablesForByte (BooleanFileContents content) =
  [createTruthTableForBit bitPos (BooleanFileContents content) | bitPos <- [0 .. length (getBit (fst (head content))) - 1]]

-- k-map
filterTruth :: TruthTable -> [Byte]
filterTruth (TruthTable []) = []
filterTruth (TruthTable ((b, o):t))
  | o = [b] ++ filterTruth (TruthTable t)
  | otherwise = filterTruth (TruthTable t)


logDif :: HashMap Int Int -> Int -> HashMap Int Int
logDif h n = insert n (findWithDefault 0 n h + 1) h -- With poor documentation, comes great frustration -yoda the senior dev

boolToBase3 :: Bool -> Int -> Int
boolToBase3 n d
 | n = d * 2
 | otherwise = d

encodeByte :: Byte -> Int -> [Int] -- Find a way to encode every fact about a Byte. and turn them into a list. Maybe base 3
encodeByte (Byte []) _  = [0]
encodeByte (Byte (x:n)) las  =
  let r = encodeByte (Byte n) (las * 3)
  in (map (+ (boolToBase3 x las)) r) ++ r


logEmAll :: HashMap Int Int -> [Int] -> HashMap Int Int
logEmAll h [] = h
logEmAll h (x:n) = logEmAll (logDif h x) n

storeDifs :: [Byte] -> HashMap Int Int -> HashMap Int Int
storeDifs  [] n = n
storeDifs (x:t) difs = storeDifs t (logEmAll difs (encodeByte x 1))

isPowerOf2 :: Int -> Float
isPowerOf2 n =
  let lg = logBase 2 (fromIntegral n)
  in lg



filterDif :: [(Int, Int)] -> [Int]
filterDif []  = []
filterDif ((x, occurance):rest)
  | x == 1 = x : filterDif rest
  | not ((fromIntegral byte_len - ( (isPowerOf2 x ) )) == (fromIntegral (div occurance 2)) ) = filterDif rest
  | otherwise = x : filterDif rest



storeOccurence :: [(Int, Int)] ->  HashMap Int Int -> HashMap Int Int
storeOccurence [] h = h
storeOccurence ((_, o):n) h = storeOccurence n (logDif h o)
--analyzeTruth :: Byte -> TruthTable -> Expression


survivalOfTheFittest :: [Int] -> [(Int, Int)] -> [Int]
survivalOfTheFittest _ [] = []
survivalOfTheFittest fit ((x, o):n)
 | o `elem` fit  = x : survivalOfTheFittest fit n
 | otherwise = survivalOfTheFittest fit n

main :: IO ()
main = do
  --let filePath = "C:/Users/aliek/Desktop/hello.txt"  -- Replace with your actual file path
  --allTruthTables <- strToNum filePath
  
  --let idk = createTruthTablesForByte allTruthTables
  --let t = filterTruth (head idk)
  
  --print  (isPowerOf2 7)
  -- (filterDif (toList (storeDifs [Byte [True, False, True], Byte [True, False, True], Byte [False, True, True]] empty)) [] 0)

  -- [Byte [True, False, True], Byte [True, True, True], Byte [True, False, False], Byte [True, True, False] ]

  let k = toList (storeDifs [Byte [True, False, True]] empty)

  let st = storeOccurence k empty

  let fittest = (filterDif . toList) (st)

  print k
  print st

  print (maximum (survivalOfTheFittest ( fittest) k))



  --print (encodeByte (Byte [True, False, True]) 1)


--[Byte [True,False,False,False,False,False,False,False],Byte [False,True,False,False,False,False,False,False]]
