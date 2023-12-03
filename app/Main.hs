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

encodeByte :: Byte -> Int -> [Int]
encodeByte (Byte []) _  = []
encodeByte (Byte (x:n)) las 
  | x = [las] ++ encodeByte (Byte n) (las * 2)
  | otherwise = [1 + las] ++ encodeByte (Byte n) (las * 2)


logEmAll :: HashMap Int Int -> [Int] -> HashMap Int Int
logEmAll h [] = h
logEmAll h (x:n) = logEmAll (logDif h x) n

storeDifs :: [Byte] -> HashMap Int Int -> HashMap Int Int
storeDifs  [] n = n
storeDifs (x:t) difs = storeDifs t (logEmAll difs (encodeByte x 2))

isPowerOf2 :: Int -> Float
isPowerOf2 n =
  let lg = logBase 2 (fromIntegral n)
  in lg



filterDif :: [(Int, Int)] -> [Int]
filterDif []  = []
filterDif ((x, occurance):rest)
  | not ((((fromIntegral byte_len) - (isPowerOf2 x ) ) <= (fromIntegral occurance)) ) = filterDif rest 
  | otherwise = x : filterDif rest 
 


storeOccurence :: [(Int, Int)] ->  HashMap Int Int -> HashMap Int Int
storeOccurence [] h = h
storeOccurence ((_, o):n) h = storeOccurence n (logDif h o)
--analyzeTruth :: Byte -> TruthTable -> Expression


survivalOfTheFittest :: Int -> [(Int, Int)] -> Int
survivalOfTheFittest _ _ = 0

main :: IO ()
main = do
  --let filePath = "C:/Users/aliek/Desktop/hello.txt"  -- Replace with your actual file path
  --allTruthTables <- strToNum filePath
  
  --let idk = createTruthTablesForByte allTruthTables
  --let t = filterTruth (head idk)
  
  --print  (isPowerOf2 7)
  -- (filterDif (toList (storeDifs [Byte [True, False, True], Byte [True, False, True], Byte [False, True, True]] empty)) [] 0)

  let k = toList (storeDifs [Byte [True, False, True], Byte [True, True, True], Byte [True, True, False], Byte [True, False, False]] empty)
  
  let st = storeOccurence k empty
  
  print (k)
  
  print (st)
  
  let fittest = (filterDif . toList) (st)
  
  print (fittest)

--[Byte [True,False,False,False,False,False,False,False],Byte [False,True,False,False,False,False,False,False]]
