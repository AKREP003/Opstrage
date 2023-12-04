import Numeric
import Data.Char
import Prelude
import Data.Hashable
import Data.HashMap.Strict hiding (map, filter)
import Data.List hiding (insert)

newtype Byte = Byte [Bool] deriving (Show, Eq, Ord)

newtype BooleanFileContents = BooleanFileContents [(Byte, Byte)] deriving Show

newtype TruthTable = TruthTable [(Byte, Bool)] deriving (Eq, Show)

newtype Expression = Expression (Byte, Byte) deriving (Eq, Show)

newtype Statement = Statement [Expression] deriving (Eq, Show)

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
  let r = reverse (encodeByte (Byte n) (las * 3))
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
  | not ((fromIntegral byte_len - ( (isPowerOf2 x ) )) == (fromIntegral (div occurance 2)) ) = filterDif rest
  | otherwise = x : filterDif rest

filterDifSinister :: [(Int, Int)] -> [Int]
filterDifSinister []  = []
filterDifSinister ((x, occurance):rest)
  | x == 1 = x : filterDifSinister rest -- may the gods forgive me
  | not ((fromIntegral byte_len - ( (isPowerOf2 x ) )) == (fromIntegral (div occurance 2)) ) = filterDifSinister rest
  | otherwise = x : filterDifSinister rest



storeOccurence :: [(Int, Int)] ->  HashMap Int Int -> HashMap Int Int
storeOccurence [] h = h
storeOccurence ((_, o):n) h = storeOccurence n (logDif h o)
--analyzeTruth :: Byte -> TruthTable -> Expression


survivalOfTheFittest :: [Int] -> [(Int, Int)] -> [Int]
survivalOfTheFittest _ [] = []
survivalOfTheFittest fit ((x, o):n)
 | (x /= 0 ) && (o `elem` fit)  = x : survivalOfTheFittest fit n
 | otherwise = survivalOfTheFittest fit n


base3ToList :: Int -> [Int]
base3ToList n
  | n < 0     = error "Input must be a non-negative integer."
  | n == 0    = replicate (byte_len)  0
  | otherwise = 
    let x = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 3, x `div` 3)) n

    in reverse (replicate (byte_len - length x)  0 ++ x  )
isUsed :: [Int] -> [Bool]
isUsed = map (> 0)

isFlipped :: [Int] -> [Bool]
isFlipped = map (== 1)

constructExpression :: [Int] -> Expression
constructExpression n = Expression (Byte (isUsed n), Byte (isFlipped n))

compareInnovation :: [Int] -> [Int] -> Bool
compareInnovation [] _ = False
compareInnovation _ [] = False
compareInnovation (x:n1) (y:n2)
 | (x /= 0) && (x /= y) = True
 | otherwise = compareInnovation n1 n2


isInnovative :: [[Int]] -> [Int] -> Bool
isInnovative [] _ = True
isInnovative (y:n) x
  | not (compareInnovation x y) = False
  | otherwise = isInnovative n x

filterCloseMindeds :: [[Int]]  -> [[Int]] -> [[Int]]
filterCloseMindeds [ ]  r = r
filterCloseMindeds (x:n)  r
 | isInnovative n x = filterCloseMindeds n  (x : r)
 | otherwise = filterCloseMindeds n  r

k_map :: TruthTable -> ([(Int, Int)] -> [Int]) -> Statement
k_map (TruthTable n) filt =

  if (length n) == 1 then (Statement [((constructExpression . (map defaultBase3) . getBit . head . filterTruth) (TruthTable n))])
  else
    let k = (toList ( storeDifs (filterTruth (TruthTable n)) empty))
    in Statement (map constructExpression (filterCloseMindeds ( ( map base3ToList) ( survivalOfTheFittest  (( filt . toList) (storeOccurence k empty) ) k)) []))



k_M :: TruthTable -> Statement
k_M n = do
 let w = k_map n filterDif
 
 if w == (Statement []) then k_map n filterDifSinister
 else w
 

defaultBase3 :: Bool -> Int
defaultBase3 n = boolToBase3 n 1

main :: IO ()
main = do
  --let filePath = "C:/Users/aliek/Desktop/hello.txt"  -- Replace with your actual file path
  --allTruthTables <- strToNum filePath
  
  --let idk = createTruthTablesForByte allTruthTables
  let t = TruthTable [(Byte [True, False, False], True), (Byte [True, True, True], True), (Byte [True, True, False], True), (Byte [True, False, True], True)]

  --print ( ((map defaultBase3) . getBit . head . filterTruth) t)



  --let k = (toList ( storeDifs (filterTruth t) empty))

  --print k

  --let r = map base3ToList ( survivalOfTheFittest  (( filterDif . toList) (storeOccurence k empty) ) k)

  --print r
  
  print (k_M t)

  --print (map k_map idk)


  --print (encodeByte (Byte [True, False, True]) 1)


--[Byte [True,False,False,False,False,False,False,False],Byte [False,True,False,False,False,False,False,False]]
