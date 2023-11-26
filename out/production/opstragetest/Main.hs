import Numeric
import Data.Char
import Data.List (nub, sort, group, find)

newtype Byte = Byte [Bool] deriving (Show, Eq, Ord)

newtype BooleanFileContents = BooleanFileContents [(Byte, Byte)] deriving Show

newtype TruthTable = TruthTable [(Byte, Bool)] deriving Show

newtype Expression = Expression (Byte, Byte) deriving Show

newtype Statement = Statement [Expression] deriving Show

-- Parse
strToNum :: FilePath -> IO BooleanFileContents
strToNum filePath =  do
 fileContents <- readFile filePath
 return $ BooleanFileContents (addIndex (map (intToBinaryTuple . ord) fileContents))

intToBinaryTuple :: Int -> Byte
intToBinaryTuple x =
    let binaryStr = showIntAtBase 2 intToDigit x ""
        paddedBinaryStr = replicate (8 - length binaryStr) '0' ++ binaryStr
        bits = map (== '1') paddedBinaryStr
    in Byte bits

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

adjustSize :: Int -> [Bool] -> [Bool] -> Byte -> Byte
adjustSize inputSize relevancePattern flipPattern (Byte bits) =
  let adjustedBits = take inputSize (zipWith3 (\bit rel flip -> bit /= (rel && flip)) bits relevancePattern flipPattern ++ repeat False)
  in Byte adjustedBits

data TruthValue = TrueValue | FalseValue | DontCare deriving (Eq, Show)

data KMapEntry = KMapEntry { kmapInput :: Byte, kmapOutput :: TruthValue } deriving (Show, Eq)
type KMapGroup = [KMapEntry]

groupByInputPattern :: KMapGroup -> [KMapGroup] -> [KMapGroup]
groupByInputPattern [] groups = groups
groupByInputPattern entry groups =
  let matchingGroup = findMatchingGroup (head entry) groups
      updatedGroups = case matchingGroup of
        Just group -> updateGroup (head entry) group groups
        Nothing -> entry : groups
  in updatedGroups

findMatchingGroup :: KMapEntry -> [KMapGroup] -> Maybe KMapGroup
findMatchingGroup entry = find (\group -> hasSameInputPattern entry group)

hasSameInputPattern :: KMapEntry -> KMapGroup -> Bool
hasSameInputPattern entry group = all (\otherEntry -> kmapInput entry == kmapInput otherEntry) group

updateGroup :: KMapEntry -> KMapGroup -> [KMapGroup] -> [KMapGroup]
updateGroup entry group groups =
  let updatedGroup = entry : group
  in updatedGroup : filter (/= group) groups

-- Function to generate the simplified expression from a TruthTable
generateKMapExpression :: TruthTable -> Statement
generateKMapExpression (TruthTable entries) =
  let sortedEntries = sort entries
      kmapEntries = map truthToKMapEntry sortedEntries
      groupedEntries = groupByInputPattern kmapEntries []
      expressions = concatMap groupToExpression groupedEntries
  in Statement expressions


truthToKMapEntry :: (Byte, Bool) -> KMapEntry
truthToKMapEntry (inputPattern, outputValue) = KMapEntry inputPattern (boolToTruthValue outputValue)

boolToTruthValue :: Bool -> TruthValue
boolToTruthValue True = TrueValue
boolToTruthValue False = FalseValue

-- Function to adjust the size of the output pattern in a group
adjustGroupSize :: KMapGroup -> KMapGroup
adjustGroupSize group =
  let inputSize = length (getBit (kmapInput (head group)))
      relevancePattern = getBit (kmapInput (head group))
      flipPattern = getFlipPattern (kmapOutput (head group))
  in map (\entry -> entry { kmapInput = adjustSize inputSize relevancePattern flipPattern (kmapInput entry) }) group



groupToExpression :: KMapGroup -> [Expression]
groupToExpression group =
  if null group
    then [Expression (Byte [], Byte [])]  -- Adjust as needed for an empty group
    else
      let adjustedGroup = adjustGroupSize group
          inputPatterns = map kmapInput adjustedGroup
          outputPatterns = map (outputToValue . kmapOutput) adjustedGroup
      in [Expression (inputPattern, outputPattern) | (inputPattern, outputPattern) <- zip inputPatterns outputPatterns]

outputToValue :: TruthValue -> Byte
outputToValue TrueValue = Byte [True]
outputToValue FalseValue = Byte [False]
outputToValue DontCare = Byte [False]  -- Adjust as needed

getFlipPattern :: TruthValue -> [Bool]
getFlipPattern TrueValue = repeat True
getFlipPattern FalseValue = repeat False
getFlipPattern DontCare = repeat False







main :: IO ()
main = do
  let filePath = "C:/Users/aliek/Desktop/hello.txt"  -- Replace with your actual file path
  let booleanContents = BooleanFileContents [(Byte [True], Byte [True]), (Byte [False], Byte [False])]
  let allTruthTables = generateKMapExpression ( head (createTruthTablesForByte booleanContents))
  print allTruthTables


--TruthTable [(Byte [True,True,True],True),(Byte [True,False,True],False)]
--Statement [Expression (Byte [True,False,True],Byte [False]),Expression (Byte [True,True,True],Byte [True])]
