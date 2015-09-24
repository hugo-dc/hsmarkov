module Main where
import qualified Data.Map
import Data.List (intercalate)
import System.Random
import System.Random.Shuffle
import System.IO.Strict as St

-- | String partitions
partitionAll :: Int -> Int -> [a] -> [[a]]
partitionAll a b [] = []
partitionAll a b xs = take a xs : (partitionAll a b (drop b xs))

-- | Create a chain using a Map
wordChain :: [[String]] -> Data.Map.Map [String] [String]
wordChain [] = Data.Map.empty
wordChain xs = calcWordChain xs Data.Map.empty (length (head xs))

-- | Calculate Word Chain
calcWordChain :: [[String]] -> Data.Map.Map [String] [String] -> Int -> Data.Map.Map [String] [String]
calcWordChain []     m n = m
calcWordChain (x:xs) m n | length x == ( n - 1 )      = calcWordChain xs ev n
                         | length x == 1              = calcWordChain xs ek n
                         | Data.Map.member (init x) m = calcWordChain xs cm n
                         | otherwise                  = calcWordChain xs nm n
  where nm = Data.Map.insert (init x) [last x] m
        cm = Data.Map.insert (init x) (( m Data.Map.! (init x) ) ++ [last x]) m
        ev = Data.Map.insert x [] m
        ek = Data.Map.insert ( x ++ [""] ) [] m

-- | Convert text to word chain
textToWordChain x = wordChain (partitionAll 3 1 ( words x))

-- | Convert chain to text
chainToText :: [String] -> String
chainToText= intercalate " "

-- | walkChain
walkChain :: [String] -> Data.Map.Map [String] [String] -> [String] -> IO [String]
walkChain p c r | null suffixes = return r
                | otherwise     = do
                    ncc <- newResCharCount
                    if ncc >= 140 then
                      return r
                    else do
                      np <- newPref
                      suf <- suffix
                      walkChain np c ( r ++ [suf] )
  where suffixes | p `elem` (Data.Map.keys c) = c Data.Map.! p
                 | otherwise                  = []
        suffix :: IO String
        suffix = do
                    g <- newStdGen
                    let rndSuf = shuffle' suffixes (length suffixes) g
                    return $ head rndSuf
        newPref = do
          suf <- suffix
          return $ (last p) : suf : []
        resultWithSpaces = chainToText r
        resultCharCount  = length resultWithSpaces
        suffixCharCount  = do
             suf <- suffix
             return $ (length suf ) + 1
        newResCharCount  = do
             sc <- suffixCharCount
             return $ resultCharCount + sc

-- | Generate text
generateText st wc = do
            rs <- resChain
            return $ chainToText rs
  where prefix = words st
        resChain = walkChain prefix wc prefix

-- | Process file NOT USED
processFile fname = do
  contents <- St.readFile fname
  return $ textToWordChain contents

-- | Files 
files = ["resources/quangle-wangle.txt",
         "resources/functional.txt",
         "resources/haskell.txt",
         "resources/quixote.txt"]

-- | Merge file contents into one string
getFilesContent xs = appendConts "" xs
  where appendConts st []     = return st
        appendConts st (y:ys) = do
          conts <- St.readFile y
          let new = st ++ " " ++ conts
          appendConts new ys

-- | Get a prefix list
prefixList = do
  conts <- St.readFile "resources/prefix_list.txt"
  return (lines conts)

-- | Replace char in string
replace st c w = map (\x -> if x == c then w else x) st

-- | End punctuation
endLastPunct st = replace ct '"' '\''
                  where rt = trimToLastPunct st
                        ct = if last rt == ',' ||
                                last rt == ' '
                             then (init rt) ++ ['.']
                             else rt ++ ['.']

trimToLastPunct st | elem ',' st || elem '.' st = trimPunct (reverse st)
                   | otherwise                  = st
        where trimPunct (x:xs) | x == ',' || x == '.' = reverse xs
                               | otherwise            = trimPunct xs

-- | Generate Tweet
tweetText = do
  g <- newStdGen
  prefLst <- prefixList
  let rndSuf = shuffle' prefLst (length prefLst) g
  let prefix = head rndSuf
  conts <- getFilesContent files
  let chain = textToWordChain conts
  text <- generateText prefix chain
  let res = endLastPunct text
  putStrLn res


main :: IO ()
main = tweetText

