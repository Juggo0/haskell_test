module Main where

import Debug.Trace
import Control.Arrow (returnA)

multiplyBy :: [Int] -> Int -> [Int]
multiplyBy input factor = [x*factor| x<-input]

loop :: [Int] -> [Bool] -> [Bool]
loop numbers result = do
    if null numbers then
        result
    else do
        let currAnsw = even $ head numbers
        loop (tail numbers) $ result ++ [currAnsw]

splitBySpace :: [Char] -> ([Char], [Char])
splitBySpace list = do
    removeSpaces list $ findSpaceIndexes list   -- list: [1241241    1424234141], findSpaceIndexes: [7,8,9,10]
    where
        removeSpaces :: [Char] -> [Int] -> ([Char], [Char]) -- returns ([1241241], [1424234141])
        removeSpaces list indexes = do
            let firstList = take (head indexes) list
            let secondList = drop (last indexes+1) list
            (firstList, secondList)
        findSpaceIndexes :: [Char] -> [Int]                     -- returns [7,8,9,10]
        findSpaceIndexes list = do
            let retList = []
            if null list then
                retList
            else findSpaceRecurse list 0 (length list) retList
                where
                    findSpaceRecurse :: [Char] -> Int -> Int -> [Int] -> [Int] -- list, index, lenght of list, stored indexes, returning indexes       
                                                                                 -- returns [7,8,9,10]                                                                           
                    findSpaceRecurse list i len indexes = do
                        if i == len-1 then
                            indexes
                        else findSpaceRecurse list (i+1) len $ isSpace (list !! i) indexes i
                            where
                                isSpace :: Char -> [Int] -> Int -> [Int]       -- returns if the current character given is a spacebar
                                isSpace currText indexes currIndex = do
                                    if currText == ' ' then
                                        indexes ++ [currIndex]
                                    else indexes

main :: IO ()
main = do
    listInput <- getLine
    multiplyInput <- getLine
    let texts = splitBySpace listInput
    let from = read $ fst texts
    let to = read $ snd texts
    print $ loop (multiplyBy [from..to] $ read multiplyInput) []
