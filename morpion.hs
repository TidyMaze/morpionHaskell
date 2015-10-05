module Main where

import Control.Lens
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe

gridToString :: [[Int]] -> String
gridToString grid = concat (map (\line -> ((intercalate " " (map show line)) ++ "\n")) grid)

doEnd :: [[Int]] -> IO ()
doEnd grid = putStr(gridToString grid)
          >> putStrLn "Fin de la partie :)"

play :: Int -> [[Int]] -> IO ()
play numTurn grid | isOver grid = doEnd grid
                  | null(getEmptyCells grid) = putStrLn "Aucune case jouable"
                  | otherwise = (update grid numTurn) >>= play (numTurn+1)

getEmptyCells :: [[Int]] -> [(Int, Int)]
getEmptyCells grid = [ (l,c) | l<-indexesOf grid, c<-indexesOf grid, getFrom grid l c == 0]

calcOffset :: [[a]] -> Int -> Int -> Int
calcOffset grid line column = line*(length grid)+column

{- Retourne une liste 2D modifiée : -}
update2DList :: [[a]] -> Int -> Int -> a -> [[a]]
update2DList grid line column el = chunksOf gridLength editedGrid
    where
        editedGrid = replaceInList flatGrid offset el
        flatGrid = concat grid
        gridLength = length grid
        offset = calcOffset grid line column


{- remplace une valeur dans une liste, gros hack -}
replaceInList :: [a] -> Int -> a -> [a]
replaceInList l o el = l & element o .~ el

askCell :: IO (Int, Int)
askCell = do
    line <- getLine
    return (parseCell line)

mapTouchePosi :: Map.Map Int (Int, Int)
mapTouchePosi = Map.fromList [
        (7, (0,0)),(8, (0,1)),(9, (0,2)),
        (4, (1,0)),(5, (1,1)),(6, (1,2)),
        (1, (2,0)),(2, (2,1)),(3, (2,2))
    ]

parseCell :: String -> (Int, Int)
parseCell s = fromJust (Map.lookup (read s :: Int) mapTouchePosi)

updateByPlaying :: [[Int]] -> Int -> IO [[Int]]
updateByPlaying grid numPlayer = do
    putStrLn ""
    putStr(gridToString grid)
    cell <- askCell
    return (playOnCell grid cell ((numPlayer `mod` 2)+1))

update :: [[Int]] -> Int -> IO [[Int]]
update grid numPlayer = updateByPlaying grid numPlayer
    
playOnCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
playOnCell grid (l,c) = update2DList grid l c

isOver :: [[Int]] -> Bool
isOver grid = any (playerWin grid) [1,2]

playerWin :: [[Int]] -> Int -> Bool
playerWin grid numPlayer = checkLines   grid numPlayer
                        || checkColumns grid numPlayer
                        || checkDiags   grid numPlayer

indexesOf :: [a] -> [Int]
indexesOf l = [0..length l-1]

getFrom :: [[a]] -> Int -> Int -> a
getFrom grid l c = grid !! l !! c

checkLines :: [[Int]] -> Int -> Bool
checkLines grid numPlayer = any (checkLine grid numPlayer) (indexesOf grid)

checkLine :: [[Int]] -> Int -> Int -> Bool
checkLine grid numPlayer numLine = all (==numPlayer) cases
    where cases = [getFrom grid numLine x | x<-indexesOf grid]

checkColumns :: [[Int]] -> Int -> Bool
checkColumns grid numPlayer = any (checkColumn grid numPlayer) (indexesOf grid)

checkColumn :: [[Int]] -> Int -> Int -> Bool
checkColumn grid numPlayer numColumn = all (==numPlayer) cases
    where cases = [getFrom grid x numColumn | x<-indexesOf grid]

checkDiags :: [[Int]] -> Int -> Bool
checkDiags grid numPlayer = all (==numPlayer) [getFrom grid y y     | y <-indexesOf grid]
                         || all (==numPlayer) [getFrom grid (2-y) y | y <-indexesOf grid]

initListe :: [[Int]]
initListe = [[0,0,0],[0,0,0],[0,0,0]]

main :: IO ()
main = do
    play 0 initListe