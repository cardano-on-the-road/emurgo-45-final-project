module Draw where

import              Types                   (Env, GameState, Rat, Snake, rat, snake)
import              Data.Matrix


draw :: Env -> GameState -> IO()
draw env  gameState = do
    let table = env
        sTable  = drawSnake table $ snake gameState
        srTable = drawRat sTable $ rat gameState
    print srTable

    where
        drawSnake:: Matrix Char-> Snake -> Matrix Char
        drawSnake m [pos] = setElem 'S' pos m
        drawSnake m (pos:xs) = drawSnake (setElem 'S' pos m) xs

        drawRat:: Matrix Char-> Rat -> Matrix Char
        drawRat m pos = setElem 'R' pos m

clearScreen:: IO ()
clearScreen = putStr "\ESC[2J"