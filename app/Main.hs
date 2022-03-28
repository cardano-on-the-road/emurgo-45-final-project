module Main where

{-# LANGUAGE OverloadedStrings #-}    

import              Control.Monad                                       (when)
import              Control.Monad.Trans.Class                           (lift)
import              Control.Monad.Trans.RWS                             (RWST, runRWST, evalRWST, execRWST, modify, tell, ask, put)
import              Control.Monad.Trans.RWS.Lazy                        (get)
import              Data.Text                                           (append, pack, unpack, Text, toUpper )
import              Data.Matrix
import              Data.Tuple.Extra                                    (first, second)
import              System.Environment                                  (getArgs)

import              Data.List


import              Types
import              Draw                                           (draw, clearScreen)
import              Data.Maybe
import              Controller
import              Logger


cli:: IO (Matrix Char)
cli = do
  parseArgs <$> getArgs

parseArgs:: [String] -> Matrix Char
parseArgs [c]
    | Prelude.read c >= 4 && Prelude.read c <= 15 = matrix (Prelude.read c) (Prelude.read c) (\(i, j) -> ' ')
    | otherwise = error "Invalid argument use a number between 4 and 15"


runApp:: RWSIO ()
runApp = do
    gameState <- get
    env <- ask
    lift $ draw env gameState
    lift $ print gameState
    lift $ putStrLn "Move snake with WASD"
    userInput <- lift getLine
    let direction = parseDirection userInput
    if direction /= NA
        then do
        let newSnakePos = moveSnake direction (snake gameState)
            newGameState = gameState {snake = newSnakePos}
        (tell . logEvent) ("New snake position: " ++ show newSnakePos ++ " direction: " ++ show direction )
        if isLost env newSnakePos $ snake gameState
            then do
                (tell . logEvent) ("GAME OVER - gamestate: " ++ show gameState)
                lift $ putStrLn ("GAME OVER - gamestate: " ++ show gameState)
                return ()
            else do
                if rat newGameState `elem` snake newGameState
                    then do
                        let newSnake = feedSnake gameState
                            newScore = score newGameState + 1
                        newRat <- lift $ randomRatPos env newSnake
                        (tell . logEvent) ("Snake ate rat at: " ++ show (rat newGameState))
                        let newGameState' = newGameState {snake = newSnake, rat = newRat, score = newScore}
                        put newGameState'
                        lift $ draw env newGameState'
                        lift clearScreen
                        runApp
                    else do
                        put newGameState
                        lift clearScreen
                        runApp
    else do
        lift clearScreen
        lift $ putStrLn "You can use only WASD keys!"
        runApp


main :: IO ()
main = do
    env <- cli
    let gameState = initGame env
    (s,w') <- execRWST(do
                        (tell . logEvent) "READY PLAYER ONE"
                        runApp
                        (tell.logEvent) "GAME ENDED\n\n\n"
                        ) env gameState
    print s
    w <- w'
    appendFile "log.txt" $ unpack w