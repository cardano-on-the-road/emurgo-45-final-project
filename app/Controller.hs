module Controller where

import              System.Random
import              Types
import              Data.Tuple.Extra                                    (first, second)
import              Data.Text
import              Data.Matrix

parseDirection:: String -> Direction
parseDirection s
    | pack "W" == toUpper (pack s) =  U
    | pack "S" == toUpper (pack s) =  D
    | pack "A" == toUpper (pack s) =  L
    | pack "D" == toUpper (pack s) =  R
    | otherwise = NA


initGame:: Env -> GameState
initGame env =
    let sPos = (1,1)
        cols = ncols env
        rPos = (4, 4)
        score = 1
    in
        GameState {snake = [sPos]
                    , rat = rPos
                    , score = 1
                    , direction = R
                  }

randomRatPos:: Env -> Snake -> IO Pos
randomRatPos env snake = do
    newStdGen
    gen <- getStdGen
    let cols = ncols env
        (num1, gen') = randomR (1, cols) gen :: (Int, StdGen)
        (num2, gen'') = randomR (1, cols) gen' :: (Int, StdGen)
        newRatPos = (num1, num2)
    if newRatPos `elem` snake
        then randomRatPos env snake
        else return (num1, num2)


feedSnake:: GameState -> Snake
feedSnake gs = let
                 x = rat gs
                 xs = snake gs
                in
                    x:xs


moveSnake:: Direction -> Snake -> Snake
moveSnake direction snake =
    let
        snakeHead = Prelude.head snake
        snakeHeadEl = case direction of
            U -> first (\x -> x - 1) snakeHead
            D -> first (+ 1) snakeHead
            L -> second (\x -> x - 1) snakeHead
            R -> second (+1) snakeHead
        snake' = rmLastEl snake
    in
         snakeHeadEl :snake'
    where
        rmLastEl:: Snake -> Snake
        rmLastEl [x] = []
        rmLastEl (x:xs) = x:rmLastEl xs

isLost:: Env -> Snake -> Snake -> Bool
isLost env (npos:nxs) (pos:xs) =
    let fstHeadSnake = fst npos
        sndHeadSnake = snd npos
        cols = ncols env
    in
        npos `elem` (pos:xs) || fstHeadSnake <= 0 || sndHeadSnake <= 0 || fstHeadSnake > cols || sndHeadSnake > cols
isLost _ _ _ = True