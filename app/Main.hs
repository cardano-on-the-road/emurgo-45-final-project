module Main where

import              Control.Monad                                       (when)
import              Control.Monad.Trans.Maybe
import              Control.Monad.Trans.Class                           (lift)
import              Control.Monad.Trans.RWS                             (RWST, runRWST, evalRWST, execRWST, modify, tell, ask, put)
import              Control.Monad.Trans.RWS.Lazy                        (get)
import              Data.Text                                           (append, pack, unpack, Text, toUpper )    
import              Data.Matrix
import              Data.Tuple.Extra                                    (first, second)
import              System.Environment                                  (getArgs)
import              System.Random
import              Data.List
import              Data.Time                                           (getCurrentTime)



type Pos = (Int, Int)
type Rat = Pos
type Snake = [Pos]
type Env = Matrix Char

type RWSIO a = RWST Env Text GameState IO a

data Direction = U | D | L | R | NA
    deriving (Show, Eq)


data GameState = GameState { snake :: Snake
                            , rat ::  Rat
                            , score :: Int
                            , direction:: Direction
                            } deriving Show

logEvent:: String -> IO Text
logEvent log = do 
    time <- getCurrentTime
    let logLine = append (append (pack $ show time) (pack " - ")) (append (pack log) (pack "\n"))
    return logLine

-- Implement read costant
initGame:: Env -> GameState
initGame env =
    let sPos = (1,1)
        cols = ncols env
        rPos = (4, 4) 
        score = 0
    in
        GameState {snake = [sPos]
                    , rat = rPos
                    , score = 0
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

--To Update Score
feedSnake:: Snake -> Pos -> Snake
feedSnake [] pos = [pos]
feedSnake xs pos = pos:xs

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

isLost:: Env -> GameState -> Bool 
isLost env gameState =
    let (pos:xs) = snake gameState
        nduplicate = nub $ (pos:xs)
        fstHeadSnake = fst $ pos
        sndHeadSnake = snd $ pos
        cols = ncols env
    in 
        (pos `elem` xs) || (fstHeadSnake <= 0) || (sndHeadSnake <= 0) || (fstHeadSnake > cols) || (sndHeadSnake > cols)

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


--isADead
cli:: IO (Matrix Char)
cli = do
  args <- getArgs
  return (parseArgs args)

parseArgs:: [String] -> Matrix Char
parseArgs [c] 
    | (Prelude.read c >= 4) && (Prelude.read c <= 12)  = matrix (Prelude.read c) (Prelude.read c) (\(i, j) -> ' ')
    | otherwise = error "Invalid argument use a number between 4 and 12"

parseDirection:: String -> Direction
parseDirection s 
    | pack "W" == toUpper (pack s) = U
    | pack "S" == toUpper (pack s) = D
    | pack "A" == toUpper (pack s) = L
    | pack "D" == toUpper (pack s) = R
    | otherwise = NA

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
        ln <- lift $ logEvent ("New snake position: " ++ show newSnakePos ++ " direction: " ++ show direction )
        tell ln
        if isLost env newGameState
            then do
                ln <- lift $ logEvent ("GAME OVER - gamestate: " ++ show gameState)
                tell ln 
                return ()
            else do
                if rat newGameState `elem` snake newGameState
                    then do 
                        let newSnake = feedSnake (snake gameState) (rat newGameState)
                            newScore = score newGameState + 1
                        newRat <- lift $ randomRatPos env newSnake
                        ln <- lift $ logEvent ("Snake ate rat at: " ++ show (rat newGameState))
                        tell ln
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
    let env = matrix 10 10 (\(x,y) -> ' ')
        gameState = initGame env
    
    (s,w) <- execRWST(do 
                        ln <- lift $ logEvent "READY PLAYER ONE"
                        tell ln
                        runApp
                        ln' <- lift $ logEvent "GAME ENDED\n\n\n"
                        tell ln') env gameState
                        
    
    print s
    appendFile "log.txt" $ unpack w