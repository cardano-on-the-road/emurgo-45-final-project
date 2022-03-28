module Types where

import              Data.Text
import              Data.Matrix
import              Control.Monad.Trans.RWS             (RWST)
import              Control.Monad.Trans.Maybe

type Pos = (Int, Int)
type Rat = Pos
type Snake = [Pos]
type Env = Matrix Char

type RWSIO a = RWST Env (IO Text) GameState IO a

data Direction = U | D | L | R | NA
    deriving (Show, Eq)


data GameState = GameState { snake :: Snake
                            , rat ::  Rat
                            , score :: Int
                            , direction:: Direction
                            } deriving Show