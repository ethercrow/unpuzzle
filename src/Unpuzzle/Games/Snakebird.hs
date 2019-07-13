module Unpuzzle.Games.Snakebird where

import Relude
import Relude.Extra.Enum
import qualified Data.Text as T
import Data.Foldable (maximum)
import qualified Data.Vector.Unboxed as V
import Data.List (findIndex, (!!))
import Data.Vector.Instances ()

import Unpuzzle.Generic

data GameState = G
    { levelWidth :: !Int
    , levelHeight :: !Int
    , snake :: (Int, Int)
    , fruits :: V.Vector (Int, Int)
    , cells :: V.Vector Int8
    }
  deriving (Show, Generic)

instance Eq GameState where
    g1 == g2 =
        snake g1 == snake g2
        &&
        fruits g1 == fruits g2

instance Hashable GameState where
    hashWithSalt salt G {..} = hashWithSalt salt (snake, fruits)
    {-# inline hashWithSalt #-}

pattern Empty :: Int8
pattern Empty = 0
pattern Wall :: Int8
pattern Wall = 1
pattern Portal :: Int8
pattern Portal = 2
pattern Fruit :: Int8
pattern Fruit = 3

(!) :: V.Unbox a => V.Vector a -> Int -> a
-- v ! i = v `V.unsafeIndex` i -- without bounds checking
v ! i = v V.! i -- with bounds checking

type Cell = Int8

isFull :: GameState -> Int -> Int -> Bool
isFull g@(G {..}) x y = isWall g x y -- || isTail g x y
{-# inline isFull #-}

isWall :: GameState -> Int -> Int -> Bool
isWall G {..} x y = case cells ! (y * levelWidth + x) of
    Wall -> True
    _ -> False
{-# inline isWall #-}

isEmpty :: GameState -> Int -> Int -> Bool
isEmpty g x y = not $ isFull g x y
{-# inline isEmpty #-}

isFruit :: GameState -> Int -> Int -> Bool
isFruit G {..} x y = (x, y) `V.elem` fruits
{-# inline isFruit #-}

data Move = R | U | L | D
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

possibleMoves :: GameState -> [(Move, GameState)]
possibleMoves g = [(m, g') | m <- universe, g' <- applyMove m g]

applyMove :: Move -> GameState -> [GameState]
applyMove R g@(G {..}) =
    let (x, y) = snake
    in if isEmpty g (x + 1) y
        then [g {snake = (x + 1, y)}]
        else []
applyMove L g@(G {..}) =
    let (x, y) = snake
    in if isEmpty g (x - 1) y
        then [g {snake = (x - 1, y)}]
        else []
applyMove _ _ = []

isWon :: GameState -> Bool
isWon G {..} = V.null fruits && Portal == cells ! (y * levelWidth + x)
    where
    (x, y) = snake

instance Game GameState Move where
    isWon = Unpuzzle.Games.Snakebird.isWon
    possibleMoves = Unpuzzle.Games.Snakebird.possibleMoves

parseState' :: Text -> GameState
parseState' (lines -> rows) = g
    where
    g = G {..}
    levelWidth = maximum (map T.length rows) + 4
    levelHeight = length rows + 4
    Just playerY = (2 +) <$> findIndex ("@" `T.isInfixOf`) rows
    Just playerX = (2 +) <$> T.findIndex (== '@') (rows !! (playerY - 2))
    cells' = V.fromList (concatMap
                (map f . T.unpack . pad)
                (replicate 2 (T.replicate (levelWidth - 4) "#")
                <> rows
                <> replicate 2 (T.replicate (levelWidth - 4) "#")))
    cells = V.map (\case
        Fruit -> Empty
        x -> x) cells'
    pad t = "##" <> t <> T.replicate (levelWidth - T.length t - 2) "#"
    fruits = mempty
    snake = (playerX, playerY)
    f ' ' = Empty
    f '@' = Empty
    f '#' = Wall
    f 'F' = Portal
    f '.' = Fruit
    f _ = error "Snakebird.parseState'"
