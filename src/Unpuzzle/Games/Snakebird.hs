module Unpuzzle.Games.Snakebird where

-- TODO: multiple snakes
-- TODO: gravity
-- TODO: spikes
-- TODO: pushable objects

import Relude hiding (init)
import Relude.Unsafe (init)
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
    , snake :: [(Int, Int)]
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
pattern Exit :: Int8
pattern Exit = 2
pattern Fruit :: Int8
pattern Fruit = 3
pattern Tail :: Int8
pattern Tail = 4

(!) :: V.Unbox a => V.Vector a -> Int -> a
-- v ! i = v `V.unsafeIndex` i -- without bounds checking
v ! i = v V.! i -- with bounds checking

type Cell = Int8

isFull :: GameState -> Int -> Int -> Bool
isFull g@(G {..}) x y = isWall g x y || isTail g x y
{-# inline isFull #-}

isTail :: GameState -> Int -> Int -> Bool
isTail g@(G {..}) x y = elem (x, y) snake
{-# inline isTail #-}

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
    let ((x, y) : _) = snake
    in if isFruit g (x + 1) y
       then [g { snake = (x + 1, y) : snake
               , fruits = V.filter (/= (x + 1, y)) fruits
               }]
       else if isEmpty g (x + 1) y
       then [g { snake = (x + 1, y) : init snake}]
       else []
applyMove L g@(G {..}) =
    let ((x, y) : _) = snake
    in if isFruit g (x - 1) y
       then [g { snake = (x - 1, y) : snake
               , fruits = V.filter (/= (x - 1, y)) fruits
               }]
       else if isEmpty g (x - 1) y
       then [g { snake = (x - 1, y) : init snake}]
       else []
applyMove U g@(G {..}) =
    let ((x, y) : _) = snake
    in if isFruit g x (y - 1)
       then [g { snake = (x, y - 1) : snake
               , fruits = V.filter (/= (x, y - 1)) fruits
               }]
       else if isEmpty g x (y - 1)
       then [g { snake = (x, y - 1) : init snake}]
       else []
applyMove D g@(G {..}) =
    let ((x, y) : _) = snake
    in if isFruit g x (y + 1)
       then [g { snake = (x, y + 1) : snake
               , fruits = V.filter (/= (x, y + 1)) fruits
               }]
       else if isEmpty g x (y + 1)
       then [g { snake = (x, y + 1) : init snake}]
       else []
applyMove _ _ = []

isWon :: GameState -> Bool
isWon G {..} = V.null fruits && Exit == cells ! (y * levelWidth + x)
    where
    ((x, y) : _) = snake

instance Game GameState Move where
    isWon = Unpuzzle.Games.Snakebird.isWon
    possibleMoves = Unpuzzle.Games.Snakebird.possibleMoves

parseState' :: Text -> GameState
parseState' (lines -> rows) = g
    where
    g = G {..}
    levelWidth = maximum (map T.length rows) + 4
    levelHeight = length rows + 4
    Just playerY = (2 +) <$> findIndex ("R" `T.isInfixOf`) rows
    Just playerX = (2 +) <$> T.findIndex (== 'R') (rows !! (playerY - 2))
    cells' = V.fromList (concatMap
                (map f . T.unpack . pad)
                (replicate 2 (T.replicate (levelWidth - 4) "#")
                <> rows
                <> replicate 2 (T.replicate (levelWidth - 4) "#")))
    cells = V.map (\case
        Fruit -> Empty
        Tail -> Empty
        x -> x) cells'
    pad t = "##" <> t <> T.replicate (levelWidth - T.length t - 2) "#"
    fruits = V.map (swap . (`quotRem` levelWidth)) $ V.elemIndices Fruit cells'
    snake = parseSnake [(playerX, playerY)]
    parseSnake ((x, y) : rest) | cells' ! (y * levelWidth + x + 1) == Tail && notElem (x + 1, y) rest = parseSnake ((x + 1, y) : (x, y) : rest)
    parseSnake ((x, y) : rest) | cells' ! (y * levelWidth + x - 1) == Tail && notElem (x - 1, y) rest = parseSnake ((x - 1, y) : (x, y) : rest)
    parseSnake ((x, y) : rest) | cells' ! (succ y * levelWidth + x) == Tail && notElem (x, succ y) rest = parseSnake ((x, succ y) : (x, y) : rest)
    parseSnake ((x, y) : rest) | cells' ! (pred y * levelWidth + x) == Tail && notElem (x, pred y) rest = parseSnake ((x, pred y) : (x, y) : rest)
    parseSnake acc = reverse acc
    f ' ' = Empty
    f '>' = Tail
    f '<' = Tail
    f 'v' = Tail
    f '^' = Tail
    f 'R' = Empty
    f '#' = Wall
    f 'w' = Wall
    f 'E' = Exit
    f 'F' = Fruit
    f _ = error "Snakebird.parseState'"
