module Unpuzzle.Games.Sokoban where

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
    , playerX :: !Int
    , playerY :: !Int
    , cells :: V.Vector Int8
    , crates :: V.Vector (Int, Int)
    , corners :: V.Vector (Int, Int)
    }
  deriving (Show, Generic)

instance Eq GameState where
    g1 == g2 =
        playerX g1 == playerX g2
        &&
        playerY g1 == playerY g2
        &&
        crates g1 == crates g2

instance Hashable GameState where
    hashWithSalt salt G {..} = hashWithSalt salt (playerX, playerY, crates)
    {-# inline hashWithSalt #-}

pattern Empty :: Int8
pattern Empty = 0
pattern Wall :: Int8
pattern Wall = 1
pattern Crate :: Int8
pattern Crate = 2
pattern TargetEmpty :: Int8
pattern TargetEmpty = 3
pattern TargetCrate :: Int8
pattern TargetCrate = 4

type Cell = Int8

isFull :: GameState -> Int -> Int -> Bool
isFull g@(G {..}) x y = isWall g x y || isCrate g x y
{-# inline isFull #-}

isWall :: GameState -> Int -> Int -> Bool
isWall G {..} x y = case cells V.! (y * levelWidth + x) of
    Wall -> True
    _ -> False
{-# inline isWall #-}

isEmpty :: GameState -> Int -> Int -> Bool
isEmpty g x y = not $ isFull g x y
{-# inline isEmpty #-}

isCrate :: GameState -> Int -> Int -> Bool
isCrate G {..} x y = (x, y) `V.elem` crates
{-# inline isCrate #-}

data Move = R | U | L | D
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

possibleMoves :: GameState -> [(Move, GameState)]
possibleMoves g = [(m, g') | m <- universe, g' <- applyMove m g]

applyMove :: Move -> GameState -> [GameState]

applyMove R g@(G {..})
    | playerX < levelWidth - 1
    && isEmpty g (playerX + 1) playerY
    = [G levelWidth levelHeight (playerX + 1) playerY cells crates corners]
applyMove R g@(G {..})
    | playerX < levelWidth - 2
    && isCrate g (playerX + 1) playerY
    && isEmpty g (playerX + 2) playerY
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX + 1, cy == playerY -> (cx + 1, cy)
            c -> c) crates
          g' = G levelWidth levelHeight (playerX + 1) playerY cells crates' corners
    in [g' | not $ isStuck g' (playerX + 2) playerY]

applyMove L g@(G {..})
    | playerX > 0
    && isEmpty g (playerX - 1) playerY
    = [G levelWidth levelHeight (playerX - 1) playerY cells crates corners]
applyMove L g@(G {..})
    | playerX > 1
    && isCrate g (playerX - 1) playerY
    && isEmpty g (playerX - 2) playerY
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX - 1, cy == playerY -> (cx - 1, cy)
            c -> c) crates
          g' = G levelWidth levelHeight (playerX - 1) playerY cells crates' corners
    in [g' | not $ isStuck g' (playerX - 2) playerY]

applyMove D g@(G {..})
    | playerY < levelHeight - 1
    && isEmpty g playerX (playerY + 1)
    = [G levelWidth levelHeight playerX (playerY + 1) cells crates corners]
applyMove D g@(G {..})
    | playerY < levelHeight - 2
    && isCrate g playerX (playerY + 1)
    && isEmpty g playerX (playerY + 2)
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX, cy == playerY + 1 -> (cx, cy + 1)
            c -> c) crates
          g' = G levelWidth levelHeight playerX (playerY + 1) cells crates' corners
    in [g' | not $ isStuck g' playerX (playerY + 2)]

applyMove U g@(G {..})
    | playerY > 0
    && isEmpty g playerX (playerY - 1)
    = [G levelWidth levelHeight playerX (playerY - 1) cells crates corners]
applyMove U g@(G {..})
    | playerY > 1
    && isCrate g playerX (playerY - 1)
    && isEmpty g playerX (playerY - 2)
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX, cy == playerY - 1 -> (cx, cy - 1)
            c -> c) crates
          g' = G levelWidth levelHeight playerX (playerY - 1) cells crates' corners
    in [g' | not $ isStuck g' playerX (playerY - 2)]
applyMove _ _ = []

isLost :: GameState -> Bool
isLost g = V.any (uncurry $ isStuck g) (crates g)

isStuck :: GameState -> Int -> Int -> Bool
isStuck g@(G {..}) x y = 
   (V.elem (x, y) corners)
    ||
   (x > 0 && y > 0
    && isFull g x (y - 1)
    && isFull g (x - 1) (y - 1)
    && isFull g (x - 1) y)
    ||
   (x > 0 && y < levelHeight - 2
    && isFull g x (y + 1)
    && isFull g (x - 1) (y + 1)
    && isFull g (x - 1) y)
    ||
   (x < levelWidth - 2 && y > 0 
    && isFull g x (y - 1)
    && isFull g (x + 1) (y - 1)
    && isFull g (x + 1) y)
    ||
   (x < levelWidth - 2 && y < levelHeight - 2
    && isFull g x (y + 1)
    && isFull g (x + 1) (y + 1)
    && isFull g (x + 1) y)

isWon :: GameState -> Bool
isWon G {..} = V.all (\(x,y) -> TargetEmpty == cells V.! (y * levelWidth + x)) crates

instance Game GameState Move where
    isWon = Unpuzzle.Games.Sokoban.isWon
    possibleMoves = Unpuzzle.Games.Sokoban.possibleMoves

parseState' :: Text -> GameState
parseState' (lines -> rows) = g
    where
    g = G {..}
    levelWidth = maximum (map T.length rows)
    levelHeight = length rows
    Just playerY = findIndex ("@" `T.isInfixOf`) rows
    Just playerX = T.findIndex (== '@') (rows !! playerY)
    cells' = V.fromList (concatMap (map f . T.unpack . rightPad) rows)
    crates = V.map (swap . (`quotRem` levelWidth)) $ V.findIndices (`elem` [Crate, TargetCrate]) cells'
    cells = V.map (\case
        Crate -> Empty
        TargetCrate -> TargetEmpty
        x -> x) cells'
    isCorner ((x, y), Empty) =
       (x > 0 && y > 0
        && isWall g x (y - 1)
        && isWall g (x - 1) y)
        ||
       (x > 0 && y < levelHeight - 2
        && isWall g x (y + 1)
        && isWall g (x - 1) y)
        ||
       (x < levelWidth - 2 && y > 0 
        && isWall g x (y - 1)
        && isWall g (x + 1) y)
        ||
       (x < levelWidth - 2 && y < levelHeight - 2
        && isWall g x (y + 1)
        && isWall g (x + 1) y)
    isCorner _ = False
    corners =
        V.map fst
        $ V.filter isCorner
        $ V.imap (\i c -> (swap (i `quotRem` levelWidth), c)) cells
    rightPad t = t <> T.replicate (levelWidth - T.length t) "#"
    f '@' = Empty
    f '#' = Wall
    f '$' = Crate
    f '.' = TargetEmpty
    f '0' = TargetCrate
    f ' ' = Empty
    f _ = error "Sokoban.parseState'"
