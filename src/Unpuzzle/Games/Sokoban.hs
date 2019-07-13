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
    , deadCells :: V.Vector (Int, Int)
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

(!) :: V.Unbox a => V.Vector a -> Int -> a
v ! i = v `V.unsafeIndex` i -- without bounds checking
-- v ! i = v V.! i -- with bounds checking

type Cell = Int8

isFull :: GameState -> Int -> Int -> Bool
isFull g@(G {..}) x y = isWall g x y || isCrate g x y
{-# inline isFull #-}

isWall :: GameState -> Int -> Int -> Bool
isWall G {..} x y = case cells ! (y * levelWidth + x) of
    Wall -> True
    _ -> False
{-# inline isWall #-}

isEmpty :: GameState -> Int -> Int -> Bool
isEmpty g x y = not $ isFull g x y
{-# inline isEmpty #-}

isDead :: GameState -> Int -> Int -> Bool
isDead g x y = (x, y) `V.elem` deadCells g
{-# inline isDead #-}

isCrate :: GameState -> Int -> Int -> Bool
isCrate G {..} x y = (x, y) `V.elem` crates
{-# inline isCrate #-}

data Move = R | U | L | D
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

possibleMoves :: GameState -> [(Move, GameState)]
possibleMoves g = [(m, g') | m <- universe, g' <- applyMove m g]

applyMove :: Move -> GameState -> [GameState]

applyMove R g@(G {..})
    | isEmpty g (playerX + 1) playerY
    = [G levelWidth levelHeight (playerX + 1) playerY cells crates deadCells]
applyMove R g@(G {..})
    | isCrate g (playerX + 1) playerY
    && isEmpty g (playerX + 2) playerY
    && not (isDead g (playerX + 2) playerY)
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX + 1, cy == playerY -> (cx + 1, cy)
            c -> c) crates
          g' = G levelWidth levelHeight (playerX + 1) playerY cells crates' deadCells
    in [g']

applyMove L g@(G {..})
    | isEmpty g (playerX - 1) playerY
    = [G levelWidth levelHeight (playerX - 1) playerY cells crates deadCells]
applyMove L g@(G {..})
    | isCrate g (playerX - 1) playerY
    && isEmpty g (playerX - 2) playerY
    && not (isDead g (playerX - 2) playerY)
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX - 1, cy == playerY -> (cx - 1, cy)
            c -> c) crates
          g' = G levelWidth levelHeight (playerX - 1) playerY cells crates' deadCells
    in [g']

applyMove D g@(G {..})
    | playerY < levelHeight - 1
    && isEmpty g playerX (playerY + 1)
    = [G levelWidth levelHeight playerX (playerY + 1) cells crates deadCells]
applyMove D g@(G {..})
    | playerY < levelHeight - 2
    && isCrate g playerX (playerY + 1)
    && isEmpty g playerX (playerY + 2)
    && not (isDead g playerX (playerY + 2))
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX, cy == playerY + 1 -> (cx, cy + 1)
            c -> c) crates
          g' = G levelWidth levelHeight playerX (playerY + 1) cells crates' deadCells
    in [g']

applyMove U g@(G {..})
    | playerY > 0
    && isEmpty g playerX (playerY - 1)
    = [G levelWidth levelHeight playerX (playerY - 1) cells crates deadCells]
applyMove U g@(G {..})
    | playerY > 1
    && isCrate g playerX (playerY - 1)
    && isEmpty g playerX (playerY - 2)
    && not (isDead g playerX (playerY - 2))
    = let crates' = V.map (\case
            (cx, cy) | cx == playerX, cy == playerY - 1 -> (cx, cy - 1)
            c -> c) crates
          g' = G levelWidth levelHeight playerX (playerY - 1) cells crates' deadCells
    in [g']
applyMove _ _ = []

isWon :: GameState -> Bool
isWon G {..} = V.all (\(x,y) -> TargetEmpty == cells ! (y * levelWidth + x)) crates

instance Game GameState Move where
    isWon = Unpuzzle.Games.Sokoban.isWon
    possibleMoves = Unpuzzle.Games.Sokoban.possibleMoves

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
                (replicate 2 (T.replicate (levelWidth - 4) "#") <> rows <> replicate 2 (T.replicate (levelWidth - 4) "#")))
    crates = V.map (swap . (`quotRem` levelWidth)) $ V.findIndices (`elem` [Crate, TargetCrate]) cells'
    cells = V.map (\case
        Crate -> Empty
        TargetCrate -> TargetEmpty
        x -> x) cells'
    isCorner ((x, y), Empty) =
       (isWall g x (y - 1)
        && isWall g (x - 1) y)
        ||
       (isWall g x (y + 1)
        && isWall g (x - 1) y)
        ||
       (isWall g x (y - 1)
        && isWall g (x + 1) y)
        ||
       (isWall g x (y + 1)
        && isWall g (x + 1) y)
    isCorner _ = False
    initial_corners =
        V.map fst
        $ V.filter isCorner
        $ V.imap (\i c -> (swap (i `quotRem` levelWidth), c)) cells
    paired_corners_v =
        [ (c1, c2)
        | c1@(x1, y1) <- V.toList initial_corners
        , c2@(x2, y2) <- V.toList initial_corners
        , x1 == x2, y1 < y2
        ]
    more_dead_cells_v =
        [ (x, yy) 
        | ((x, y1), (_, y2)) <- paired_corners_v
        , yy <- [y1 + 1 .. y2 - 1]
        , all (\y -> Empty == cells ! (y * levelWidth + x)) [y1 .. y2]
             && ((x > 0 && all (\y -> Wall == cells ! (y * levelWidth + x - 1)) [y1 .. y2])
                ||
                 (x < levelWidth - 2 && all (\y -> Wall == cells ! (y * levelWidth + x + 1)) [y1 .. y2]))

        ]
    paired_corners_h =
        [ (c1, c2)
        | c1@(x1, y1) <- V.toList initial_corners
        , c2@(x2, y2) <- V.toList initial_corners
        , y1 == y2, x1 < x2
        ]
    more_dead_cells_h =
        [ (xx, y)
        | ((x1, y), (x2, _)) <- paired_corners_h
        , xx <- [x1 + 1 .. x2 - 1]
        , all (\x -> Empty == cells ! (y * levelWidth + x)) [x1 .. x2]
             && ((y > 0 && all (\x -> Wall == cells ! (pred y * levelWidth + x)) [x1 .. x2])
                ||
                 (y < levelHeight - 2 && all (\x -> Wall == cells ! (succ y * levelWidth + x)) [x1 .. x2]))

        ]
    deadCells = initial_corners <> V.fromList more_dead_cells_h <> V.fromList more_dead_cells_v
    pad t = "##" <> t <> T.replicate (levelWidth - T.length t - 2) "#"
    f '@' = Empty
    f '#' = Wall
    f '$' = Crate
    f '.' = TargetEmpty
    f '0' = TargetCrate
    f ' ' = Empty
    f _ = error "Sokoban.parseState'"
