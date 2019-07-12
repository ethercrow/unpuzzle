module Unpuzzle.Games.Sokoban where

import qualified Data.Set as S
import Relude
import Relude.Extra.Enum
import qualified Data.Text as T
import Data.Foldable (maximum)
import qualified Data.Vector as V
import Data.List (findIndex, (!!))

data GameState = G
    { levelWidth :: !Int
    , levelHeight :: !Int
    , playerX :: !Int
    , playerY :: !Int
    , cells :: V.Vector Cell
    }
  deriving (Eq, Ord, Show, Generic)

data Cell
  = Wall
  | Empty
  | Crate
  | TargetEmpty
  | TargetCrate
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

fuller :: Cell -> Cell
fuller Empty = Crate
fuller TargetEmpty = TargetCrate
fuller x = x

emptier :: Cell -> Cell
emptier Crate = Empty
emptier TargetCrate = TargetEmpty
emptier x = x

isFull :: Cell -> Bool
isFull Empty = False
isFull TargetEmpty = False
isFull _ = True

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty TargetEmpty = True
isEmpty _ = False

data Move = R | U | L | D
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

reachableStates :: GameState -> [(Move, GameState)]
reachableStates g = [(m, g') | m <- universe, g' <- applyMove m g]

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to input = go [] input
    where
    go acc rest | from `isPrefixOf` rest = go (acc <> to) (drop (length from) rest)
    go acc (c : rest) = go (acc <> [c]) rest
    go acc [] = acc

applyMove :: Move -> GameState -> [GameState]

-- walking
applyMove R G {..}
    | playerX < levelWidth - 1
    && isEmpty (cells V.! (playerY * levelWidth + playerX + 1))
    = [G levelWidth levelHeight (playerX + 1) playerY cells]
applyMove L G {..}
    | playerX > 0
    && isEmpty (cells V.! (playerY * levelWidth + playerX - 1))
    = [G levelWidth levelHeight (playerX - 1) playerY cells]
applyMove D G {..}
    | playerY < levelHeight - 1
    && isEmpty (cells V.! ((playerY + 1) * levelWidth + playerX))
    = [G levelWidth levelHeight playerX (playerY + 1) cells]
applyMove U G {..}
    | playerY > 0
    && isEmpty (cells V.! ((playerY - 1) * levelWidth + playerX))
    = [G levelWidth levelHeight playerX (playerY - 1) cells]

-- pushing
applyMove R G {..}
    | playerX < levelWidth - 2
    && elem (cells V.! (playerY * levelWidth + playerX + 1)) [Crate, TargetCrate]
    && isEmpty (cells V.! (playerY * levelWidth + playerX + 2))
    = let old_near = cells V.! (playerY * levelWidth + playerX + 1)
          old_far = cells V.! (playerY * levelWidth + playerX + 2)
          new_near = emptier old_near
          new_far = fuller old_far
          cells' = V.unsafeUpd cells
            [ (playerY * levelWidth + playerX + 1, new_near)
            , (playerY * levelWidth + playerX + 2, new_far)
            ]
    in [G levelWidth levelHeight (playerX + 1) playerY cells']
applyMove L G {..}
    | playerX > 1
    && elem (cells V.! (playerY * levelWidth + playerX - 1)) [Crate, TargetCrate]
    && isEmpty (cells V.! (playerY * levelWidth + playerX - 2))
    = let old_near = cells V.! (playerY * levelWidth + playerX - 1)
          old_far = cells V.! (playerY * levelWidth + playerX - 2)
          new_near = emptier old_near
          new_far = fuller old_far
          cells' = V.unsafeUpd cells
            [ (playerY * levelWidth + playerX - 1, new_near)
            , (playerY * levelWidth + playerX - 2, new_far)
            ]
    in [G levelWidth levelHeight (playerX - 1) playerY cells']
applyMove D G {..}
    | playerY < levelHeight - 2
    && elem (cells V.! ((playerY + 1) * levelWidth + playerX)) [Crate, TargetCrate]
    && isEmpty (cells V.! ((playerY + 2) * levelWidth + playerX))
    = let old_near = cells V.! ((playerY + 1) * levelWidth + playerX)
          old_far = cells V.! ((playerY + 2) * levelWidth + playerX)
          new_near = emptier old_near
          new_far = fuller old_far
          cells' = V.unsafeUpd cells
            [ ((playerY + 1) * levelWidth + playerX, new_near)
            , ((playerY + 2) * levelWidth + playerX, new_far)
            ]
    in [G levelWidth levelHeight playerX (playerY + 1) cells']
applyMove U G {..}
    | playerY > 1
    && elem (cells V.! ((playerY - 1) * levelWidth + playerX)) [Crate, TargetCrate]
    && isEmpty (cells V.! ((playerY - 2) * levelWidth + playerX))
    = let old_near = cells V.! ((playerY - 1) * levelWidth + playerX)
          old_far = cells V.! ((playerY - 2) * levelWidth + playerX)
          new_near = emptier old_near
          new_far = fuller old_far
          cells' = V.unsafeUpd cells
            [ ((playerY - 1) * levelWidth + playerX, new_near)
            , ((playerY - 2) * levelWidth + playerX, new_far)
            ]
    in [G levelWidth levelHeight playerX (playerY - 1) cells']
applyMove _ _ = []

isLost :: GameState -> Bool
isLost G {..} = V.any identity $ V.imap foo cells
    where
    foo i Crate =
        let (y, x) = i `quotRem` levelWidth
        in (x > 0 && y > 0
            && Wall == cells V.! ((y - 1) * levelWidth + x) 
            && Wall == cells V.! (y * levelWidth + (x - 1)))
            ||
           (x > 0 && y < levelHeight - 2
            && Wall == cells V.! ((y + 1) * levelWidth + x) 
            && Wall == cells V.! (y * levelWidth + (x - 1)))
            ||
           (x < levelWidth - 2 && y > 0 
            && Wall == cells V.! ((y - 1) * levelWidth + x) 
            && Wall == cells V.! (y * levelWidth + (x + 1)))
            ||
           (x < levelWidth - 2 && y < levelHeight - 2
            && Wall == cells V.! ((y + 1) * levelWidth + x) 
            && Wall == cells V.! (y * levelWidth + (x + 1)))
    foo _ _ = False

isWon :: GameState -> Bool
isWon G {..} =
  null $
    V.filter
      (== Crate)
      cells

solve'dfs :: GameState -> [(GameState, [Move])]
solve'dfs g0 = go [] mempty g0
  where
    go :: [Move] -> S.Set GameState -> GameState -> [(GameState, [Move])]
    go _ visited g | S.member g visited = []
    go moves _ g | isWon g = [(g, moves)]
    go _ _ g | isLost g = []
    go moves visited g =
      let visited' = S.singleton g <> visited
       in concatMap (\(m_, g_) -> go (m_ : moves) visited' g_) (reachableStates g)

solve :: GameState -> [(GameState, [Move])]
solve g0 = wave [(g0, [])] mempty
  where
    wave :: [(GameState, [Move])] -> Set GameState -> [(GameState, [Move])]
    wave [] _visited = []
    wave front visited =
        let wins = filter (isWon . fst) front
            visited' = visited <> S.fromList (map fst front)
            front' = [(s, m : ms) | (x, ms) <- front, m <- universe, s <- applyMove m x, not (S.member s visited'), not (isLost x)]
        in case wins of
            [] -> wave front' visited'
            _ -> trace ("Visited " <> show (length visited) <> " states") wins

-- renderState :: GameState -> Text
-- renderState G{..} = unlines $ map renderRow (r:rest)

-- renderRow :: Vector Cell -> Text
-- renderRow = T.pack . map \case
--     You -> '@'
--     Wall -> '#'
--     Crate -> '$'
--     TargetEmpty -> '.'
--     TargetCrate -> '0'
--     Empty -> ' '

parseState' :: Text -> GameState
parseState' (lines -> rows) = G {..}
    where
    levelWidth = maximum (map T.length rows)
    levelHeight = length rows
    Just playerY = findIndex ("@" `T.isInfixOf`) rows
    Just playerX = T.findIndex (== '@') (rows !! playerY)
    cells = V.fromList (concatMap (map f . T.unpack . rightPad) rows)
    rightPad t = t <> T.replicate (levelWidth - T.length t) "#"
    f '@' = Empty
    f '#' = Wall
    f '$' = Crate
    f '.' = TargetEmpty
    f '0' = TargetCrate
    f ' ' = Empty
    f _ = error "Sokoban.parseState'"
