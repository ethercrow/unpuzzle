module Unpuzzle.Games.Sokoban where

import qualified Data.Set as S
import Relude
import Relude.Extra.Enum
import qualified Data.Text as T

data GameState = G [[Cell]]
  deriving (Eq, Ord, Show, Generic)

data Cell
  = You
  | Wall
  | Empty
  | Crate
  | TargetEmpty
  | TargetCrate
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

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
applyMove R (G rows) =
    [G $ map (replace [You, Empty] [Empty, You] . replace [You, Crate, TargetEmpty] [Empty, You, TargetCrate] . replace [You, Crate, Empty] [Empty, You, Crate]) rows]
applyMove L (G rows) =
    [G $ map (replace [Empty, You] [You, Empty] . replace [TargetEmpty, Crate, You] [TargetCrate, You, Empty] . replace [Empty, Crate, You] [Crate, You, Empty]) rows]
applyMove D (G (transpose -> columns)) =
    [G $ transpose $ map (replace [You, Empty] [Empty, You] . replace [You, Crate, TargetEmpty] [Empty, You, TargetCrate] . replace [You, Crate, Empty] [Empty, You, Crate]) columns]
applyMove U (G (transpose -> columns)) =
    [G $ transpose $ map (replace [Empty, You] [You, Empty] . replace [TargetEmpty, Crate, You] [TargetCrate, You, Empty] . replace [Empty, Crate, You] [Crate, You, Empty]) columns]

isLost :: GameState -> Bool
isLost = const False

isWon :: GameState -> Bool
isWon (G g) =
  null $
    filter
      (`elem` [TargetEmpty, Crate])
      (concat g)

solve :: GameState -> [(GameState, [Move])]
solve g0 = go [] mempty g0
  where
    go :: [Move] -> S.Set GameState -> GameState -> [(GameState, [Move])]
    go _ visited g | S.member g visited = []
    go moves _ g | isWon g = [(g, moves)]
    go _ _ g | isLost g = []
    go moves visited g =
      let visited' = S.singleton g <> visited
       in concatMap (\(m_, g_) -> go (m_ : moves) visited' g_) (reachableStates g)

renderState :: GameState -> Text
renderState (G (r:rest)) = unlines $
    [T.pack ('-' <$ r)]
    <> map renderRow (r:rest)
renderState _ = ""

renderRow :: [Cell] -> Text
renderRow = T.pack . map \case
    You -> 'Y'
    Wall -> '|'
    Crate -> 'O'
    TargetEmpty -> '.'
    TargetCrate -> '0'
    Empty -> ' '

parseState' :: Text -> GameState
parseState' (drop 1 . lines -> rows) = G (map (map f . T.unpack) rows)
    where
    f 'Y' = You
    f '|' = Wall
    f 'O' = Crate
    f '.' = TargetEmpty
    f '0' = TargetCrate
    f ' ' = Empty
