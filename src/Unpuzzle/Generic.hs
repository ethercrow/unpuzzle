module Unpuzzle.Generic where

import qualified Data.HashSet as S
import Relude

class (Eq state, Hashable state) => Game state move | state -> move where
    isWon :: state -> Bool
    possibleMoves :: state -> [(move, state)]

solve'bfs :: forall gstate move. (Game gstate move) => gstate -> [(gstate, [move])]
solve'bfs g0 = wave [(g0, [])] mempty
  where
    wave :: [(gstate, [move])] -> HashSet gstate -> [(gstate, [move])]
    wave [] _visited = []
    wave front visited =
        let wins = filter (isWon . fst) front
            visited' = visited <> S.fromList (map fst front)
            front' = [(s, m : ms) | (x, ms) <- front, (m, s) <- possibleMoves x, not (S.member s visited')]
        in case wins of
            [] -> wave front' visited'
            _ -> trace ("Visited " <> show (length visited) <> " states") wins


solve :: forall gstate move. (Game gstate move) => gstate -> [(gstate, [move])]
solve = solve'bfs
