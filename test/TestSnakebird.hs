{-# language QuasiQuotes #-}

module TestSnakebird where

import Relude hiding (head)
import Relude.Unsafe
-- import Relude.Extra.Enum

import NeatInterpolation

import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck

import Unpuzzle.Generic (solve)
import Unpuzzle.Games.Snakebird

-- instance Arbitrary GameState where
--     arbitrary = do
--         w <- choose (3, 20)
--         h <- choose (3, 20)
--         rows <- replicateM h (T.pack <$> replicateM w (elements "@$. #"))
--         G <$> arbitrary <*> pure rows

-- prop_parse_render_roundtrip :: GameState -> Property
-- prop_parse_render_roundtrip s = parseState' (renderState s) === s

unit_snakebird_simple_r =
    let level = parseState' "@F"
    in [R] @=? snd (head $ solve level)

unit_snakebird_simple_rrr =
    let level = parseState' "@. F"
    in [R, R, R] @=? snd (head $ solve level)

unit_snakebird_simple_l =
    let level = parseState' "F@"
    in [L] @=? snd (head $ solve level)

unit_snakebird_simple_lll =
    let level = parseState' "F. @"
    in [L, L, L] @=? snd (head $ solve level)

