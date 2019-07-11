module TestSokoban where

import Relude
import Relude.Extra.Enum

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Unpuzzle.Games.Sokoban

unit_soko_1r = solve (G [[You, Crate, TargetEmpty]]) @=? [(G [[Empty, You, TargetCrate]], [R])]

unit_soko_1d = solve (G [[You], [Crate], [TargetEmpty]]) @=? [(G [[Empty], [You], [TargetCrate]], [D])]

instance Arbitrary Cell where
    arbitrary = elements universe

instance Arbitrary GameState where
    arbitrary = do
        w <- choose (3, 20)
        h <- choose (3, 20)
        G <$> (replicateM h $ replicateM w arbitrary)

prop_parse_render_roundtrip :: GameState -> Property
prop_parse_render_roundtrip s = parseState' (renderState s) === s
