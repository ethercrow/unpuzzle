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

unit_snakebird_simple_r =
    let level = parseState' "RE"
    in [R] @=? snd (head $ solve level)

unit_snakebird_simple_rrr =
    let level = parseState' "RF E"
    in [R, R, R] @=? snd (head $ solve level)

unit_snakebird_simple_l =
    let level = parseState' "ER"
    in [L] @=? snd (head $ solve level)

unit_snakebird_simple_lll =
    let level = parseState' "EF R"
    in [L, L, L] @=? snd (head $ solve level)

unit_snakebird_simple_u =
    let level = parseState' [text|
        E
        R
        w
        |]
    in [U] @=? snd (head $ solve level)

unit_snakebird_simple_d =
    let level = parseState' [text|
       >R
       wE
        |]
    in [D] @=? snd (head $ solve level)

unit_snakebird_star_2 = 
    let level = parseState' [text|
              wwwwwww
              w F F ww
             wwFwFwFww www
             wFFFFFFFwwwwww
            ww wFwFw R<< Ew
            wwFFFFFFFwwwww
            wwwFwFwFwwww
              w F F w
               wwwwww
               wwwwww
               |]
    in [R,R,R,R,R,U,R,U,U,R,R,D,D,L,D,D,R,R,U,U,R,R,D,D,L,D,D,L,L,L,L,U,U,L,U,U,R,U,U,R,R,R,R,D,D,R,D,D,L,L,L,D,D,L,L,U,U,L,U,L] @=? snd (head (solve level))
