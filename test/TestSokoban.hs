{-# language QuasiQuotes #-}

module TestSokoban where

import Relude hiding (head)
import Relude.Unsafe
-- import Relude.Extra.Enum

import NeatInterpolation

import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck

import Unpuzzle.Generic
import Unpuzzle.Games.Sokoban

-- instance Arbitrary GameState where
--     arbitrary = do
--         w <- choose (3, 20)
--         h <- choose (3, 20)
--         rows <- replicateM h (T.pack <$> replicateM w (elements "@$. #"))
--         G <$> arbitrary <*> pure rows

-- prop_parse_render_roundtrip :: GameState -> Property
-- prop_parse_render_roundtrip s = parseState' (renderState s) === s

unit_soko_simple =
    let level = parseState' "#@$.#"
    in [R] @=? snd (head $ solve level)

unit_soko_simple_2 =
    let level = parseState' "#@ $.#"
    in length (take 1 $ solve level) @=? 1

unit_soko_simple_3 =
    let level = parseState' "#. $@#"
    in length (take 1 $ solve level) @=? 1

unit_soko_simple_4 =
    let level = parseState' ".$@"
    in length (take 1 $ solve level) @=? 1

unit_soko_simple_5 =
    let level = parseState' "@$."
    in length (take 1 $ solve level) @=? 1

unit_soko_simple_vertical =
    let level = parseState' [text|
        ###
        #@#
        #$#
        #.#
        ###
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_simple_vertical_2 =
    let level = parseState' [text|
        ###
        #@#
        # #
        #$#
        # #
        #.#
        ###
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_simple_vertical_3 =
    let level = parseState' [text|
        #.#
        # #
        #$#
        #@#
        ###
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_simple_vertical_5 =
    let level = parseState' [text|
        .
        $
         
        @
        #
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_simple_vertical_4 =
    let level = parseState' [text|
        #.#
        # #
        #$#
        # #
        #@#
        ###
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_simple_corners =
    let level = parseState' [text|
        #####
        # $.#
        # #
        #@#
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_simple_corners_2 =
    let level = parseState' [text|
        #####
        #   #
        # #$#
        #@#.#
        |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_original_1_1 =
    let level = parseState' [text|
                    #####
                    #   #
                    #   #
                  ###   ##
                  #      #
                ### # ## #   ######
                #   # ## #####  ..#
                #    $          ..#
                ##### ### #@##  ..#
                    #     #########
                    #######
                |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_original_1_2 =
    let level = parseState' [text|
                    #####
                    #   #
                    #   #
                  ###   ##
                  #      #
                ### # ## #   ######
                #   # ## #####  ..#
                # $  $          ..#
                ##### ### #@##  ..#
                    #     #########
                    #######
                |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_original_1_3 =
    let level = parseState' [text|
                    #####
                    #   #
                    #   #
                  ###   ##
                  #    $ #
                ### # ## #   ######
                #   # ## #####  ..#
                # $  $          ..#
                ##### ### #@##  ..#
                    #     #########
                    #######
                |]
    in 1 @=? length (take 1 $ solve level)

unit_soko_original_1_4 =
    let level = parseState' [text|
                    #####
                    #   #
                    #   #
                  ###   ##
                  #  $ $ #
                ### # ## #   ######
                #   # ## #####  ..#
                # $  $          ..#
                ##### ### #@##  ..#
                    #     #########
                    #######
                |]
    in 1 @=? length (take 1 $ solve level)

-- unit_soko_original_1 =
--     let level = parseState' [text|
--                     #####
--                     #   #
--                     #$  #
--                   ###  $##
--                   #  $ $ #
--                 ### # ## #   ######
--                 #   # ## #####  ..#
--                 # $  $          ..#
--                 ##### ### #@##  ..#
--                     #     #########
--                     #######
--                 |]
--     in 1 @=? length (take 1 $ solve level)
