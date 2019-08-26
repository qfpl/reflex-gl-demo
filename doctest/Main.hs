module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Data/Clamped.hs", "src/Data/Wraparound.hs"]
