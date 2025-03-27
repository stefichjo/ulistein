{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Main where

import Test.Hspec (hspec)
import qualified BoardSpec (spec)
import qualified CardSpec (spec)

main :: IO ()
main = hspec $ do
  BoardSpec.spec
  CardSpec.spec
