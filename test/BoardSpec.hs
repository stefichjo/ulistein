{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Cards (Card(..))
import Data.Maybe (isNothing, isJust)
import Data.Array ((!), array, elems, (//))
import Board

spec :: Spec
spec = do
  describe "Board" $ do
    it "sollte ein leeres 3x3 Board erstellen" $ do
      let board = emptyBoard
      all isNothing (elems board) `shouldBe` True  -- Alle Felder leer

    it "sollte die erste Karte unrotiert in die Mitte des Boards legen" $ do
      let firstCard = Card 's' 'p' 'm' 'k'
      let boardWithFirstCard = array ((0,0), (2,2)) [
              ((0,0), Nothing), ((0,1), Nothing), ((0,2), Nothing),
              ((1,0), Nothing), ((1,1), Just firstCard), ((1,2), Nothing),
              ((2,0), Nothing), ((2,1), Nothing), ((2,2), Nothing)
            ]
      (firstCard `on` emptyBoard) `shouldBe` boardWithFirstCard

    it "sollte die erste Karte auf einem (leeren) Board platzieren" $ do
      let firstCard = Card 's' 'p' 'm' 'k'
      let boardWithFirstCard = array ((0,0), (2,2)) [
              ((0,0), Nothing), ((0,1), Nothing), ((0,2), Nothing),
              ((1,0), Nothing), ((1,1), Just firstCard), ((1,2), Nothing),
              ((2,0), Nothing), ((2,1), Nothing), ((2,2), Nothing)
            ]
      (firstCard `on` emptyBoard) `shouldBe` boardWithFirstCard

    it "sollte die zweite Karte auf dem Board platzieren" $ do
      let board = Card 's' 'p' 'm' 'k' `on` emptyBoard
      let nextCard = Card 'S' 'P' 'M' 'K'
      nextCard `on` board `shouldBe` board // [((1, 0), Just nextCard)]
