module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Cards (Card(..), rotate)
import Data.Maybe (isNothing, isJust)
import Data.Array ((!), array, elems, (//))
import Board

spec :: Spec
spec = do
  describe "Board" $ do
    it "sollte ein leeres 3x3 Board erstellen" $ do
      all isNothing (elems emptyBoard) `shouldBe` True  -- Alle Felder leer

    it "sollte die erste Karte unrotiert in die Mitte des Boards legen (remove me)" $ do
      let
        firstCard = Card 's' 'p' 'm' 'k'
      firstCard `on` emptyBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard)])

    it "sollte die erste Karte unrotiert in die Mitte des Boards legen" $ do
      let
        firstCard = Card 's' 'p' 'm' 'k'
        firstBoard :: Maybe Board
        firstBoard = firstCard `on` emptyBoard
      firstBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard)])

    it "sollte die zweite Karte rotiert über die Mitte des Boards legen" $ do
      let
        firstCard = Card 's' 'p' 'm' 'k'
        firstBoard = firstCard `on` emptyBoard
        secondCard = Card 'S' 'P' 'M' 'K'
        secondBoard = firstBoard >>= (secondCard `on`)
      secondBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard), ((1,0), Just (rotate . rotate $ secondCard))])

    it "sollte alle Karten auf das Board legen, wenn sie eine Lösung sind" $ do
      True `shouldBe` True
