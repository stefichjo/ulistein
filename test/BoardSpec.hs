{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, shouldContain )
import Cards ( Card(..), rotate, allCards )
import Data.Maybe (isNothing, isJust)
import Data.Array ((!), array, elems, (//))
import Board
import Data.Foldable (Foldable(..))

c0 :: Card
c1 :: Card
c2 :: Card
c3 :: Card
c4 :: Card
c5 :: Card
c6 :: Card
c7 :: Card
c8 :: Card
[c0, c1, c2, c3, c4, c5, c6, c7, c8] = allCards

solution247861053 :: Maybe Board
solution247861053 = toSolution [c2, c4, c7, c8, c6, c1, c0, c5, c3]

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
        firstCard = c2
        firstBoard = firstCard `on` emptyBoard
      firstBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard)])

    it "sollte die zweite Karte rotiert über die Mitte des Boards legen" $ do
      let
        firstCard = c2
        secondCard = c4
        secondBoard = Just emptyBoard
          >>= (firstCard `on`)
          >>= (secondCard `on`)
      secondBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard), ((0,1), Just (rotate . rotate $ secondCard))])

    it "sollte alle Karten (247861053) auf das Board legen, da sie eine Lösung sind" $ do
      solution247861053 `shouldBe` Just (Just <$> array ((0,0), (2,2)) [
          ((0,0), rotate c3), ((0,1), rotate . rotate $ c4), ((0,2), rotate . rotate $ c1),
          ((1,0), rotate . rotate $ c6), ((1,1), c2), ((1,2), rotate c7),
          ((2,0), rotate . rotate $ c5), ((2,1), rotate . rotate $ c8), ((2,2), c0)
        ])

    it "sollte alle Lösungen (7) finden" $ do
      solutions `shouldContain` toList solution247861053
      length solutions `shouldBe` 7

