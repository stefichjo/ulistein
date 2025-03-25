{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Cards ( Card(..), rotate, allCards )
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
        firstCard = allCards !! 2
        firstBoard = firstCard `on` emptyBoard
      firstBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard)])

    it "sollte die zweite Karte rotiert über die Mitte des Boards legen" $ do
      let
        firstCard = allCards !! 2
        secondCard = allCards !! 4
        secondBoard = Just emptyBoard
          >>= (firstCard `on`)
          >>= (secondCard `on`)
      secondBoard `shouldBe` Just (emptyBoard // [((1,1), Just firstCard), ((0,1), Just (rotate . rotate $ secondCard))])

    it "sollte alle Karten (2 4786 1053) auf das Board legen, da sie eine Lösung sind" $ do
      let
        [c0, c1, c2, c3, c4, c5, c6, c7, c8] = allCards
        solution = Just emptyBoard
          -- center
          >>= on c2

          -- edges
          >>= on c4
          >>= on c7
          >>= on c8
          >>= on c6

          -- corners
          >>= on c1
          >>= on c0
          >>= on c5
          >>= on c3

      solution `shouldBe` Just (Just <$> array ((0,0), (2,2)) [
          ((0,0), rotate c3), ((0,1), rotate . rotate $ c4), ((0,2), rotate . rotate $ c1),
          ((1,0), rotate . rotate $ c6), ((1,1), c2), ((1,2), rotate c7),
          ((2,0), rotate . rotate $ c5), ((2,1), rotate . rotate $ c8), ((2,2), c0)
        ])
