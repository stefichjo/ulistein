module CardsSpec (spec) where

import Test.Hspec
import Cards
import Cards
import Cards (rotations)

spec :: Spec
spec = do
  describe "Card" $ do
    it "sollte eine Karte korrekt rotieren" $ do
      let
        card = Card 'S' 'k' 'M' 'p'
        rotated = rotate card
      rotated `shouldBe` Card 'p' 'S' 'k' 'M'

    it "sollte eine Karte als kompakte Zeichenkette anzeigen" $ do
      let card = Card 'S' 'k' 'M' 'p'
      show card `shouldBe` "SkMp"

    it "sollte eine Karte 4 mal rotieren können" $ do
      let
        card = Card 's' 'p' 'm' 'k'
      map show (rotations card) `shouldBe` [
          "spmk",
          "kspm",
          "mksp",
          "pmks"
        ]

