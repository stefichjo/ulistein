module CardsSpec (spec) where

import Test.Hspec
import Cards

spec :: Spec
spec = do
  describe "Card" $ do
    it "sollte eine Karte korrekt erstellen" $ do
      let card = Card 'S' 'k' 'M' 'p'
      top card `shouldBe` 'S'
      right card `shouldBe` 'k'
      bottom card `shouldBe` 'M'
      left card `shouldBe` 'p'

    it "sollte eine Karte korrekt rotieren" $ do
      let card = Card 'S' 'k' 'M' 'p'
          rotated = rotateCard card 1
      top rotated `shouldBe` 'p'
      right rotated `shouldBe` 'S'
      bottom rotated `shouldBe` 'k'
      left rotated `shouldBe` 'M'

    it "sollte nach vier Rotationen wieder im Ursprungszustand sein" $ do
      let card = Card 'S' 'k' 'M' 'p'
          rotated = rotateCard (rotateCard (rotateCard (rotateCard card 1) 1) 1) 1
      rotated `shouldBe` card

    it "sollte eine Karte als kompakte Zeichenkette anzeigen" $ do
      let card = Card 'S' 'k' 'M' 'p'
      show card `shouldBe` "SkMp"

    it "sollte eine Karte 4 mal rotieren können" $ do
      let
        card = Card 's' 'p' 'm' 'k'
        rotations = rotationsCard card
      rotations `shouldBe` [
          Card 's' 'p' 'm' 'k',
          Card 'k' 's' 'p' 'm',
          Card 'm' 'k' 's' 'p',
          Card 'p' 'm' 'k' 's'
        ]

