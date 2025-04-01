module CardSpec where

import Test.Hspec ( Spec, describe, it, shouldBe, shouldNotBe )
import Card ( rotate, rotations, Card(Card) )

card :: Card
card = Card 'S' 'k' 'M' 'p'

spec :: Spec
spec = do
  describe "Card" $ do
    it "sollte eine Karte als kompakte Zeichenkette anzeigen" $ do
      show card `shouldBe` "SkMp"

    it "sollte eine Karte korrekt rotieren" $ do
      rotate card `shouldBe` Card 'p' 'S' 'k' 'M'

    it "sollte die möglichen Rotationen einer Karte kennen" $ do
      rotations card `shouldBe` [
          Card 'S' 'k' 'M' 'p',
          Card 'p' 'S' 'k' 'M',
          Card 'M' 'p' 'S' 'k',
          Card 'k' 'M' 'p' 'S'
        ]

    it "sollte gelegte Karten rotationstolerant vergleichen" $ do
      (Nothing :: Maybe Card) `shouldBe` (Nothing :: Maybe Card)
      (Nothing :: Maybe Card) `shouldNotBe` Just card
      Just card `shouldNotBe` (Nothing :: Maybe Card)
      Just card `shouldBe` Just card
      Just card `shouldBe` Just (rotate card)
      Just card `shouldBe` Just (rotate . rotate $ card)
      Just card `shouldBe` Just (rotate . rotate . rotate $ card)


