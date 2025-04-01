module CardSpec where

import Test.Hspec ( Spec, describe, it, shouldBe, shouldNotBe )
import Card ( rotate, rotations, Card(Card), Half(Half), Part(..), Animal(..) )

card :: Card
card = Card (Half Upper Schwein) (Half Lower Katze) (Half Upper Maus) (Half Lower Pinguin)

spec :: Spec
spec = do
  describe "Card" $ do
    it "sollte eine Karte als kompakte Zeichenkette anzeigen" $ do
      show card `shouldBe` "SkMp"

    it "sollte eine Karte korrekt rotieren" $ do
      rotate card `shouldBe` Card (Half Lower Pinguin) (Half Upper Schwein) (Half Lower Katze) (Half Upper Maus)

    it "sollte die möglichen Rotationen einer Karte kennen" $ do
      rotations card `shouldBe` [
          Card (Half Upper Schwein) (Half Lower Katze) (Half Upper Maus) (Half Lower Pinguin),
          Card (Half Lower Pinguin) (Half Upper Schwein) (Half Lower Katze) (Half Upper Maus),
          Card (Half Upper Maus) (Half Lower Pinguin) (Half Upper Schwein) (Half Lower Katze),
          Card (Half Lower Katze) (Half Upper Maus) (Half Lower Pinguin) (Half Upper Schwein)
        ]

    it "sollte gelegte (d.h. evtl. rotierte) Karten rotationstolerant vergleichen" $ do
      Just card `shouldNotBe` (Nothing :: Maybe Card)
      (Nothing :: Maybe Card) `shouldNotBe` Just card
      (Nothing :: Maybe Card) `shouldBe` (Nothing :: Maybe Card)
      Just card `shouldBe` Just card
      Just card `shouldBe` Just (rotate card)
      Just card `shouldBe` Just (rotate . rotate $ card)
      Just card `shouldBe` Just (rotate . rotate . rotate $ card)
