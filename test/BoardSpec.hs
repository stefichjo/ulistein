{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Board ( emptyBoard, Board )
import Cards (Card(..))
import Data.Maybe (isNothing, isJust)
import Data.Array ((!), array, elems)

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

    it "sollte den nächsten Setter aus einem Board ableiten" $ do
      let board = array ((0,0), (2,2)) [
              ((0,0), Nothing), ((0,1), Nothing), ((0,2), Nothing),
              ((1,0), Nothing), ((1,1), Just (Card 's' 'p' 'm' 'k')), ((1,2), Nothing),
              ((2,0), Nothing), ((2,1), Nothing), ((2,2), Nothing)
            ]
      let cardsCount = length [() | Just _ <- elems board]
      let position = positions !! cardsCount
      let nextSetter = makeSetter position -- TODO: makeSetter :: Board -> Setter

      let nextCard = Card 'S' 'P' 'M' 'K'
      let nextBoard = array ((0,0), (2,2)) [
              ((0,0), Nothing), ((0,1), Nothing), ((0,2), Nothing),
              ((1,0), Just nextCard), ((1,1), Just (Card 's' 'p' 'm' 'k')), ((1,2), Nothing),
              ((2,0), Nothing), ((2,1), Nothing), ((2,2), Nothing)
            ]

      nextSetter nextCard board `shouldBe` nextBoard

    it "sollte eine Position in einen Setter umwandeln" $ do
      let card = Card 's' 'p' 'm' 'k'
      let board = array ((0,0), (2,2)) [
              ((0,0), Nothing), ((0,1), Nothing), ((0,2), Nothing),
              ((1,0), Just card), ((1,1), Nothing), ((1,2), Nothing),
              ((2,0), Nothing), ((2,1), Nothing), ((2,2), Nothing)
            ]
      let setter c = \b -> array ((0,0), (2,2)) [
              ((0,0), b ! (0,0)), ((0,1), b ! (0,1)), ((0,2), b ! (0,2)),
              ((1,0), Just c), ((1,1), b ! (1,1)), ((1,2), b ! (1,2)),
              ((2,0), b ! (2,0)), ((2,1), b ! (2,1)), ((2,2), b ! (2,2))
            ]

      let position = (1,0)

      makeSetter position card emptyBoard `shouldBe` board

nextSetter' :: Board -> Setter
nextSetter' board =
  let
    cardsCount = length [() | Just _ <- elems board]
    position = positions !! cardsCount
  in
    undefined

on :: Card -> Board -> Board
card `on` board = array ((0,0), (2,2)) [
    ((0,0), board ! (0,0)), ((0,1), board ! (0,1)), ((0,2), board ! (0,2)),
    ((1,0), board ! (1,0)), ((1,1), Just card), ((1,2), board ! (1,2)),
    ((2,0), board ! (2,0)), ((2,1), board ! (2,1)), ((2,2), board ! (2,2))
  ]

type Setter = Card -> Board -> Board

makeSetter :: Position -> Setter
makeSetter (1, 0) c = \b -> array ((0,0), (2,2)) [
    ((0,0), b ! (0,0)), ((0,1), b ! (0,1)), ((0,2), b ! (0,2)),
    ((1,0), Just c), ((1,1), b ! (1,1)), ((1,2), b ! (1,2)),
    ((2,0), b ! (2,0)), ((2,1), b ! (2,1)), ((2,2), b ! (2,2))
  ]

type Position = (Int, Int)

positions :: [Position]
positions = [(1,1), (1,0), (2,1), (1,2), (0,1), (2,0), (2,2), (0,2), (0,0)]
