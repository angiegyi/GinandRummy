-- | Rummy-specific rules
module Rummy.Rules where

import Data.List(sort)

import Deck
import Cards
import Rummy.Types
import EitherIO

-- | Card value is face value for number cards, 10 for face cards.
toPoints :: Card -> Int
toPoints (Card _ rank) | rank < Jack = fromEnum rank
                       | otherwise = 10 -- Face cards are all worth 10

-- | The scoring in Rummy is:
--
--  - card value for Deadwood;
--  - nil for Melds.
cardPoints :: Meld -> Int
cardPoints (Deadwood c) = toPoints c
cardPoints _ = 0

-- There is no bidding in Rummy.
dealAndBid :: Int -> [Player] -> [Card] -> EitherIO PlayerError [Hand]
dealAndBid n players deck = return $ zipWith Hand players hands
  where
    hands = deal n (length players) deck

-- | Verify that the chosen card is a valid play:
--
--  - Cannot play the card just drawn (replay).
--  - Has to play a card in hand (invalid).
validPlay
  :: Action   -- ^ Card discarded
  -> Card     -- ^ Last drawn card
  -> PlayerId -- ^ Player
  -> [Card]   -- ^ Hand
  -> Either GameError Card
validPlay (Action _ card) drawn playerId handCards
  | card == drawn = Left $ GameError ReplayError playerId
  | card `notElem` handCards = Left $ GameError InvalidCardError playerId
  | otherwise = Right card

-- | Check that melds formed are valid.
validMeld :: PlayerId -> Meld -> Either GameError Meld
validMeld playerId meld = case meld of
  Deadwood _ -> Right meld
  Set3 a b c -> toEither (sameRank a [b, c]) meld setError
  Set4 a b c d -> toEither (sameRank a [b, c, d]) meld setError
  Straight3 a b c -> toEither (sameSuit a [b, c]) meld straightError
  Straight4 a b c d -> toEither (sameSuit a [b, c, d]) meld straightError
  Straight5 a b c d e -> toEither (sameSuit a [b, c, d, e]) meld straightError
  where
    setError = GameError SetError playerId
    straightError = GameError StraightError playerId
    rank (Card _ r) = r
    suit (Card s _) = s
    sameSuit = same suit
    sameRank = same rank
    same f c xs = all (== f c) (map f xs)

-- | Check that the full hand of melds is valid.
validHand :: Play -> [(PlayerId, Meld)] -> Either GameError [(PlayerId, Meld)]
validHand Play{playId, finalHand, act} melds =
  toEither (allCards laid) melds (GameError OmitError playId) >>
  toEither check melds (GameError err playId)
  where
    laid = map snd $ filter ((playId ==) . fst) melds
    deadwood = [x | Deadwood x <- laid]
    (Action action _) = act
    (check, err) = case action of
      Gin -> (length deadwood > 0, GinError)
      Knock -> ((sum . (map toPoints)) deadwood > 10, KnockError)
      Drop -> (True, undefined) -- No deadwood error when not the caller
    -- Works because there is a complete order on Cards
    allCards melded = sort finalHand == ((sort . concatMap combine) melded)
    combine (Deadwood c) = [c]
    combine (Set3 a b c) = [a, b, c]
    combine (Set4 a b c d) = [a, b, c, d]
    combine (Straight3 a b c) = [a, b, c]
    combine (Straight4 a b c d) = [a, b, c, d]
    combine (Straight5 a b c d e) = [a, b, c, d, e]

toEither :: Bool -> a -> b -> Either b a
toEither b v err
  | b = Right v
  | otherwise = Left err
