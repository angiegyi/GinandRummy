-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
import Data.List


-- You can add more imports if you need them

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc 
  -- -> Card          -- ^ card on top of the discard pile
  -- -> Maybe String -- ^ player's memory, on first player turn it will be Nothing
  -- -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
  -- -> [Card]       -- ^ the player's hand
  -- -> (Draw, String) -- ^ which pile did the player chose to draw from
pickCard cardTop memory draw card = undefined

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard lastcard memory hand = undefined

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: makeMelds
makeMelds = undefined

-- My Functions ---------------------------------------------

--- Straights ----

-- | Function makes straights based on your current list of cards. 
-- Input: takes in a list of cards: [Card] type
-- Return: a boolean if a straight can be made 
makeStraights :: [Card] -> Bool
makeStraights = undefined
-- for each type of suit, run checkStraights

-- | Function checks straights for a particular suits 
-- By getting all cards of a particular suit and checking if they're in order. 
-- Input: takes in a list of cards: [Card] type
-- Input: takes in a suit: Suit type
-- Return: a list of cards of the specified Suit 
checkStraights :: [Card] -> Suit -> [Card] 
checkStraights cards suit = sortCards (getSuit (cards) (suit))
-- for each pair of card, if check consequtive is true, return an array with the cards 
-- given a suit, check if there are more than 4 consecutive cards and if so, return the cards that make that set

-- | Function checks the difference between the values of two cards 
-- Input: takes in a list of cards: [Card] type
-- Return: a boolean if the two cards are consecutive cards 
checkConsequtive :: Card -> Card -> Bool 
checkConsequtive (Card _ rank1) (Card _ rank2) 
    | (rank1 == Jack && rank2 == Queen) || (rank2 == Jack && rank1 == Queen) = True 
    | (rank1 == Queen && rank2 == King) || (rank2 == Queen && rank1 == King) = True 
    | abs(fromEnum rank1 - fromEnum rank2) == 1 = True
    | abs(fromEnum rank1 - fromEnum rank2) /= 1 = False 

-- | Function that filters for a list of cards with the suit you want
-- Input: takes in a list of cards and the suit you want: [Card] type, Suit type
-- Return: returns a list of cards that are only of the suit you want: [Card] type
getSuit :: [Card] -> Suit -> [Card]
getSuit cards suitWanted = filter (\x -> cardToSuit x == suitWanted) cards
    
--- Creating Sets ----

-- | Function makes straights based on your current list of cards. 
-- Input: takes in a list of cards: [Card] type
-- Return: a boolean if a set can be made 
makeSets :: [Card] -> Bool
makeSets cards 
    | length (getClubs $ cards) > 3  = True
    | length (getHearts $ cards) > 3 = True 
    | length (getClubs $ cards) > 3 = True 
    | length (getDiamonds $ cards) > 3 = True 

-- | Function makes the highest set based on your current list of cards. 
-- By looking for cards with the same number 
-- Input: takes in a list of cards: [Card] type
-- Return: a list of cards of the specified Suit 
checkSets :: [Card] -> [Card]
checkSets cards = findHighestSet allRanks
  where 
    ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    sameRanks = checkSameRank cards <$> ranks
    allRanks = filter (\x -> x /= [] && length(x) > 2) (sameRanks)

-- find cards of the same value then check if they are of different suits 
-- | Function that filters for a list of cards with the rank you want
-- Input: takes in a list of cards and the rank you want: [Card] type, Rank type
-- Return: returns a list of cards that are only of the rank you want: [Card] type
checkSameRank :: [Card] -> Rank -> [Card]
checkSameRank cards rankWanted = filter (\x -> cardToRank x == rankWanted) cards

-- | Function that finds the set with the highest score 
--findHighestSet :: [[Card]] -> [Int]
--findHighestSet options = calculateSetScore <$> options 

findHighestSet :: [[Card]] -> [Card]
findHighestSet cards = foldr1 (\x y ->if x >= y then x else y) cards

-- | Function that calculates the score of a set 
calculateSetScore :: [Card] -> Int 
calculateSetScore [] = 0
calculateSetScore (x:xs) = (fromEnum (cardToRank x)+1) + calculateSetScore xs

-- Conversion Functions ------------------------------

-- | Function gets the suit of the card via pattern matching. Pattern matching allows us to get the suit of the card.
-- Input: takes in the card: Card type
-- Return: returns the suit of the card: Suit type
cardToSuit :: Card -> Suit
cardToSuit (Card cardSuit _) = cardSuit

-- | Function gets the rank of the card via pattern matching. Pattern matching allows us to get the rank of the card.
-- Input: takes in the card: Card type
-- Return: returns the rank of the card: Rank type
cardToRank :: Card -> Rank
cardToRank (Card _ cardRank) = cardRank

-- Getting Suits ------------------------------

-- | Function that filters for a list of cards that are all Hearts suit (point-free)
-- Input: takes in a list of cards: [Card] type
-- Return: returns a list of cards that are of the Hearts Suit: [Card] type
getHearts :: [Card] -> [Card]
getHearts =  Data.List.filter ((== Heart) . cardToSuit)

-- | Function that filters for a list of cards that are all Clubs suit (point-free)
-- Input: takes in a list of cards: [Card] type
-- Return: returns a list of cards that are of the Clubs Suit: [Card] type
getClubs :: [Card] -> [Card]
getClubs =  Data.List.filter ((== Club) . cardToSuit)

-- | Function that filters for a list of cards that are all Spades suit (point-free)
-- Input: takes in a list of cards: [Card] type
-- Return: returns a list of cards that are of the Clubs Suit: [Card] type
getSpades :: [Card] -> [Card]
getSpades = Data.List.filter ((== Spade) . cardToSuit)

-- | Function that filters for a list of cards that are all Diamonds suit (point-free)
-- Input: takes in a list of cards: [Card] type
-- Return: returns a list of cards that are of the Diamond Suit: [Card] type
getDiamonds :: [Card] -> [Card]
getDiamonds =  filter ((== Diamond) . cardToSuit)

-- Sorting cards ------------------------------

-- | Function that sorts the cards based on the rank using the compare function (point-free style)
-- Input: a list of cards that you want to sort: [Card] type
-- Return: returns a list of cards that are sorted by rank: [Card] type
sortCardsOnRank :: [Card] -> [Card]
sortCardsOnRank = sortBy (\(Card _ a) (Card _ b) -> compare a b)

-- | Function that sorts the cards based on the suit and rank. It does this by filtering for each suit (creating a new list) then concatenating them
-- in the order of clubs, diamonds, spades and hearts.
-- Input: a list of cards that you want to sort: [Card] type
-- Return: returns a list of cards that are sorted by suit and the rank: [Card] type
sortCards :: [Card] -> [Card]
sortCards cards =  getClubs (sortCardsOnRank cards) ++ getDiamonds (sortCardsOnRank cards) ++ getSpades (sortCardsOnRank cards) ++ getHearts (sortCardsOnRank cards)

--- Choosing Card to Discard ------------------------------

-- | Function that finds the maximum valued card from a sorted list of cards
-- Input: a list of cards: [Card] type
-- Return: returns the maximum value card [Card] type
findMaxCard :: [Card] -> Card
findMaxCard cards = getLastElement (sortCardsOnRank(cards))

-- | Function that finds the maximum valued card from a sorted list of cards
-- Input: a list of cards: [Card] type
-- Return: returns the maximum value card [Card] type
getLastElement:: [Card] -> Card
getLastElement [x] = x 
getLastElement (_:xs) = getLastElement xs 

-- | Function that finds the maximum valued card from a sorted list of cards
-- Input: a list of cards: [Card] type
-- Return: returns the maximum value card [Card] type
chooseCardDiscard:: [Card] -> Card 
chooseCardDiscard discardCards = findMaxCard (discardCards)

