{-# LANGUAGE BlockArguments #-}
-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where
import Parser.Parser -- This is the source for the parser from the course notes
import Parser.Instances 
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
import Data.List ( filter, (\\), sortBy, map )
import Data.Char (isDigit)

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
-- Input: Card on top of the pile (Card)
-- Input: The score (Int, Int)
-- Input: Memory (String)
-- Input: hand of cards [Card]
-- Output: a tuple of the pile chosen, the memory (Draw, String)

pickCard :: ActionFunc 
-- pattern match to check if it is the first turn 
pickCard card _ _ Nothing cards 
   | calculateSetScore(deckWithoutDiscard ++ [card]) > calculateSetScore cards = (Discard, show(Turn 1 (0,0)))
   | otherwise = (Stock, show(Turn 1 (0,0)))
  where
    deckWithoutDiscard = deckWithoutCard highestCard cards
    highestCard = chooseCardDiscard deadWoodCards
    cardsNotinStraights = (Data.List.filter (\x -> not (inList x (checkStraights cards))) cards)  
    deadWoodCards = Data.List.filter (\x -> not ((inList x (checkSets cardsNotinStraights)) || 
                    (inList x (checkStraights cards)))) cards

-- pattern match again to check if it is the first turn 
pickCard card _ Nothing _ cards 
   | calculateSetScore(deckWithoutDiscard ++ [card]) > calculateSetScore cards = (Discard, show(Turn 1 (0,0)))
   | otherwise = (Stock, show(Turn 1 (0,0)))
  where
    deckWithoutDiscard = deckWithoutCard highestCard cards
    highestCard = chooseCardDiscard deadWoodCards
    cardsNotinStraights = (Data.List.filter (\x -> not (inList x (checkStraights cards))) cards)  
    deadWoodCards = Data.List.filter (\x -> not ((inList x (checkSets cardsNotinStraights)) || (inList x (checkStraights cards)))) cards  

-- pattern match for every other turn
pickCard card score memory _ cards 
   | calculateSetScore(deckWithoutDiscard ++ [card]) > calculateSetScore cards = (Discard, show(res))
   | otherwise = (Stock, show(res))
  where
    deckWithoutDiscard = deckWithoutCard highestCard cards
    highestCard = chooseCardDiscard deadWoodCards
    cardsNotinStraights = (Data.List.filter (\x -> not (inList x (checkStraights cards))) cards)  
    deadWoodCards = Data.List.filter (\x -> not ((inList x (checkSets cardsNotinStraights)) || (inList x (checkStraights cards)))) cards 
    mem = parse parseTurns (toString memory)
    res = if score == getParseResultScore mem 
          then incrementTurns (resultToTurn mem) 
          else resetTurns (resultToTurn mem)

-- | This function is called once you have drawn a card, you need to decide 
-- which action to call.
-- Input: the card which was picked up from either the discard or the stock (Card)
-- Input: the score (Int, Int)
-- Input: the memory (String)
-- Input: your current hand of cards (Cards)
-- Output: Action chosen, card to discard and the memory (Action, Card, String)
playCard :: PlayFunc
playCard pickUpCard score memory cards 
  | (turns /= 1 && length(deadwoodCards) == 0 && snd score > fst score) = (Action Gin discardCard, "") 
  | (turns /= 1 && length(deadwoodCards) == 0) = (Action Gin discardCard, "") 
  | (turns /= 1 && calculateSetScore (deadwoodCards) < 10) = (Action Knock discardCard, "") 
  | discardCard == pickUpCard = (Action Drop discardCard, "")
  | otherwise = (Action Drop (discardCard), "")
  where 
    cardsIncPickup = (deckWithoutCard discardCard cards) ++ [pickUpCard]
    discardCard = chooseCardDiscard cards
    deadwoodCards = meldToCardList (getDeadWood melds) -- this is a meld 
    melds = makeMelds score memory cardsIncPickup 
    mem = parse parseTurns memory
    turns = getParseResultTurns (mem)

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
-- Input: the score (Int, Int)
-- Input: the memory (String)
-- Input: your current hand of cards (Cards)
-- Output: a list of melds [Meld]
makeMelds :: MeldFunc
makeMelds _ _ cards 
   | length(straightCards) >= 3 = makeStraights cards ++ makeSets cardsNotinStraights ++ makeDeadWood deadWoodCards
   | length(straightCards) < 3 && length(setCards) == 0 = makeDeadWood cards
   | otherwise = makeSets cards ++ makeDeadWood cardsNotinSets
  where 
    straightCards = meldToCardList (makeStraights cards) 
    setCards = checkSets cards -- is a list of cards that make up a set  
    cardsNotinStraights = (Data.List.filter (\x -> not (inList x straightCards)) cards) -- these are the cards left after a straight is made 
    deadWoodCards = Data.List.filter (\x -> not ((inList x (checkSets cardsNotinStraights)) || (inList x straightCards))) cards -- checks for all cards not in any straight or set 
    cardsNotinSets = (Data.List.filter (\x -> not (inList x setCards)) cards)

-- Function converts a meld to individual cards 
-- Input: a meld: Meld type 
-- Output: a list of cards that make up the melds: [Card] type
meldToCard:: Meld -> [Card]
meldToCard (Straight5 a b c d e) = [a] ++ [b] ++ [c] ++ [d] ++ [e]
meldToCard (Straight4 a b c d) = [a] ++ [b] ++ [c] ++ [d] 
meldToCard (Straight3 a b c) = [a] ++ [b] ++ [c]
meldToCard (Deadwood c) = [c]
meldToCard (Set3 c1 c2 c3) = [c1] ++ [c2] ++ [c3]     -- 3 cards of same rank different suit
meldToCard (Set4 c1 c2 c3 c4) = [c1] ++ [c2] ++ [c3] ++ [c4]

-- My Functions ---------------------------------------------

-- | Function converts each item in the list to deadwood 
-- Input: takes in a list of cards: [Card] type
-- Return: list of deadwood melds: [Meld] type
makeDeadWood :: [Card] -> [Meld]
makeDeadWood [] = []
makeDeadWood cards = Data.List.map (\x -> Deadwood x) cards

-- | Checks if a card is in a list 
-- Input: takes in a card and a list of cards: [Card] type
-- Return: True if the card is in the list otherwise, False: Boolean type
inList :: Card -> [Card] -> Bool
inList _ [] = False
inList card (x:xs) 
    | card == x = True
    | otherwise = inList card xs

-- | This function filters the cards for all cards minus a given card
-- Input: takes in a card and a list of cards: Card and [Card] type 
-- Return: returns all cards which is the card not to include: Card type 
deckWithoutCard:: Card -> [Card] -> [Card]
deckWithoutCard cardNotInclude cards = Data.List.filter (\x -> (x /= cardNotInclude)) cards 

-- Converts a maybe string to a string 
-- Input: takes in a string: String type
-- Return: a list of melds excluding deadwood cards: [Card] 
toString:: Maybe String -> String 
toString (Just x) = x 
toString (Nothing) = ""

-- this function filters a meld for non deadwood cards 
-- Input: takes in a list of melds: [Meld] type
-- Return: a list of melds excluding deadwood cards: [Card] 
checkDeadWood :: [Meld] -> [Meld]
checkDeadWood cards = Data.List.filter (\x -> not (isDeadWood x)) cards 

-- this function filters for the deadwood cards  
-- Input: takes in a list of cards: [Card] type
-- Return: a boolean if the card is of type deadwood: Bool type
getDeadWood :: [Meld] -> [Meld]
getDeadWood cards = Data.List.filter (\x -> (isDeadWood x)) cards 

-- Function checks if a card is of type deadwood 
-- Input: takes in a list of cards: [Card] type
-- Return: a boolean if the card is of type deadwood: Bool type
isDeadWood :: Meld -> Bool
isDeadWood (Deadwood _) = True 
isDeadWood _ = False 

-- Function converts a list of melds to a list of cards 
-- Input: a list of melds: [Meld] type
-- Return: a list of cards that make up the melds: [Card] type 
meldToCardList:: [Meld] -> [Card]
meldToCardList cards = foldList (Data.List.map(\x -> meldToCard x) cards) 

-- Function flatterns a list
-- Input: a double nested list to be flattened: [] 
-- Return: a single that has been flattened: [Card] type 
foldList:: [[Card]] -> [Card]
foldList l = foldr (++) [] l

--- Straights ------------------------------------------------

-- | Function makes straights based on your current list of cards. 
-- Input: takes in a list of cards: [Card] type
-- Return: a meld of straights: [Meld] type
makeStraights :: [Card] -> [Meld]
makeStraights c 
    | length(cards) >  5 = [Straight5 (cards !! (length(cards)-5)) (cards !! (length(cards)-4)) (cards !! (length(cards)-3)) (cards !! (length(cards)-2)) (cards !! (length(cards)-1))]
    | length(cards) == 5 = [Straight5 (cards !! 0) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)]
    | length(cards) == 4 = [Straight4 (cards !! 0) (cards !! 1) (cards !! 2) (cards !! 3)] 
    | length(cards) == 3 = [Straight3 (cards !! 0) (cards !! 1) (cards !! 2)]
    | otherwise = []
  where 
      cards = (checkStraights $ c)

-- | Function makes a list of cards that make up a straights based on your current list of cards. 
-- Input: takes in a list of cards: [Card] type
-- Return: a list of cards that make up a straight: [Card] type
checkStraights :: [Card] -> [Card]
checkStraights cards = findHighestSet [heartSuits, spadeSuits, clubSuits, diamondSuits]
  where 
    heartSuits =  formStraights (sortCards (getSuit cards Heart)) [] []
    spadeSuits =  formStraights (sortCards (getSuit cards Spade)) [] []
    clubSuits =  formStraights  (sortCards (getSuit cards Club)) [] []
    diamondSuits =  formStraights (sortCards (getSuit cards Club)) [] []

-- | Function checks the difference between the values of two cards 
-- Input: takes in a list of cards: [Card] type
-- Return: a boolean if the two cards are consecutive cards 
checkConsequtive :: Card -> Card -> Bool 
checkConsequtive (Card _ rank1) (Card _ rank2) 
    | (rank1 == Jack && rank2 == Queen) || (rank2 == Jack && rank1 == Queen) = True 
    | (rank1 == Queen && rank2 == King) || (rank2 == Queen && rank1 == King) = True 
    | abs(fromEnum rank1 - fromEnum rank2) == 1 = True
    | otherwise = False 

-- | Function that returns the largest straight that can be made with a hand of cards 
-- Input: takes in a list of cards: [Card] type
-- Return: A array of cards that make the highest straight
formStraights:: [Card] -> [Card] -> [Card] -> [Card]
formStraights [] _ _ = [] 
formStraights [_] [] [] = []
formStraights [x] acc maxrun = if (length(acc) == 0 || length(maxrun) == 0) then [] else if (checkConsequtive x (last(maxrun))) then maxrun ++ [x] else maxrun
formStraights(x:xs) [] [] = if (checkConsequtive x (head(xs))) then formStraights xs [x] [x] else formStraights xs [x] []
formStraights (x:xs) acc maxrun = if (checkConsequtive x (last(acc))) && (length(acc ++ [x]) > length(maxrun)) 
                                    then formStraights xs (acc ++ [x]) (acc ++ [x]) 
                                    else formStraights xs [x] maxrun

-- | Function that filters for a list of cards with the suit you want
-- Input: takes in a list of cards and the suit you want: [Card] type, Suit type
-- Return: returns a list of cards that are only of the suit you want: [Card] type
getSuit :: [Card] -> Suit -> [Card]
getSuit cards suitWanted = Data.List.filter (\x -> cardToSuit x == suitWanted) cards
    
--- Creating Sets ------------------------------------------------

-- | Function makes sets based on your current list of cards. 
-- Input: takes in a list of cards: [Card] type
-- Return: Meld of the set
makeSets :: [Card] -> [Meld]
makeSets cards 
    | length (checkSets card_set) == 3 = [Set3 (card_set !! 0) (card_set !! 1) (card_set !! 2)]
    | length (checkSets card_set) == 4 = [Set4 (card_set !! 0) (card_set !! 1) (card_set !! 2) (card_set !! 3)]
    | otherwise = []
    where 
      card_set = checkSets $ cards

-- | Function makes the highest set based on your current list of cards. 
-- By looking for cards with the same number 
-- Input: takes in a list of cards: [Card] type
-- Return: a list of cards of the specified Suit 
checkSets :: [Card] -> [Card]
checkSets cards 
    | allRanks == [] = [] 
    | otherwise = findHighestSet allRanks
  where 
    ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    sameRanks = checkSameRank cards <$> ranks
    allRanks = Data.List.filter (\x -> x /= [] && length(x) > 2) (sameRanks)

-- find cards of the same value then check if they are of different suits 

-- | Function that filters for a list of cards with the rank you want
-- Input: takes in a list of cards and the rank you want: [Card] type, Rank type
-- Return: returns a list of cards that are only of the rank you want: [Card] type
checkSameRank :: [Card] -> Rank -> [Card]
checkSameRank cards rankWanted = Data.List.filter (\x -> cardToRank x == rankWanted) cards

-- | Function that finds the set with the highest score 
-- Input: takes in a list of cards: [[Card]] type
-- Return: returns the set with the highest score: [Card]
findHighestSet :: [[Card]] -> [Card]
findHighestSet cards = foldr1 (\x y ->if x >= y then x else y) cards

-- | Function that calculates the score of a hand 
-- Input: takes in a list of cards: [Card] type
-- Return: returns the score of the hand: Int type 
calculateSetScore :: [Card] -> Int 
calculateSetScore [] = 0
calculateSetScore [x] = fromEnum (cardToRank x) + 1 
calculateSetScore (x:xs) = fromEnum (cardToRank x) + 1 + calculateSetScore xs

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
getDiamonds =  Data.List.filter ((== Diamond) . cardToSuit)

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
findMaxCard cards = last (sortCardsOnRank(cards))

-- | Function that finds the maximum valued card from a sorted list of cards
-- Input: a list of cards: [Card] type
-- Return: returns the maximum value card [Card] type
chooseCardDiscard:: [Card] -> Card 
chooseCardDiscard discardCards = findMaxCard (discardCards)

--- Parser -------------------------
-- the memory will store the number of turns 

data Turns = Turn { turns :: Int , lastScore :: (Int, Int) } 
-- turn, score 

--show instance 
instance Show Turns where 
  show (Turn turns score) = show(turns) ++ "," ++ show (fst(score)) ++ "," ++ show (snd(score))

-- these functions are from the week 11 lab
list1 :: Parser a -> Parser [a]
list1 p = do {
    x <- p;
    xs <- list (p);
    return (x:xs)
}

list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

digit :: Parser Char
digit = P (parse (satisfy (isDigit)))

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P (\x -> if empty x then Error UnexpectedEof else if (f (head x)) then parse character x else Error (UnexpectedChar (head x)))
    where
        empty str = 0 == (length str)

-- This function is a parser for the Turns data structure 
parseTurns :: Parser Turns
parseTurns = do 
  turns <- list digit
  _ <- is (',')
  score1 <- list digit 
  _ <- is (',')
  score2 <- list digit
  return (Turn (read turns :: Int) (read score1 :: Int, read score2:: Int))

-- This function increments the turn of the game 
incrementTurns :: Turns -> Turns 
incrementTurns (Turn turns score) = Turn (turns + 1) score

resetTurns :: Turns -> Turns 
resetTurns (Turn _ score) = Turn 1 score

-- This function pattern to get the score 
getParseResultScore :: ParseResult Turns -> (Int, Int) 
getParseResultScore (Result _ (Turn _ lastScore)) = lastScore  
getParseResultScore _ = (0,0)

-- This function pattern to get the turn data type 
getParseResultTurns:: ParseResult Turns -> Int 
getParseResultTurns (Result _ (Turn turns _))  = turns  
getParseResultTurns _ = 1

-- This function pattern to get the score 1 
getParseResultScore1 :: ParseResult Turns -> Int
getParseResultScore1 (Result _ (Turn _ (score1, _))) = score1  
getParseResultScore1 _ = 1

-- This function pattern to get the score 2 
getParseResultScore2 :: ParseResult Turns -> Int
getParseResultScore2 (Result _ (Turn _ (_, score2))) = score2  
getParseResultScore2 _ = 1

-- This function pattern matches to get the turn object from the parse result 
resultToTurn :: ParseResult Turns -> Turns 
resultToTurn (Result _ turnObj ) = turnObj
resultToTurn _ = Turn 1 (0,0)