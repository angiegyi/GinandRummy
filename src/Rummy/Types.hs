{-# LANGUAGE NamedFieldPuns #-}

-- | Datatypes for playing Gin Rummy
module Rummy.Types where

import Cards
import Control.DeepSeq(NFData, rnf, rwhnf)

type PlayerId = String
-- | Is a bit more complex than expected as we need the info for making logs
data Play = Play
  { playId :: PlayerId
  , top :: Card
  , picked :: Card
  , draw :: Draw
  , act :: Action
  , memory :: String
  , finalHand :: [Card]
  }
type Trick = [Play]

data Meld =
      Deadwood Card            -- An unmelded card
    | Set3 Card Card Card      -- 3 cards of same rank different suit
    | Set4 Card Card Card Card -- 4 cards of same rank different suit
    | Straight3 Card Card Card -- 3 cards of same suit, sequential ranks
    | Straight4 Card Card Card Card -- 4 cards of same suit, sequential ranks
    | Straight5 Card Card Card Card Card -- 5 cards of same suit, sequential ranks

instance NFData Meld where
  rnf = rwhnf

data Draw = Stock | Discard
data Action = Action Act Card
data Act = Gin | Knock | Drop
  deriving(Show)

instance NFData Action where
  rnf = rwhnf

instance NFData Draw where
  rnf = rwhnf

-- Error management
data GameError = GameError PlayerError PlayerId

instance Show GameError where
    show (GameError err pid) = "Error: '" ++ pid ++ "' " ++ show err

data PlayerError = ReplayError
                 | InvalidCardError
                 | TimeError
                 | SetError
                 | StraightError
                 | KnockError
                 | GinError
                 | OmitError

-- | Default text for errors
instance Show PlayerError where
  show ReplayError = "Cannot play the last drawn card"
  show InvalidCardError = "Card played wasn't in player's hand"
  show TimeError = "Took too long to play"
  show SetError = "All cards in a set must be of the same rank"
  show StraightError = "All cards in a straight must be of the same suit"
  show KnockError = "Knocked with more than 10 points in deadwood"
  show GinError = "Called gin with remaining deadwood"
  show OmitError = "Did not combine all cards into melds"

data Player = Player {
  playerId :: PlayerId,
  playFunc :: PlayFunc,
  actionFunc :: ActionFunc,
  meldFunc :: MeldFunc
}

instance Eq Player where
  Player{playerId=a} == Player{playerId=b} = a == b

instance Show Player where
  show Player{playerId} = "Player: " ++ show playerId

data Hand = Hand {
  owner :: Player,
  cards :: [Card]
}

data GameResult = GameResult {
  hands :: [HandResult],
  gameScore :: [GameScore],
  updatedPlayers :: [Player]
}

data HandResult = HandResult {
  tricks :: Trick,      -- ^ One play at a time
  scores :: [HandScore] -- ^ One score per player
}

instance Show HandResult where
  show (HandResult _ scores) = show scores

data HandScore = HandScore {
  scoreId :: PlayerId,
  score :: Int
} deriving Show

data GameScore = GameScore {
  player :: Player,
  finalScore :: Int
}

instance Show GameScore where
  show (GameScore Player{playerId} score) = "Player: " ++ show playerId ++
    ", final score: " ++ show score

-- | Play function type.
--
-- A player receives the card he decided to draw (from discard or stock), her
-- hand and her memory. She then choses whether to Knock or Discard.
type PlayFunc
  = Card              -- ^ last picked card
  -> String           -- ^ the player's memory
  -> [Card]           -- ^ the player's hand
  -> (Action, String) -- ^ the player's chosen card and new state

-- | Action function type.
--
-- This function is called at the beginning of a turn before the player has to
-- form melds.
type ActionFunc
  = Card          -- ^ card on top of the discard pile
  -> Maybe String -- ^ player's memory, on first player turn it will be Nothing
  -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
  -> [Card]       -- ^ the player's hand
  -> (Draw, String) -- ^ which pile did the player chose to draw from

-- | Meld function type.
--
-- Which melds to use for scoring.
type MeldFunc
  = String  -- ^ the player's memory
  -> [Card] -- ^ cards in player's hand
  -> [Meld] -- ^ elected melds