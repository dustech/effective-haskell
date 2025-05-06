import           Data.List
import           System.IO

-- simple algebraic data types for card values and suites
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Enum)

data CardSuite = Clubs | Spades | Diamonds | Hearts deriving (Eq, Enum)

-- our card type - merely combining CardValue and CardSuite
data Card = Card CardValue CardSuite deriving(Eq)

-- synonym for list of cards to store decks
type Deck = [Card]

instance Show CardValue where
  show c = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"] !! fromEnum c

-- please make sure to enable utf-8 in your terminal, otherwise you may get errors when trying to execute
instance Show CardSuite where
  show Spades   = "♠"
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"

-- defining show function that is a little nicer then default
instance Show Card where
  show (Card a b) = show a ++ show b

-- defining full deck of cards via comprehension;
fullDeck :: Deck
fullDeck = [ Card x y | y <- [Clubs .. Hearts], x <- [Two .. Ace] ]
smallDeck = [Card Ace Spades, Card Two Clubs, Card Jack Hearts]
main = print smallDeck
  >> putStrLn "Press Enter to deal the full deck"
  >> getLine
  >> mapM_ print fullDeck
