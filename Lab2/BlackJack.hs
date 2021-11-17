module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random


--A0
-----------------------------------------------------------
--Given definition of hand2
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)


--Breakdown of the size function in steps 
-- Should return list of 2s 
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            ,1 + size (Add (Card Jack Spades) Empty)
            ,2 + size (Empty)
            ,2]

-----------------------------------------------------------
--A1

--Function for displaying a hand of cards 
display :: Hand -> String
display Empty = ""
display (Add ( Card(Numeric n) suit) hand) = show(n) ++ " of " ++ show(suit) ++ "\n" ++ display(hand)
display (Add card hand) = show(rank card) ++ " of " ++ show(suit card) ++ "\n" ++ display(hand)

------------------------------------------------------------
--A2
--Function for getting the value of a card rank
--Helper for value
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank rank = 10

--Function for calculating the value of a hand of cards 
value :: Hand -> Integer
value hand
    | handValue <= 21 = handValue
    | handValue > 21 = handValue - 10*numberOfAces hand
    where handValue = initialValue hand

--Function for finding the amount of aces in a hand
--Helper function for value
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace suit ) hand) = 1 + numberOfAces hand
numberOfAces (Add card hand) = numberOfAces hand

--Calculates the total value of the hand without regards to aces being 11 or 1
--Helper function for value 
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand

-------------------------------------------------------------
--A3
--Checks if hand value is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-------------------------------------------------------------
--A4 
--hand2 is the Bank hand 

--hand1 is the guest hand
hand1 = Add (Card (Numeric 4) Hearts)
            (Add (Card Jack Spades) Empty)

-- Function for finding the winner in a game of blackjack 
--If they tie the bank will win because of gambling
winner :: Hand -> Hand -> Player
winner hand1 hand2
    | gameOver hand1 && gameOver hand2      = Bank
    | not(gameOver hand1) && gameOver hand2 = Guest
    | gameOver hand1 && not(gameOver hand2) = Bank
    | value hand1 > value hand2             = Guest
    | value hand1 < value hand2             = Bank
    | value hand1 == value hand2            = Bank

----------------------------------------------------------------
--Lab 2B

--B1
--Function for adding hands together
(<+) :: Hand -> Hand -> Hand
Empty <+ h1 = h1
h1 <+ Empty = h1
h1 <+ Add card h2 = Add card h3
    where h3 = h1 <+ h2
--prop for checking association for <+ function 
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

--prop for checking that size is maintained for <+ function 
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

----------------------------------------------------------------
--B2
--Creates a hand with all ranks of a suit 
createCards :: [Rank] -> Suit -> Hand
createCards [] s = Empty
createCards (x:xs) s = Add (Card x s) (createCards xs s)

--Creates a list of all ranks and calls createCard function with given 
--suit and list to create a hand 
createHand :: Suit -> Hand
createHand = createCards ([Numeric n | n <- [2..10]] ++ [Jack,Queen,King,Ace])

--Creates a full deck of cards 
fullDeck :: Hand
fullDeck = createHand Hearts <+ createHand Spades <+ createHand Diamonds <+ createHand Clubs

----------------------------------------------------------------
--B3
--Draws a card from a hand/deck of cards
--First arg is deck
draw :: Hand -> Hand ->(Hand, Hand)
draw Empty h1 = error "draw: The deck is empty."
draw (Add c deck) h = (deck, Add c h)

----------------------------------------------------------------
--B4
--Bank algorithm function foe drawing cards 
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

--Helper function for playBank, draws cards until value is > 16 
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand
    | value hand < 16 = playBankHelper smallerDeck biggerHand
    | otherwise = hand
  where (smallerDeck,biggerHand) = draw deck hand

----------------------------------------------------------------
--B5


--Function for shuffling deck 
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g Empty = Empty
shuffleDeck g d = newDeck
    where(deck,newDeck) = shuffleHelper g (d,Empty)

--Helper function to make the recursion work
shuffleHelper :: StdGen -> (Hand,Hand) -> (Hand,Hand)
shuffleHelper _ (Empty,nd) = (Empty,nd)
shuffleHelper g (deck,newDeck) = shuffleHelper g' (deck',newDeck')
    where (n,g') = randomR(0, size deck - 1) g
          newDeck' = newDeck <+ Add (findNthCard n deck) Empty
          deck' = removeNthCard n deck

--Gets the n:th card of the deck
findNthCard :: Integer -> Hand -> Card
findNthCard 0 (Add card hand) = card
findNthCard n (Add card hand) = findNthCard (n-1) hand

--Remves the n:th card from the deck
removeNthCard :: Integer -> Hand -> Hand
removeNthCard 0 (Add card hand) = hand
removeNthCard n (Add card hand) = Add card (removeNthCard (n-1) hand)

------------------------------------------------------
--PROPS

--Props for checking that size is maintained after shuffle 
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h) 

--Prop for checking that cards are maintained after shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h
--Checks that a card belongs to a hand/deck 
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
--------------------------------------------------------------------
--B6
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }
--Main function for running the game 
main :: IO ()
main = runGame implementation