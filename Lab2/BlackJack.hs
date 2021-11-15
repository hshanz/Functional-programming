module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

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
--Test var för card 
ranktest = Card Jack Spades

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

(<+) :: Hand -> Hand -> Hand
(<+) Empty h1 = h1
(<+) h1 Empty = h1
(<+) Add Card = 