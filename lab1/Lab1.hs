{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k
   | k < 0 = error "power: negative argument"
stepsPower n k = k + 1 


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "power: negative argument"
power1 n k = product(replicate (fromInteger k) n)

-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k 
   | even k = power2 (n * n) ( k `div` 2)
   | otherwise = n * power2 n (k-1)

-- D -------------------------
{- 

<Describe your test cases here>
   k < 0 (Might be hard to write test case for as this throws an error)
   k = 0 
   n = 0 
   power n k == power1 n k == power2 n k
 -}

--
prop_powers :: Integer -> Integer -> Bool 
prop_powers n k = 
   power n k == power1 n k
   && power1 n k == power2 n k

--
powerTest :: Bool
powerTest =
   prop_powers 0 0 
   && prop_powers 2 0
   && prop_powers 2 2
   && prop_powers 5 7

--
prop_powers' :: Integer -> Integer -> Bool 
prop_powers' n k = 
   power n k' == power1 n k'
   && power1 n k' == power2 n k'
   where k' = abs k