import Data.List

largestPrime :: Int -> Int
findPrimes :: Int -> Int -> [Int] -> [Int]
isPrime :: Int -> Int -> Bool
findFactors :: Int -> Int -> Int

-- This method is an implementation of the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
-- It will find all of the prime numbers in a sequence from 2 - x. 
-- Cur Val is the current value we are looking at as we move from the Sieve of Eratosthenes
-- Multiplier is how we are getting multiples of that number to remove them
-- total Array is the array of values that contain the primes. As we find multiples, we remove numbers from 
-- this array. Unfortunately, this method is too slow to solve this solution, but is still a cool implementation, so I'll leave it here for 
-- educational purposes
findPrimes curVal multiplier totalArray =
	if curVal == (last totalArray) then
		totalArray
	else if ((*) curVal  multiplier) <= (last totalArray) then
		findPrimes curVal (multiplier+1) (delete (curVal * multiplier) totalArray)
	else
		findPrimes (curVal + 1) 2 totalArray


-- Function that checks if a number is prime
isPrime largest divisor =
	if (mod largest divisor) == 0 then
		False
	else if divisor < 7 then
		isPrime largest (divisor+1)
	else 
		True


findFactors val factor  =
	if (mod val factor) == 0 then
		if isPrime (div val factor) 2 then
			(div val factor)
		else
			findFactors val (factor+1)
	else
		findFactors val (factor+1)

-- The largestPrime function is how we are actually driving the problem. The argument here is the upper bound value
-- that we want to find. For Euler Problem #3, this is 600851475143.
largestPrime x =
	findFactors x 2