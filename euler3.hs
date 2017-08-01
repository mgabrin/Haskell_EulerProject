import Data.List
import Data.Numbers.Primes

largestPrime :: Int -> Int

-- The largestPrime function is how we are actually driving the problem. The argument here is the upper bound value
-- that we want to find. For Euler Problem #3, this is 600851475143. All we do in this function is call maximum on 
-- the Data.Numbers.Primes method primeFactors. Although this solution does not use much of my own code, there is clearly
-- a library out there that does this type of thing more efficiently than I could. For this reason, I am comfortable using
-- it.
largestPrime x =
	maximum (primeFactors x)