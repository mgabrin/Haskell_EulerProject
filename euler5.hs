smallestDivisibleNum :: Int -> Int
allDivisible :: Int -> Int -> Int

-- This is the method doing all of the work here. We check to see if the value we are checking is divisble by every number
-- from 20 - 3. We can short circuit here and don't have to check values 1 and 2. We don't have to check 2 because we know
-- that if a number is divisble by 20 it will also be divisble by 3. All numbers are divisble by 1, so we don't have to 
-- check it either
allDivisible val divisor =
	if mod val divisor == 0 && divisor == 3 then
		val
	else if mod val divisor == 0 && divisor > 3 then
		allDivisible val (divisor-1)
	else
		allDivisible (val+20) 20

-- driver program. 
smallestDivisibleNum x =
	allDivisible 20 20 