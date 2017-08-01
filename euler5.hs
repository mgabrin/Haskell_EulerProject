smallestDivisibleNum :: Int -> Int
allDivisible :: Int -> Int -> Int

-- This is the method doing all of the work here. We check to see if the value we are checking is divisble by every number
-- from 20 - 11. We can short circuit here and don't have to check values below 11 because they are all covered in the range
-- 20 - 11 
allDivisible val divisor =
	if mod val divisor == 0 && divisor == 11 then
		val
	else if mod val divisor == 0 && divisor > 11 then
		allDivisible val (divisor-1)
	else
		allDivisible (val+20) 20

smallestDivisibleNum x =
	allDivisible 20 20 