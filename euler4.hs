largestPalindrome :: Int -> Int
checkPalindrome :: Int -> Bool
findPalindrome :: Int -> Int -> Int -> Int

-- Simple function that will check if a value is a palindrome
checkPalindrome val =
	(reverse (show val)) == (show val)


-- This is the function that is doing all of the work. It keeps track of the largest palindrome found as we iterate
-- through all of the possible multiplication solutions. NOTE: This is a basic brute force approach. It could almost
-- certainly be optimized
findPalindrome valOne valTwo largest= 
	if checkPalindrome (valOne * valTwo) && (valOne * valTwo) > largest then 
		findPalindrome valOne valTwo (valOne * valTwo)
	else if valOne > 100 then
		(findPalindrome (valOne-1) valTwo largest)
	else if valTwo > 100 then
		(findPalindrome 999 (valTwo - 1) largest)
	else
		largest

-- Driver function - calls findPalindrome. The argument to this function is not used.
largestPalindrome x =
	findPalindrome 999 999 0
