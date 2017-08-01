squareSumDiff :: Int -> Int


-- Main method for euler 6
squareSumDiff x =
	let sumSquare = sum (map (^2) [1..100])
	    squareSum = (sum [1..100]) ^ 2 in
	squareSum - sumSquare