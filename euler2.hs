evenFib :: Int -> Int
fib :: Int -> Int
findPoint :: Int -> Int -> Int

-- Basic fibonacci sequence function
fib 0 = 1
fib 1 = 2
fib x =
	fib(x-1) + fib(x-2)

-- Helper function that finds the index of the fibonacci sequence where a val is less than a threshold.
-- For the second Euler problem, this means finding the last index of the fib sequence where it is less
-- than 4 million.
findPoint x threshold =
	if fib(x) > threshold then
		x
	else
		findPoint (x+1) threshold

-- Control function. This is where we actually do the work to find the sum. Note: The 
evenFib x =
	let topVal = findPoint 0 4000000
	    curArray = [1..topVal] in
	sum (filter (even) (map (fib) (curArray)))
	
