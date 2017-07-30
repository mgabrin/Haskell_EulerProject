import Data.List

findMultiples :: Int -> Int

findMultiples x =
	let threeMultiples = filter (\n -> n `mod` 3 == 0) [0..999]
	    fiveMultiples = filter (\n -> n `mod` 5 == 0) [0..999] in
	sum (nub (threeMultiples ++ fiveMultiples))