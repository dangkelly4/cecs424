module Main where

--main function
main = do 
  putStrLn "Using Quicksort: "  
  --print quicksort
  print(qsort[4, 65, 2, -31, 0, 99, 2, 83, 782, 1]) 

  putStrLn "Using Mergesort: " --print mergesort
  print(msort[4, 65, 2, -31, 0, 99, 2, 83, 782, 1])

--qsort function
--takes in a list and returns a stored list 
qsort :: Ord a => [a] -> [a] 
qsort [] = [] --base case; returns empty list if qsort is called w/ empty list

--sort a list in which the first element is x and the rest of the list is xs
qsort (x : xs) = qsort smallList ++ [x] ++ qsort largeList --concatenate both smallList and largeList with pivot [x]
  where 
  smallList = [small | small <- xs , small <= x] --list of elements in xs that are less than or equal to x
  largeList  = [large | large <- xs , large >  x] ----list of elements in xs that are greater than or equal to x


--split list in two
firstHalf  xs = let { n = length xs } in take (div n 2) xs --splitting first half of the list; "take" is used to make a new list containing just the first N elements from an existing list
secondHalf xs = let { n = length xs } in drop (div n 2) xs --splitting second half of the list; "drop" is used to delete the first N elements from a list

--merge help function
mergeHelp [] ys = ys --base case; merging empty list with a list = list
mergeHelp xs [] = xs --base case; merging a list with an empty list = list
mergeHelp (x:xs) (y:ys) = if x <= y --takes smaller number from the first elements of the lists; call mergeHelp recursively until all elements from the lists are fully merged
                      then x : mergeHelp xs (y:ys)
                      else y : mergeHelp (x:xs) ys

--merge sort function
msort :: Ord a => [a] -> [a]
msort [] = [] -- base case; returns empty list if msort is called with an empty list
msort [a] = [a] --base case; handles one element list
msort xs = mergeHelp (msort (firstHalf xs)) (msort (secondHalf xs)) --this splits the list into two, sorting each half of the list and then merging them together

