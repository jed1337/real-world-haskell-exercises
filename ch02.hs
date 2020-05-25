twoElements = [1, 2]
xs = [1..10]

-- 1. Haskell provides a standard function, last :: [a] -> a, that returns the last element of a list. From reading the type alone, what are the possible valid behaviors (omitting crashes and infinite loops) that this function could have? What are a few things that this function clearly cannot do?
-- It can't get the last item from an empty list

-- 2. Write a function, lastButOne, that returns the element before the last.
lastButOne xs = if (length xs)==2 then head xs else lastButOne (tail xs)

lastButOne' (x:y:[]) = x
lastButOne' (x:xs) = lastButOne xs


-- 3. Load your lastButOne function into ghci and try it out on lists of different lengths. What happens when you pass it a list that’s too short?
-- We get a non-exhaustive pattern error