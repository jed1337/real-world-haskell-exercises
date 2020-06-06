import Data.Char (digitToInt)

xs = [1..10]

-- 1. Use a fold (choosing the appropriate fold will make your code much simpler) to
-- rewrite and improve upon the asInt function from the earlier section“Explicit Recursion” on page 85.

-- -- file: ch04/ch04.exercises.hs
-- asInt_fold :: String -> Int
-- 2. Your function should behave as follows:
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798

-- 3. Extend your function to handle the following kinds of exceptional conditions by
-- calling error:
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374
asInt::String->Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt_fold::String->Int
asInt_fold [] = 0
asInt_fold (x:xs) = let func x y = (x*10)+y
                    in if x=='-'
                        then (foldl func 0 (map digitToInt xs))*(-1)
                        else foldl func 0 (map digitToInt (x:xs))

-- 4. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite
-- the function to fix this problem:
-- -- file: ch04/ch04.exercises.hs
-- type ErrorMessage = String
-- asInt_either :: String -> Ei
-- ghci> asInt_either "33"
-- Right 33
-- ghci> asInt_either "foo"
-- Left "non-digit 'o'"
-- 5. The Prelude function concat concatenates a list of lists into a single list and has the
-- following type:
-- -- file: ch04/ch04.exercises.hs
-- concat :: [[a]] -> [a]

-- 6. Write your own definition of concat using foldr.
concatFoldr::[[a]]->[a]
concatFoldr [] = []
concatFoldr arr = let func x y = x++y
                     in foldr func [] arr

-- 7. Write your own definition of the standard takeWhile function, first using explicit
-- recursion, and then foldr.
takeWhileRecursion::(a->Bool)->[a]->[a]
takeWhileRecursion _ [] = []
takeWhileRecursion predicate (x:xs) = if predicate x
                                      then x:takeWhileRecursion predicate xs
                                      else takeWhileRecursion predicate []

-- takeWhile takes elements from the beginning of a list as long as the predicate returns True
-- takeWhileFoldr::(a->Bool)->[a]->[a]
-- takeWhileFoldr _ [] = []
-- takeWhileFoldr predicate xs = foldr (\x acc -> if predicate x then ) []] xs

-- 8. The Data.List module defines a function, groupBy, which has the following type:
-- -- file: ch04/ch04.exercises.hs
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- 9. Use ghci to load the Data.List module and figure out what groupBy does, then
-- write your own implementation using a fold.
-- 10. How many of the following Prelude functions can you rewrite using list folds?
-- • any
-- • cycle
-- • words
-- • unlines
-- For those functions where you can use either foldl' or foldr, which is more appropriate in each case

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' f xs = foldr step False xs
    where step el acc
        | f el = True
        | otherwise = acc