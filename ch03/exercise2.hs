tenDigits = [1..10]
palindrome = tenDigits ++ reverse tenDigits

-- 1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.
myLength::[a]->Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 2. Add a type signature for your function to your source file. To test it, load the source file into ghci again.
--Updated the function in #1

-- 3. Write a function that computes the mean of a list, i.e., the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating-point number.)
-- mean::[Int]->Float
mean xs = fromIntegral (sum xs)/fromIntegral (myLength xs)

-- 4. Turn a list into a palindrome; i.e., it should read the same both backward and forward. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
toPalindrome list = list ++ reverse list

-- 5. Write a function that determines whether its input list is a palindrome.
isPalindrome::Eq a=>[a]->Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = ((head xs) == (last xs)) && isPalindrome (init (tail xs))

-- 6. Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)
-- 7. Define a function that joins a list of lists together using a separator value: -- file: ch03/Intersperse.hs intersperse :: a -> [[a]] -> [a]
-- 8. The separator should appear between elements of the list, but it should not follow the last element. Your function should behave as follows: ghci> :load Intersperse [1 of 1] Compiling Main ( Intersperse.hs, interpreted ) Ok, modules loaded: Main. ghci> intersperse ',' [] ""ghci> intersperse ',' ["foo"] "foo"ghci> intersperse ',' ["foo","bar","baz","quux"] "foo,bar,baz,quux"
-- 9. Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x"Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
-- 10. Consider three two-dimensional points, a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
-- 11. Write a function that calculates the turn made by three two-dimensional points and returns a Direction.
-- 12. Define a function that takes a list of two-dimensional points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction.
-- 13. Using the code from the preceding three exercises, implement Graham’s scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how the Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia (http://en.wikipedia.org/).