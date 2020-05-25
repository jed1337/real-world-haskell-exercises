durianList = fromList "durian"
justMonikaList = fromList [Just "Monika"]

optionalTree = OptionalTree 'a'

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- 1. Write the converse of fromList for the List type: a function that takes a List a and generates a [a].
toList Nil = []
toList (Cons a b) = a : toList b

-- 2. Define a tree type that has only one constructor, like our Java example. Instead of the Empty constructor, use the Maybe type to refer to a node’s children.
data OptionalTree a = OptionalTree a (Maybe (OptionalTree a)) (Maybe (OptionalTree a)) deriving (Show)