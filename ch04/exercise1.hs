import Data.Maybe

xs = [1..10]

safeHead::[a]->Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail::[a]->Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast::[a]->Maybe a
safeLast [] = Nothing
safeLast (x:xs) = if null xs
                then Just x
                else safeLast xs

--Init code taken from https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.List.html#init
safeInit::[a]->Maybe [a]
safeInit [] = Nothing
safeInit (x:xs) = Just(safeInit' x xs)
    where safeInit' _ [] = []
          safeInit' y (z:zs) = y:safeInit' z zs