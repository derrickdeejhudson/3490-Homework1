minlist xs = error "No Solution"

-- returns the minimum element of a list.
minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList x:xs = min x (minList xs)

min :: Integer -> Integer -> Integer
min a b
    | a > b  = b
    | a < b  = a


-- using let
fact2' :: Int -> Int
fact2' n = let y = fact2' (n-1)
           in if n==0 then 1 else n * y

-- using guards
fact3 :: Int -> Int
fact3 x | x == 0    = 1
        | otherwise = x * fact3 (x-1)
