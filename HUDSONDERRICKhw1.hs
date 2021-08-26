minlist xs = error "No Solution"

-- returns the minimum element of a list.
minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList (x:y:xs) |x > y = minList (y:xs)
                 |x < y = minList (x:xs)
                 |x == y = minList (x:xs)

-- multiply together all the elements of a list
multiplyList :: [Integer] -> Integer
multiplyList [] = 0
multiplyList [x] = x
multiplyList (x:xs) = x * multiplyList xs

-- return True if there is an odd element in a list
existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd [x] = if even x then False else True
existsOdd (x:xs) | odd x = True
                 | even x = existsOdd xs

-- return Just x if there is some x in the input that is odd
findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd [x] = if even x then Nothing else Just x
findOdd (x:xs)  | odd x = Just x
                | even x = findOdd xs

-- removes all empty strings from a list
removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty [""] = []
-- removeEmpty [x] = [x]
removeEmpty (x:xs)  | x == "" = removeEmpty xs
                    | x /= "" = x 


-- custom implementation of the parity check
myEven :: Integer -> Bool
myEven n = (mod n 2 == 0)
-- myEven n = if mod n 2 == 0 then True else False

-- using let
fact2' :: Int -> Int
fact2' n = let y = fact2' (n-1)
           in if n==0 then 1 else n * y

-- using guards
fact3 :: Int -> Int
fact3 x | x == 0    = 1
        | otherwise = x * fact3 (x-1)
