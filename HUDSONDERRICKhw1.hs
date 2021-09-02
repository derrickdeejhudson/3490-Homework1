-- minlist xs = error "No Solution"

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
removeEmpty (x:xs) = if x == "" then removeEmpty xs else x:[] ++ removeEmpty xs

-- makes a list of non-Nothing values
catMaybes :: [Maybe a] -> Maybe a
catMaybes xs = error "No Solution"
--catMaybes [] = []
--catMaybes (x:xs) = if x == Nothing then catMaybes xs else [xs] ++ catMaybes xs
--catMaybes (Nothing:xs) = catMaybes xs
--catMaybes (Just y:xs) = Just y

--catMaybes (x:xs) = case x of
--        Nothing -> catMaybes xs
--        Just y -> Just y:[] ++ catMaybes xs

--catMaybes (Nothing:xs) = catMaybes xs
--catMaybes (Just y:xs) = Just y:[] ++ catMaybes xs

collect :: [Either a b] -> ([a], [b])
collect xs = error "No Solution"

isPrefix :: (Eq a) => [a] -> [a] -> [a]
isPrefix xs = error "No Solution"

findIndex :: Integer -> [Integer] -> Maybe Integer
findIndex xs = error "No Solution"

repeatInt :: a -> Integer -> [a]
repeatInt xs = error "No Solution"

--tag every element with index list
addIndex :: [a] -> [(Integer, a)]
addIndex xs = addIndexHelper xs 0

addIndexHelper :: [a] -> Integer -> [(Integer, a)]
addIndexHelper [] n = []
addIndexHelper (x:xs) n = (n,x): addIndexHelper xs (n + 1)

--invert the pairs of a list
swapAll :: [(a,b)] -> [(b,a)]
swapAll [] = []
swapAll [(x,y)] = [(y,x)]
--swapAll [(x,y):xs] = [(y,x)] swapAll xs
--swap (a,b) = (b,a)

--swapAllHelper ::[(a,b)] -> Integer -> [(b,a)]
--swapAllHelper [] n = []
--swapAllHelper (x:y:xs) n = (x,y): swapAll xs (y,x) (n + 1)

findDouble :: a -> [(a,a)]
findDouble xs = error "No Solution"

-- True if matches the form (Just x)
defined :: Maybe a -> Bool
defined Nothing = False
defined _       = True

--skips every other entry on the list
skip :: [a] -> [a]
skip xs = error "No Solution"

--removes evens
removeEvens :: [Integer] -> [Integer]
removeEvens [] = []
removeEvens (x:xs) | odd x = x : removeEvens xs
                   | otherwise = removeEvens xs

--doubles all entries
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2*x | x<-xs]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

-- takes lists and makes it one list
flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:xs) = flatten xs
flatten ((x:xs):xxs) = x:flatten (xs:xxs)

countInt :: Integer -> [Integer] -> Integer
--countInt [] n = 0
countInt xs = error "No Solution"

countEq :: Eq a => [a] -> a -> Integer
countEq [] n = 0
countEq (x:xs) n | n == x = 1 + (countEq xs n)
                 | otherwise = countEq xs n



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
