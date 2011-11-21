
doubleMe :: Int -> Int
doubleMe x = x + x

doubleSmallNumbers :: Int -> Int
doubleSmallNumbers x = ( if x > 100 then x else x*2) + 1

boomBang :: [Int] -> [[Char]]
boomBang xs = [ if x < 10 then "BOOM" else "BANG" | x <- xs]

removeUpperCase :: [Char] -> [Char]
removeUpperCase str = [ c | c <- str, c `elem` ['a'..'z'] ]

length' :: [Char] -> Int
length' str = sum [ 1 | _ <- str ]

length'' :: [a] -> Int
length'' [] = 0
length'' (_:t) = 1 + length'' t

first :: (a, b) -> a
first (x1, _) = x1

second :: (a, b) -> b
second (_, x2) = x2

rightTriangle :: Int -> Int -> Int -> [(Int,Int,Int)]
rightTriangle a1 a2 a3 = [ (a,b,c) | a <- [1..a1], b <- [1..a2], c <- [1..a3], a^2 + b^2 == c^2, a+b+c == 24 ]

lucky :: (Integral a) => a -> String
lucky 7 = "You rock!"
lucky x = "You suck!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, Dumb!"
head' (h:_) = h

head'' :: [a] -> a
head'' xs = case xs of [] -> error "Can't call head on an empty list, Dumb ass!"
		       (x:_) -> x	

bmiCalc :: (RealFloat a) => a -> a -> a
bmiCalc w h = w / h^2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
	| bmiCalc w h <= 18.5 = "You are underweighted, eat something!"
	| bmiCalc w h <= 25.0 = "You damn normal son of a bitch!"
	| bmiCalc w h <= 30.0 = "You are overweighted, try not to eat!"
	| otherwise   = "You are a fuckin' truck!"

max' :: (Ord a) => a -> a -> a
max' a b 
	| a > b = a
	| otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
	| a > b     = GT
	| a == b    = EQ
	| otherwise = LT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
	where (f:_) = firstName
	      (l:_) = lastName

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [ bmi w h | (w,h) <- xs ] 
	where bmi weight height = weight / height^2


calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [ bmi | (w,h) <- xs, let bmi = w / h^2 ]


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
	| n <= 0 = []
	| otherwise = x:replicate' (n-1) x


take' :: (Num a, Ord a) => a -> [i] -> [i]
take' n x
	| n <=0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:tail) = reverse'(tail) ++ [h]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t)
	| x == h = True
	| otherwise = elem' x t

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] 
quicksort (head:tail) =
	let firstHalf  = quicksort [ x | x <- tail, x <= head ]
	    secondHalf = quicksort [ x | x <- tail, x > head ]
	in  firstHalf ++ [head] ++ secondHalf

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f ( f x ) 


zipWith' :: ( a -> b -> c ) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: ( a -> b -> c ) -> (b -> a -> c)
flip' f x y = f y x

map' :: ( a -> b ) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: ( a -> Bool ) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
	| f x == True = x : filter' f xs
	| otherwise   = filter' f xs


sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldl (\acc y -> if x == y then True else acc) False xs


map'' :: ( a -> b ) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
























