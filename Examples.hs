module Examples where

import Data.Char

-- The function to square an integer

square :: Int -> Int
square x = x * x


-- The function to double an integer

double   :: Int -> Int
double x = x + x 

-- Some type synonyms

type Grade = Char
type Username = String

type ExamResult = (Username, Grade)

-- other
sayHello :: String -> String
sayHello name = "Hello " ++ name
moreThan :: Int -> Int -> Bool
moreThan threshold n = n > threshold
increment :: Int -> Int
increment n = n+1
sphereVolume :: Float -> Float
sphereVolume r = (4/3) * pi * r*r*r
isSafe item =
 not (elem item allergens)
 where
 allergens = ["nuts","cream","prawns"]

-- my stuff

addFor x = x + 4

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

member :: Int -> [Int] -> Bool
member y [] = False
member y (x:xs) =
    if y == x
        then True
        else member y xs

beginning :: [a] -> [a]
beginning [] = []
beginning [x] = []
beginning (y:x:xs) = y:beginning (x:xs)

at :: Int -> [a] -> a
at n [] = error "Not in range"
at 0 (x:xs) = x
at n (x:xs) = at (n-1) xs

interval :: Int -> Int -> [Int]
interval i j = 
    if i == j
        then [(i)]
        else
            if i < j
               then (i : interval(i+1) j)
            else
                (i : interval(i-1) j)


duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x: duplicate xs

between :: a -> [a] -> [a]
between e [] = []
between e [x] = [x]
between e (x:xs) = x:e:(between e xs)

thrice :: (a -> a) -> a -> a
thrice f = f . f . f

trimSp :: String -> String
trimSp = dropWhile isSpace . reverse . dropWhile isSpace . reverse

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) xs (tail xs)

-- locations :: Eq a => a -> [a] -> [Int]
-- locations x ys =
--     map fst (filter (\a -> a==x) (zip [0..] ys))


-- choose :: Int -> Int -> Int
-- choose m n = (factorial m) `div` ((factorial n) * factorial (m - n))

-- improveGuess :: Double -> Double -> Double
-- improveGuess a x = 0.5 * (x + (a / x))

-- approxSqrt :: Double -> Double -> Double
-- approxSqrt a x = improveGuess a improveGuess a x

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

followingDay :: Day -> Day
followingDay Mon = Tue
followingDay Tue = Wed
followingDay Wed = Thu
followingDay Thu = Fri
followingDay Fri = Sat
followingDay Sat = Sun
followingDay Sun = Mon

isWorkDay = not . isWeekend

canStartExperiment :: Day -> Bool
canStartExperiment day =
 isWorkDay day
 && isWorkDay (followingDay day)

isPayDay :: Day -> Bool
isPayDay Fri = True
isPayDay _ = False

-- data Item = Book | DVD | Journal
-- data User = Staff | Student

data Item = Book ISBN LoanType
    | DVD Name ID
    | Journal ISSN Volume Issue
data LoanType = ShortLoan | LongLoan
type ISBN = String
type Name = String
type ISSN = String
type Volume = Int
type Issue = Int
data User = Staff ID | Student ID
type ID = Int

-- lateFine :: Item -> User -> Float
-- lateFine (Book isbn loanType) (Student id) = 1.0
-- lateFine (Book _ _) (Student _) = 0.5
-- lateFine _ (Staff _) = 3.0
-- lateFine (DVD "Jurrasic Park" _) (Student _) = 5.0

intToDay :: Int -> Maybe Day
intToDay 1 = Just Mon
intToDay 2 = Just Tue
intToDay 3 = Just Wed
intToDay 4 = Just Thu
intToDay 5 = Just Fri
intToDay 6 = Just Sat
intToDay 7 = Just Sun
intToDay _ = Nothing


data Triangle = NotATriangle | Equilateral | Isoceles | Scalene deriving Show

sidesToTriangle :: (Int, Int, Int) -> Triangle
sidesToTriangle (a, b, c) =
    if (a == b && b == c)
        then Equilateral
        else if (a == b || b == c || a == c)
            then Isoceles
            else 
                Scalene



ones = 1: ones

sieve (n:ns) =
    n : sieve (filter (notDivBy n) ns)
    where
    notDivBy d num = (num `mod` d) /= 0

