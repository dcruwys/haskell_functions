rootList :: [Double] -> [Double]
rootList [] = []
rootList x = map sqrt (x)

multiplyPairs :: [(Int, Int)] -> [Int]
multiplyPairs = map (\(x,y) -> x*y)

incrementList :: Int -> [Int] -> [Int]
incrementList f x = map (+f) (x)

squareSum :: [Integer] -> Integer
squareSum x = sum (map (^2) (x))

boolAnd :: [Bool] -> Bool
boolAnd [] = True
boolAnd (x:xs) = x && boolAnd xs

boolOr :: [Bool] -> Bool
boolOr [] = False
boolOr (x:xs) = x || boolOr xs

dupList :: [Integer] -> [Integer]
dupList [] = []
dupList x = concatMap (replicate 2) x

trueCount :: [Bool] -> Int
trueCount x = length (filter (==True) x)

rotateTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
rotateTriples = map (\(x,y,z) -> (y, z, x))

concatLists :: [[Int]] -> [Int]
concatLists xss = foldr (++) [] xss

isOlder :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isOlder (y1, m1, d1) (y2, m2, d2)
  | y1 < y2 = True
  | y1 == y2 && m1 < m2 = True
  | y1 == y2 && m1 == m2 && d1 < d2 = True
  | otherwise = False

numberInMonth :: [(Int, Int, Int)] -> Int -> Int
numberInMonth date x = foldl(\i (_,m,_)->if m == x then i+1 else i) 0 date

numberInMonths :: [(Int, Int, Int)] -> [Int] -> Int
numberInMonths date x = foldl(\i (_,m,_)->if elem m x then i+1 else i) 0 date

datesInMonth :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
datesInMonth date x = filter(\(_, m, _)->m==x) date

datesInMonths :: [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
datesInMonths date x = filter(\(_, m, _)->elem m x) date

getNth :: [String] -> Int -> String
getNth string n = string !! (n-1)

dateToString :: (Int, Int, Int) -> String
dateToString (y, m, d) = let months = ["January", "Febraury", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
                         in (getNth months m) ++ " " ++ show d ++ ", " ++ show y

numberBeforeReachingSum :: Int -> [Int] -> Int
numberBeforeReachingSum total listofints = length (filter (< total) (scanl1 (\x y ->(x+y)) listofints))

whatMonth :: Int -> Int
whatMonth day = (let daysinmonth = [31,28,31,30,31,30,31,31,30,31,30,31]
                  in numberBeforeReachingSum day daysinmonth)+1

daysinMonth :: Int -> Int
daysinMonth m = (let daysinmonth = [31,28,31,30,31,30,31,31,30,31,30,31]
                  in  (daysinmonth !! (m-1)))


monthRange :: Int -> Int -> [Int]
monthRange start end =
  let days = [start..end]
  in map (\s -> whatMonth s) days

oldest :: [(Int, Int, Int)] -> Maybe (Int, Int, Int)
oldest [] = Nothing
oldest dateList = Just (minimum dateList)

validDate :: (Int, Int, Int) -> Bool
validDate (y,m,d)
  | y <= 0 = False
  | (m <= 0 || m > 12) = False
  | (d <= 29 && m == 2 && ((y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0 ))) = True
  | (d > daysinMonth m ) = False
  | otherwise = True
