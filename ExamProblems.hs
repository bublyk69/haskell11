--------------------------------------------------------
-- Tested
--------------------------------------------------------

{-
Напишіть функцію suffix, що знаходить суфікси заданого списку.
Test cases:
suffix "abc" == ["abc","bc","c",""]
suffix "" == [""]

-}

suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix (x:xs) = (x:xs):(suffix xs)

{-
Рядок можна розглядати як мультимножину символів. 
Наприклад, "abac" - множина що має два символи 'a' і по одному символу 'b' і 'c'.
Напишіть функцію bagIntersect st1 st2, котра бере мультимножини st1 та st2 і поветрає їх перетин.
Перетин мультимножини X і Y містить всі елементи, що зустрічаються в X і Y кількість повторень
елементу в перетині, це мінімальна кількість його повторень в X або Y.
Test cases:
bagIntersect "abbc" "abc" == "abc"
bagIntersect "abbc" "" == ""
bagIntersect "" "abbc" == ""
bagIntersect "abbc" "bccc" == "bc"
bagIntersect "abbckk" "bccckk" == "bckk"
bagIntersect "abbckk" "bccck" == "bck"

-}
bagIntersect :: String -> String -> String
bagIntersect _ [] = []
bagIntersect [] _ = []
bagIntersect (x:xs) ys | x `elem` ys = x:(bagIntersect xs (remove x ys))
                       | otherwise = (bagIntersect xs (remove x ys))

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove q (w:ws) | q == w = ws
                | otherwise = w:(remove q ws)

{-
Реалізувати функцію primeNotDiv3LN :: Int -> [Int], яка генерує елементи зі списку цілих
парних чисел, які не діляться на 3 та меньші за N.
Test cases:
primeNotDiv3LN 0 == []
primeNotDiv3LN 3 == [2]
primeNotDiv3LN 4 == [2]
primeNotDiv3LN 17 == [2,4,8,10,14,16]

-}
primeNotDiv3LN :: Int -> [Int]
primeNotDiv3LN x = [t | t <- [1..x], t `mod` 3 /= 0, even t]

{-
Список можна розглядати як мультимножину симолів.
Наприклад, "abac" - множина що має два символи 'a' і по одному символу 'b' і 'c'.
Напишіть функцію bagDif st1 st2, котра бере мультимножини st1 та st2 і поветрає їх різницю.
Різниця мультимножин X і Y містить всі елементи, що зустрічаються в X не меньшу кількість раз ніж в Y
кількість повторень елементу в перетині, це кількість його повторень в X мінус кількість повторень в Y.
Test cases:
bagDif "abc" "" == "abc"
bagDif "" "abc" == ""
bagDif "aabbcc" "abc" == "abc"
bagDif "abc" "aabbcc" == ""
bagDif "abc" "abc" == ""

-}

bagDif :: String -> String -> String
bagDif x [] = x
bagDif [] _ = []
bagDif (x:xs) ys | a >= b = c ++ (bagDif d e) -- x count in X bigger than x count in Y
                 | otherwise = bagDif d e -- ignore x
 where a = count x (x:xs) -- count of repeated x in X
       b = count x ys -- count of repeated x in Y
       c = generate x (a-b) -- list of a-b x values
       d = removeAll x xs -- X without x values
       e = removeAll x ys -- Y without x values

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (y:ys) = [a | a <- (y:ys), a /= x]

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys) | x == y = 1 + (count x ys)
               | otherwise = count x ys

generate :: a -> Int -> [a]
generate x n | n > 0 = x:(generate x (n-1))
             | otherwise = []

{-
Список можна розглядати як мультимножину симолів.
Наприклад, "abac" - множина що має два символи 'a' і по одному символу 'b' і 'c'.
Напишіть функцію bagSubbag st1 st2, котра поветрає True, якщо мультимножина st1 являється підмультимножиною st2.
X - підмультимножина Y, якщо кожний елемент X зустрічається в Y не меншу кількість раз ніж в X.
Test cases:
bagSubbag "" "" == True
bagSubbag "" "abc" == True
bagSubbag "abc" "abc" == True
bagSubbag "abc" "abcd" == True
bagSubbag "abcc" "abcd" == False
bagSubbag "abcc" "abccd" == True
bagSubbag "acc" "abccd" == True

-}

bagSubbag:: String->String->Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (x:xs) ys 
 |contains = True && (bagSubbag xs (delElFirst x ys)) 
 |otherwise = False
 where contains = foldl (\acc y->if x==y then acc || True else acc) False ys

-- допоміжна функція для bagSubbag
delElFirst::(Eq a) =>a-> [a] -> [a]
delElFirst _ [] = []
delElFirst el (l:list)
 |el==l = list
 |otherwise = l:(delElFirst el list)

{- or
bagSubbag :: String -> String -> Bool
bagSubbag [] [] = True
bagSubbag [] _ = True
bagSubbag (x:xs) ys = a <= b && (bagSubbag xs ys)
 where a = count x (x:xs) -- count of repeated x in X
       b = count x ys -- count of repeated x in Y
-}


--------------------------------------------------------
-- IO
--------------------------------------------------------

getFileName :: String -> IO String
getFileName prompt = do
 putStr prompt
 getLine

{-
Перевірити баланс дужок
Test cases:
checkBalance "(()())" == True 
checkBalance "()" == True
checkBalance "()()()" == True
checkBalance "((()))" == True
checkBalance "(2+2)*2-(3-3)" == True
checkBalance "" == True
checkBalance "(" == False
checkBalance "())" == False
checkBalance ")" == False
checkBalance ")(" == False
checkBalance "(asdw((asca)" == False
checkBalance "(Hello" == False

-}
balance :: IO()
balance = do
 from <- getFileName "From: "
 s <- readFile from
 case (checkBalance s) of
  True -> putStr "Balanced\n"
  False -> putStr "Unbalanced\n"

checkBalance :: String -> Bool
checkBalance [] = True
checkBalance xs = helper xs []
 where
 helper "" stack = (length stack) == 0
 helper (x:xs) stack | x == '(' = helper xs (x:stack) -- push
                     | x == ')' && (length stack) == 0 = False
                     | x == ')' = helper xs (tail stack) -- pop
                     | otherwise = helper xs stack -- ignore


--------------------------------------------------------
-- Not tested
--------------------------------------------------------

-- Відсортувати список чисел за к-ть дільників
sortDividers :: [Int] -> [Int]
sortDividers xs = sortDividers $ map (head) [t | t <- map (dividers) xs]

-- Допоміжна функція, що виводить список списків [число, к-ть дільників]
dividers :: Int -> [Int]
dividers x = [x, length [t | t <- [1..x], x `mod` t == 0, t /= 1, t /= x]]

-- Транспонування матриць(рядки міняються на стовпчики)
transposes :: [[a]] -> [[a]]
transposes [] = []
transposes ([] : xss) = transposes xss
transposes ((x:xs):xss) = (x:[t | (t:_) <- xss]) : transposes (xs : [h | (_:h) <- xss])


-- сума списків
listSum :: [Int] -> [Int] -> [Int]
listSum [] y = y 
listSum x [] = x
listSum x y = [(head x) + (head y)] ++ listSum (tail x) (tail y)


-- Міняємо місцями парні та непарні
oddEven :: [Int] -> [Int] 
oddEven [x] = [x]
oddEven [x,y] = [y,x]
oddEven xs = [(head(tail xs)), (head xs)] ++ oddEven (tail(tail xs))


-- Вивести індекс першого входження числа
position:: Int->[Int]->Int
position el list = snd ( head ( filter (\q->(fst q)==el) qq))
 where qq=zip list [0..]


-- Вивести список чисел, без повторень
set :: [Int] -> [Int]
set [] = []
set (x:xs) = x :( set (delEl x xs))

delEl ::(Eq a) =>a-> [a] -> [a]
delEl x xs = [t | t <- xs, t /= x]

-- Поєднання списків без дублікатів
unionL :: [Int] -> [Int] -> [Int]
unionL xs [] = xs
unionL [] xs = xs
unionL xs (y:ys) = (head xs): (unionL (delEl y xs) ys)


-- Сума елементів списку через foldr
sumFr :: [Int] -> Int
sumFr xs = foldr (\ x acc  -> acc + x) 0 xs


-- Факторіал через foldl
factorialFoldl :: Int -> Int
factorialFoldl x
  | x> 0 = foldl(\ xs acc -> acc*xs) 1 [1..x] 
  |otherwise = -1

-- Виводить к-ть простих, додатніх чисел
primeCnt :: [Int] -> Int
primeCnt xs = length [t | t <- xs, t > 0, last (dividers t) == 0 ]