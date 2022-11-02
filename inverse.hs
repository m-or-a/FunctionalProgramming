-- удаление i-го элемента строки
delitem :: Int -> [Int] -> [Int] 
delitem a i = do
    let (x, y) = splitAt a i
    x ++ tail y

-- формирование минора
minor :: Int ->[[Int]] -> [[Int]]
minor i a = map (delitem i) (tail a)

-- определитель
det :: [[Int]] -> Int
det a = 
    case length a of
        2 -> head (head a) * a!!1!!1 - head a!!1*head (a!!1)
        _ -> sum $ map (\i -> ((-1)^(i `mod` 2)) * (head a!!i) * det (minor i a)) [0..length a - 1]

--

-- получить j-ю строку транспонированной матрицы
createline :: Int -> [[Int]] -> [Int]
createline j a = map (\i -> (a!!i)!!j) [0..length a - 1]

-- транспонирование матрицы
trans :: [[Int]] -> [[Int]]
trans a = map (`createline` a) [0..length a - 1]

-- удаление i-й строки
delstr :: Int -> [[Int]] -> [[Int]]
delstr a i = do
    let (x, y) = splitAt a i
    x ++ tail y

-- алгебраическое дополнение
algc :: Int -> Int -> [[Int]] -> [[Int]]
algc i j a = map (delitem j) (delstr i a)

-- заполнение i-й строки значениями союзной матрицы
strinverse :: Int -> [[Int]] -> [Int]
strinverse i a = 
    case length a of
        2 -> if i == 0 then [a!!1!!1, -a!!1!!0] 
             else [-a!!0!!1, a!!0!!0]
        _ -> map (\j -> (-1)^((i + j) `mod` 2) * det (algc i j a)) [0..length a - 1]

-- союзная матрица
union :: [[Int]] -> [[Int]]
union a = map (`strinverse` trans a) [0..length a - 1] 

-- домножение элементов на 1/det
multd :: Int -> [Int] -> [Double]
multd d a = map (\i -> 1 / fromIntegral d * fromIntegral (a!!i)) [0..length a - 1]

--обратная матрица
inverse :: [[Int]] -> [[Double]]
inverse a = map (\i -> multd (det a) (union a!!i)) [0..length a - 1]

main :: IO()
main = do
    print $ inverse [[5, 2], [3, -7]]
    print $ inverse [[5, 2, 3], [0, 2, 1], [0, 1, 3]] 
    print $ inverse [[1, 2, 3, 7], [4, 3, 2, 1], [3, 2, 0, 3], [4, 8, -3, -2]] 
    