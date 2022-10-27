-- удаление i-го элемента строки
delitem :: Int -> [Int] -> [Int] 
delitem a i = do
    let (x, y) = splitAt a i
    x ++ tail y

--формирование минора
minor :: Int ->[[Int]] -> [[Int]]
minor i a = map (delitem i) (tail a)

--определитель
det :: [[Int]] -> Int
det a = 
    case length a of
        2 -> head (head a) * a!!1!!1 - head a!!1*head (a!!1)
        _ -> sum $ map (\i -> ((-1)^(i `mod` 2)) * (head a!!i) * det (minor i a)) [0..length a - 1]

main :: IO()
main = do
    print "det(A): "
    print $ det [[5, 2], [3, -7]]
    print $ det [[1, 2, 3], [1, -2, 1], [2, 1, 3]] 
    print $ det [[5, 2, 3], [0, 2, 1], [0, 1, 3]] 
    print $ det [[1, 2, 3, 7], [4, 3, 2, 1], [3, 2, 0, 3], [4, 8, -3, -2]] 
    