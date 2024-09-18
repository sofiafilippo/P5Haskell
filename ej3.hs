-- 3
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:x1:xs) | x >= x1 = maximo (x:xs)
                 | otherwise = maximo (x1:xs)

-- 9

--menor :: [Integer] -> Integer
--menor [x] = x
--menor (x:x1:xs) | x <= x1 = menor (x:xs)
--                | otherwise = menor (x1:xs)
quitar :: (Eq t) => t -> [t] -> [t]
quitar y [] = []
quitar y (x:xs) | y == x = xs
                | otherwise = x : quitar y xs

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar lista = ordenar(quitar(maximo lista) lista) ++ [maximo lista]


--ordenar (x:xs) = ordenar xs ++ [maximo (x:xs)]