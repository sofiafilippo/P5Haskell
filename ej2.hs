-- 5
quitar :: (Eq t) => t -> [t] -> [t]
quitar y [] = []
quitar y (x:xs) | y == x = xs
                | otherwise = x : quitar y xs

-- 6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos y (x:xs) | y == x = quitarTodos y xs
                     | otherwise = x : quitarTodos y xs

-- 7
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece y [] =  False
pertenece y (x:xs) | y == x = True
                   | otherwise = pertenece y xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos xs