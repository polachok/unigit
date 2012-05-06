-- Helper functions
-- Apply function to itself multiple times
apply 0 f = error "What d'ya mean, apply function zero times?"
apply times f = foldl1 (.) $ replicate times f

-- map f to each 2 list elements
--mappair :: ((a,c) -> b) -> [a] -> [b]
mappair f [] = []
mappair f [x] = []
mappair f (x:xs) = f (x, head xs) : mappair f xs

-- naive factorial
fact 0 = 1
fact x = x * (fact $ x - 1)

-- Дано:
x = [0.18, 0.185..0.225]
y = [5.615, 5.467, 5.352, 5.193,
     5.066, 4.946, 4.832, 4.722,
     4.618, 4.519]

x1 = 0.182

-- Конечные разности
diffr ys = mappair diff ys
           where diff (a, b) = b - a

kr ys order = apply (order+1) diffr $ ys

-- Шаг интерполяции
step xs = maximum $ diffr xs

-- Интерполяционный полином
p x1 n = y!!0 + sum [part k | k <- [1..n]]
      where krf = (!!0) . kr y
            h = step x
            part k = ((foldl1 (*) $ map (\i -> x1 - x!!i) [0..k-1])
                      * (krf (k-1)))/(fromIntegral (fact k) * h^k)
