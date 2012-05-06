-- map f to each 2 list elements
mappair :: ((a,a) -> b) -> [a] -> [b]
mappair f [] = []
mappair f [x] = []
mappair f (x:xs) = f (x, head xs) : mappair f xs

-- Дано:
ival = (0,4)
f x = x/(1+x)

-- метод прямоугольников
iRect f dx = (sum fl + sum fr)/2
        where fl = map (\x -> dx * f x) [a, a+dx..b-dx]
              fr = map (\x -> dx * f x) [a+dx, a+2*dx..b]
              a = fromIntegral $ fst ival
              b = fromIntegral $ snd ival

-- метод трапеций
iTrap f dx = sum $ mappair (\(a,b) -> dx*(a+b)/2) $ map f [a, a+dx..b]
              where a = fromIntegral $ fst ival
                    b = fromIntegral $ snd ival

-- XXX: метод Cимпсона
iSimp f dx = sum [part i | i <- [1,4..n-3]]
             where part i = 3*h*(y!!i + 3*y!!(i+1) + 3*y!!(i+2)+y!!(i+3))/8
                   y = map f [a, a+dx..b]
                   h = dx
                   n = floor $ (a+b)/h
                   a = fromIntegral $ fst ival
                   b = fromIntegral $ snd ival
        
