-- Дано:
ival = (0.2, 1.2)
y' x y = 0.173*(x^2 + cos (0.7*x))+0.754*y
h = 0.25

-- Число шагов
n = floor $ (b-a)/h
     where (a, b) = ival

y 0 = 0.25
y k = y (k-1) + h * y' (x!!(k-1)) (y (k-1))
      where x = [a,a+h..b]
            (a, b) = ival
-- Ответ
r = map y [0..n]
