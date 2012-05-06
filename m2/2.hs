ival = (0, 4)
f x = 3*(x**3) - 10*(x**2)+2*x-7
f' x = 9*(x**2) - 20*x + 2
f'' x = 18*x - 20

x 0 = snd ival
x k = x (k-1) - f (x (k-1))/f' (x (k-1))

doit f f'' = if f (x 0) * f'' (x 0) < 0
                then error "convergence condition not met"
                else (xs, abs $ xs!!3 - xs!!2)
                     where xs = map x [0..3]
