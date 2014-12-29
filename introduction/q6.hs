perpPoint (p, q) (a, b, c) = (x, y) 
    where
        x = p - (a*p + b*q -c)/(a*a + b*b) * a
        y = q - (a*p + b*q -c)/(a*a + b*b) * b

main = do
    print $ perpPoint (0, 2) (1, -1, 0)
