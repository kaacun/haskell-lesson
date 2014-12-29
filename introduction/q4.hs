length' []     = 0
length' (_:xs) = 1 + length' xs

take' _ []        = []
take' n _ | n < 1 = []
take' n (x:xs)    = x:take' (n-1) xs

drop' _ []         = []
drop' n xs | n < 1 = xs
drop' n (_:xs)     = drop' (n-1) xs

reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

sum' []     = 0
sum' (x:xs) = sum' xs + x

product' []     = 1
product' (x:xs) = product' xs * x

main = do
    print $ length' [1, 2, 3]
    print $ take' 2 [1, 2, 3]
    print $ drop' 2 [1, 2, 3]
    print $ reverse' [1, 2, 3, 4]
    print $ sum' [1, 2, 3]
    print $ product' [1, 2, 3]
