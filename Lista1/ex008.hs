fib :: Int -> Int
fib a = if(a==0)
        then 0
        else if (a==1)
        then 1
        else fib(a-1) + fib(a-2)

