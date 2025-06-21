test n = if (n `mod` 2 == 0)
         then n
         else test (2*n+1)
-- any odd number makes the program run infinitely
