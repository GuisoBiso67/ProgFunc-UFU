par1 x = if (x==0)
         then True 
         else not (par1 (x-1))
-- in 'else', the program returns the negation of par1 (x-1), that is necessarily a pair number;
