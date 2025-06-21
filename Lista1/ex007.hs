mdc2 :: Int -> Int -> Int
mdc2 a b = if (mod a b == 0)
           then b
           else mdc2 b (mod a b)
           
mdc3 :: Int -> Int -> Int -> Int
mdc3 a b c = mdc2 (mdc2 a b) c
