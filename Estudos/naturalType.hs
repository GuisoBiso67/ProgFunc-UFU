data Natural = Natural Integer

constroiNatural :: Integer -> Natural
constroiNatural x =
   if x<0 then error "Numero Negativo!"
          else Natural x
deNatural :: Natural -> Integer
deNatural (Natural x) = x

instance Eq Natural where
   x == y = (deNatural x) == (deNatural y)

instance Num Natural where
   fromInteger = constroiNatural
   x + y = constroiNatural (deNatural x + deNatural y)
   x - y = let r = deNatural x - deNatural y in
              if r < 0 then error "Numero Negativo"
              else constroiNatural r
   x * y = constroiNatural (deNatural x * deNatural y)
   negate x = let r = negate (deNatural x) in
              if r<0 then error "Numero Negativo"
                     else constroiNatural r
   abs x = x
   signum x = constroiNatural (signum (deNatural x))

instance Show Natural where
   show (Natural x) = "Natural " ++ show x
