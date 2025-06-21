firstDegree :: Double -> Double -> Double
firstDegree a b = (b*(-1))/a

secondDegree :: Double -> Double -> Double -> [Double]
secondDegree a b c = if (delta < 0)
                     then []
                     else 
                         let 
                           x1 = (-b + sqrt delta) / (2*a)
                           x2 = (-b - sqrt delta) / (2*a)
                         in 
                           [x1, x2]
                     where 
                       delta = ((b*b) - (4*a*c))
