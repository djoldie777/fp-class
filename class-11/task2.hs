{-
2. Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell.
-}

import Control.Monad.Writer

eps = 0.000001


summand :: Double -> Double -> Double -> Double
summand p n x = ((-1) * p * (x^2)) / ((n + 1) * (n + 2))


taylorRow :: Double -> Double -> Double -> Double -> Writer [Double] Double
taylorRow value prev next x = tell [prev] >>
  if abs (prev - (summand prev next x)) < eps then return value
  else taylorRow (value + (summand prev next x)) (summand prev next x) (next + 2) x


sin' :: Double -> (Double, [Double])
sin' x = runWriter $ taylorRow x x 1 x


cos' :: Double -> (Double, [Double])
cos' x = runWriter $ taylorRow 1 1 0 x