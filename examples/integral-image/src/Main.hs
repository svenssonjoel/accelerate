{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

import Data.Array.Accelerate                       as Acc
import qualified Data.Array.Accelerate.CUDA        as CUDA
import qualified Data.Array.Accelerate.Interpreter as Interp


-- |The value of each element in an integral image is the sum of all input elements
-- above and to the left, inclusive. It is calculated by performing an inclusive/post
-- scan from left-to-right then top-to-bottom.
--
integralImage :: (Elt a, IsNum a) => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
integralImage arr = sumTable
  where
    -- scan rows
    rowArr  = reshape (lift $ Z:.(w * h)) arr
    rowSegs = Acc.replicate (lift $ Z:.h) $ unit w
    rowSum  = reshape (lift (Z:.w:.h)) $ Acc.scanl1Seg (+) rowArr rowSegs

    -- scan cols
    colArr  = reshape (lift $ Z:.(h * w)) $ transpose2D rowSum
    colSegs = Acc.replicate (lift $ Z:.w) $ unit h
    colSum  = reshape (lift (Z:.h:.w)) $ Acc.scanl1Seg (+) colArr colSegs

    -- transpose back
    sumTable = transpose2D colSum

    --
    Z:.w:.h = unlift $ shape arr


-- |Simple 2D matrix transpose.
--
transpose2D :: Elt a => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
transpose2D arr = backpermute (swap $ shape arr) swap arr
  where
    swap = lift1 $ \(Z:.x:.y) -> Z:.y:.x :: Z :. Exp Int :. Exp Int


-- |An image is just a 2D array of 1s. The values don't really matter here but
-- all 1s makes it easier to validate the integral image result.
--
image :: Array DIM2 Int
image = Acc.fromList (Z:.20:.40) $ repeat 1


-- |Run integralImage through the Interpreter and CUDA backends.
--
main :: IO ()
main = do
  let cuda = CUDA.run   $ integralImage (use image)
      int  = Interp.run $ integralImage (use image)

  if toList cuda == toList int
     then putStrLn "Valid"
     else do
       putStrLn "INVALID!!"
       putStrLn $ " Interpreter: " ++ show int
       putStrLn $ " CUDA:        " ++ show cuda
