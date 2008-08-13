{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings

{- |
We follows the Box-Muller Method to Generate White Gaussian Noise 
described at: http:\/\/www.dspguru.com\/howto\/tech\/wgn.htm
-}

module ForSyDe.Shallow.Gaussian (
          pGaussianNoise
    )
where

import ForSyDe.Shallow.SynchronousLib
import ForSyDe.Shallow.UntimedLib
import ForSyDe.Shallow.Signal

import System.Random

-- |To generate an infinite Signal of Gaussian values
pGaussianNoise:: Double -- Mean value of the Gaussian noise
            -> Double   -- Variance of the Gaussian noise
            -> Int      -- The seed
            -> Signal Double -- Output gaussian noise signal
pGaussianNoise mean variance = mapU 2 gaussianXY . pUnitNormXY
  where
    gaussianXY [x, y] = [mean + sqrt(variance) * x,
                         mean + sqrt(variance) * y]

-- |To get a uniform random variable in the range [0, 1]
uniform :: (Fractional a, RandomGen g, Random a) => 
  g -> (a, g)
uniform rGen = randomR (0.0,1.0) rGen

-- |To generate an infinite signal of unit normal random variables,
-- with the specified seed
pUnitNormXY :: Int         -- The seed
         -> Signal Double  -- The infinite ouput signal
pUnitNormXY = mapU 3 unitNormXY . signal . svGenerator . mkStdGen
  where
    unitNormXY [s, v1, v2] = [sqrt(-2 * log(s) / s) * v1,
                              sqrt(-2 * log(s) / s) * v2]



-- |To generate the s, v1, v2 value
svGenerator :: StdGen -> [Double]
svGenerator s
    | sVal >=1 = []++ svGenerator newStdG
    | otherwise = svVal ++ svGenerator newStdG
  where
    svGen1 = svHelper  s
    svVal = fst svGen1
    sVal = head svVal
    newStdG = snd svGen1
    svHelper :: StdGen -> ([Double], StdGen)
    svHelper stdG = ([s, v1, v2], sNew2)
      where
        (u1, sNew1) = uniform stdG
        (u2, sNew2) = uniform sNew1
        v1=2 * u1 -1
        v2=2 * u2 -1
        s = v1*v1 + v2*v2