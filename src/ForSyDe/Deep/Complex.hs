{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable #-}
module ForSyDe.Deep.Complex (
  addCpx, subCpx, mulCpx, (+:), (-:), (*:), 
  module Data.Complex
  ) where

import Language.Haskell.TH.Lift
import Data.Complex

infixl 6 +:, -:
infixl 7 *:

addCpx :: Num a => Complex a -> Complex a -> Complex a
addCpx (x :+ y) (x' :+ y') = (x + x') :+ (y + y')

subCpx :: Num a => Complex a -> Complex a -> Complex a
subCpx (x :+ y) (x' :+ y') = (x - x') :+ (y - y')

mulCpx :: Num a => Complex a -> Complex a -> Complex a
mulCpx (x :+ y) (x' :+ y') = (x*x'-y*y') :+ (x*y'+y*x')

(+:) :: Num a => Complex a -> Complex a -> Complex a
(-:) :: Num a => Complex a -> Complex a -> Complex a
(*:) :: Num a => Complex a -> Complex a -> Complex a
(+:) = addCpx
(-:) = subCpx
(*:) = mulCpx

$(deriveLift1 ''Complex)
