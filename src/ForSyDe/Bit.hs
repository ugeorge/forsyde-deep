{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}  
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Bit
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- 'Bit' Datatype. Note that the 'Num' instance is phony and shouldn't be used
-- 
-----------------------------------------------------------------------------
module ForSyDe.Bit (Bit(..),not)where

import Language.Haskell.TH.Lift
import Data.Bits
import Data.Typeable (Typeable)
import Prelude hiding (not)

data Bit = H -- ^ High value 
         | L -- ^ Low value
 deriving (Eq, Show, Typeable)

$(deriveLift1 ''Bit)

-- | Not operation over bits
not :: Bit -> Bit
not = complement


instance Bits Bit where
 H .&. x = x
 L .&. _ = L
 H .|. _ = H
 L .|. x = x
 xor H L = H
 xor L H = H
 xor _ _ = L
 complement L = H
 complement H = L
 shift x 0 = x
 shift _ _ = L
 rotate x _ = x
 bitSize _ = 1
 isSigned _ = False


-- Phony instance of Num
instance Num Bit where
 H + L = H
 H + H = L
 L + x = x
 -- since they are unsigned and there are only two elements, (-) == (+)
 (-) = (+)
 -- multiplication is equivalent to (.&.)
 (*) = (.&.)
 -- since a bit is unsigned, it is equivalent to identity
 abs = id
 signum _ = L
 fromInteger n = if n<=0 then L else H


 

 

 

 
   
