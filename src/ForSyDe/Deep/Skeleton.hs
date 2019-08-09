{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 
module ForSyDe.Deep.Skeleton where

import ForSyDe.Deep.Ids
import ForSyDe.Deep.System
import Data.TypeLevel.Num hiding ((+), div)
import Data.Param.FSVec as FSV
import Data.List as L

mkNames :: Nat s => ProcId -> FSVec s ProcId
mkNames base = FSV.map (\i-> base L.++ show i) (reallyUnsafeVector [0::Int ..])

replicate :: (SysFun a, Nat s)
          => ProcId -> s -> (ProcId -> SysDef a) -> FSVec s a
replicate name n a = FSV.zipWith (\n f -> instantiate n (f n)) (mkNames name) (copy n a)

app11 name f = FSV.zipWith (\n c -> instantiate n $ f n c) (mkNames name)

farm11 :: (SysFun (a1 -> a), Nat s)
       => ProcId -> SysDef (a1 -> a) -> FSVec s a1 -> FSVec s a
farm11 name f = FSV.zipWith (\n i -> (instantiate n f) i) (mkNames name)

farm21 :: (Nat s, SysFun (a2 -> a1 -> a))
       => ProcId -> SysDef (a2 -> a1 -> a) -> FSVec s a2 -> FSVec s a1 -> FSVec s a
farm21 name f = FSV.zipWith3 (\n i1 i2 -> (instantiate n f) i1 i2) (mkNames name)

fold :: (Nat s, SysFun (b -> a -> a))
     => ProcId -> SysDef (b -> a -> a) -> FSVec s b -> a -> a
fold name f = FSV.foldr (.) id . FSV.zipWith (\n i -> (instantiate n f) i) (mkNames name)

reduce :: (Pos s, Succ s' s, SysFun (a -> a -> a))
       => ProcId -> SysDef (a -> a -> a) -> FSVec s a -> a
reduce name f a = fold name f (FSV.tail a) (FSV.head a)

logReduce :: (Nat s, SysFun (a -> a -> a))
          => ProcId -> SysDef (a -> a -> a) -> FSVec s a -> a
logReduce name f v = go (name L.++ "0") f $ L.splitAt (L.length lst `div` 2) lst
  where
    lst = fromVector v
    go _ _ ([],_) = error "empty list"
    go _ _ (_,[]) = error "empty list"
    go n f ([a],[b]) = instantiate n f a b
    go n f ([a],b) = instantiate n f a (go (n L.++ "1") f $ L.splitAt (L.length b `div` 2) b)
    go n f (a,[b]) = instantiate n f (go (n L.++ "0") f $ L.splitAt (L.length a `div` 2) a) b
    go n f (a,b) = instantiate n f
                   (go (n L.++ "0") f $ L.splitAt (L.length a `div` 2) a)
                   (go (n L.++ "1") f $ L.splitAt (L.length b `div` 2) b)

generate :: (Nat s, SysFun (a -> a))
         => ProcId -> s -> SysDef (a -> a) -> a -> FSVec s a
generate name n pc s = unsafeVector n $ L.foldr (\nm prev -> (instantiate nm pc) (L.head prev) : prev) [s] names
  where
    names = L.take (toInt n Prelude.- 1) $ L.map (\i-> name L.++ show i) [0..]

generate2 :: (Nat s, SysFun (b -> a -> a))
          => ProcId -> s -> SysDef (b -> a -> a) -> FSVec s b -> a -> FSVec s a
generate2 name n pc args s = unsafeVector n $ L.foldr (\(nm,arg) prev -> (instantiate nm pc arg) (L.head prev) : prev) [s]
                              $ L.zip names (FSV.fromVector args)
  where
    names = L.take (toInt n Prelude.- 1) $ L.map (\i-> name L.++ show i) [0..]
