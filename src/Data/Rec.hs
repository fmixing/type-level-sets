{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances #-}
module Data.Rec (Rec (MkRec), singleton, rempty, rmerge, rget) where

import Data.ImprovedSet
import Type.Reflection (TypeRep, Typeable, typeRep)
import Data.Dependent.Map (DMap, fromList, DSum ((:=>)), empty, union, (!), update)

-- xs -- ключи в мапе
-- Допустим, у нас Rec Identity '[Int, Bool, Char], тогда мапа может быть 
-- DMap.fromList [typeRep @Int :=> Identity 5, typeRep @Bool :=> Identity True, typeRep @Char :=> Identity 'x']
newtype Rec f (xs :: TSet k) = MkRec (DMap TypeRep f)

-- p = singleton $ Identity (5 :: Int)
-- :t p
-- > p :: Rec Identity ('Data.ImprovedSet.MkTSet '[Int])
-- rget p :: Identity Int
-- > Identity 5
singleton :: forall a as f . (Typeable a, Set '[a] ~ as) => f a -> Rec f as
singleton elem = MkRec $ fromList [typeRep @a :=> elem]

rempty :: forall as f . Set '[] ~ as => Rec f as
rempty = MkRec empty

-- p = singleton $ Identity (5 :: Int)
-- q = singleton $ Identity (True :: Bool)
-- z = rmerge p q
-- :t z
-- > z :: Rec Identity ('Data.ImprovedSet.MkTSet '[Int, Bool])
-- let MkRec w = z
-- w ! (typeRep @Int)
-- > Identity 5
-- w ! (typeRep @Bool)
-- > Identity True
rmerge :: Rec f xs -> Rec f ys -> Rec f (Merge xs ys)
rmerge (MkRec map1) (MkRec map2) = MkRec $ union map1 map2

-- l = singleton $ Identity (4 :: Int)
-- rget l :: Identity Int
-- > Identity 4
rget :: forall x xs f. (Typeable x, IsElem x xs ~ 'True) => Rec f xs -> f x
rget (MkRec map) = map ! (typeRep @x)

-- rput ::  forall x xs f . (Typeable x, IsElem x xs) ~ True => f x -> Rec f xs -> Rec f xs
-- rput (elem :: f a) (MkRec map) = 

rcast :: IsSubset xs ys ~ True => Rec f ys -> Rec f xs


-- rreplace :: Subset xs ys => Rec f ys -> Rec f xs -> Rec f xs

-- поведение как в vinyl

-- <> операция определяем на TSet как объединение множеств