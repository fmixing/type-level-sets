{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances #-}
module Data.Rec (Rec (MkRec), rsingleton, rempty, rmerge, rget, rput, rreplace, rcast) where

import Data.ImprovedSet
import Type.Reflection (TypeRep, Typeable, typeRep, SomeTypeRep (..))
import Data.Dependent.Map (DMap, fromList, DSum ((:=>)), empty, union, (!), update, insert, filterWithKey)
import Data.Set as S (member)

-- xs -- ключи в мапе
-- Допустим, у нас Rec Identity '[Int, Bool, Char], тогда мапа может быть 
-- DMap.fromList [typeRep @Int :=> Identity 5, typeRep @Bool :=> Identity True, typeRep @Char :=> Identity 'x']
newtype Rec f (xs :: TSet k) = MkRec (DMap TypeRep f)

-- p = rsingleton $ Identity (5 :: Int)
-- :t p
-- > p :: Rec Identity ('Data.ImprovedSet.MkTSet '[Int])
-- rget p :: Identity Int
-- > Identity 5
rsingleton :: forall a as f . (Typeable a, Set '[a] ~ as) => f a -> Rec f as
rsingleton elem = MkRec $ fromList [typeRep @a :=> elem]

rempty :: forall as f . Set '[] ~ as => Rec f as
rempty = MkRec empty

-- p = rsingleton $ Identity (5 :: Int)
-- q = rsingleton $ Identity (True :: Bool)
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

-- l = rsingleton $ Identity (4 :: Int)
-- rget l :: Identity Int
-- > Identity 4
rget :: forall x xs f. (Typeable x, IsElem x xs ~ 'True) => Rec f xs -> f x
rget (MkRec map) = map ! (typeRep @x)

-- l = rsingleton $ Identity (4 :: Int)
-- o = rput (Identity (5 :: Int)) l
-- rget o :: Identity Int
-- > Identity 5
rput ::  forall x xs f . (Typeable x, IsElem x xs ~ 'True) => f x -> Rec f xs -> Rec f xs
rput (elem :: f a) (MkRec map) = MkRec $ insert (typeRep @x) elem map

-- Сужает Rec
-- p = rsingleton $ Identity (5 :: Int)
-- q = rsingleton $ Identity (True :: Bool)
-- z = rmerge p q
-- casted = rcast z :: Rec Identity (Data.ImprovedSet.Set '[Bool])
-- :t casted
-- > casted :: Rec Identity ('Data.ImprovedSet.MkTSet '[Bool])
-- rget casted :: Identity Bool
-- > Identity True
-- rget casted :: Identity Int
-- merged = rmerge casted (rsingleton (Identity (1 :: Int)))
-- rget merged :: Identity Int
-- > Identity 1
rcast :: forall xs ys f . (KnownTSet xs, IsSubset xs ys ~ 'True) => Rec f ys -> Rec f xs
rcast (MkRec map) = let set = knownTSet @_ @xs in
    MkRec $ filterWithKey (\key _ -> S.member (SomeTypeRep key) set) map

-- Заменяет элементы во второй мапе элементами первой
-- p = rsingleton $ Identity (5 :: Int)
-- q = rsingleton $ Identity (True :: Bool)
-- z = rmerge p q
-- l = rsingleton $ Identity (4 :: Int)
-- w = rreplace l z
-- rget w :: Identity Int
-- > Identity 4
-- rget w :: Identity Bool
-- > Identity True
rreplace :: (IsSubset xs ys ~ 'True) => Rec f xs -> Rec f ys -> Rec f ys
rreplace (MkRec map1) (MkRec map2) = MkRec $ union map1 map2