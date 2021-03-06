{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances #-}

module Data.ImprovedSet (Data.ImprovedSet.Set, IsElem, IsSubset, IsSubsetForLists, TSet, KnownTSet, knownTSet, Merge) where

import GHC.TypeLits
import Data.Type.Set hiding (Union, Subset, Set)
import Data.Kind (Type)
import Data.Type.Equality 
import Data.Type.Bool
import Type.Reflection (Typeable, typeOf, typeRep, SomeTypeRep(..))
import Data.Functor.Identity (Identity(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Set as S (Set, empty, member, insert)

type instance Cmp (n :: k) (m :: k) = CmpTypeNonDet n m

newtype TSet k = MkTSet [k]

-- type Set xs = MkTSet (Nub (Sort xs))

type Set xs = MkTSet (GHC.TypeLits.MySet xs)

type family IsElem (a :: k) (xs :: TSet k) :: Bool where
    IsElem a (MkTSet xs) = IsElemForLists a xs

type family IsElemForLists (x :: k) (xs :: [k]) :: Bool where
  IsElemForLists x '[] = False	 
  IsElemForLists x (x ': ys) = 'True	 
  IsElemForLists x (y ': ys) = IsElemForLists x ys

-- Первое множество является сабсетом второго
type family IsSubset (xs :: TSet k) (ys :: TSet k) :: Bool where 
    IsSubset (MkTSet xs) (MkTSet ys) = IsSubsetForLists xs ys

type family IsSubsetForLists (xs :: [k]) (ys :: [k]) :: Bool where 
    IsSubsetForLists '[] ys = 'True
    IsSubsetForLists xs '[] = 'False
    IsSubsetForLists (x : xs) (x : ys) = IsSubsetForLists xs ys    
    IsSubsetForLists (x : xs) (y : ys) = IsSubsetForLists (x : xs) ys

class KnownTSet (xs :: TSet k) where
  knownTSet :: S.Set SomeTypeRep

instance KnownTSet (MkTSet '[]) where
  knownTSet = S.empty

instance (KnownTSet (MkTSet xs), Typeable x) => KnownTSet (MkTSet (x:xs)) where
  knownTSet = S.insert (SomeTypeRep (typeRep @x)) (knownTSet @_ @(MkTSet xs))

type family Merge (xs :: TSet k) (ys :: TSet k) :: TSet k where
    Merge (MkTSet xs) (MkTSet ys) = MkTSet (GHC.TypeLits.MySet (xs ++ ys))
        
type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)