{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, EmptyDataDecls, ScopedTypeVariables, TypeApplications, FlexibleInstances #-}

module Data.Union (Data.Union.Set, Union(MkUnion), ulift, umatch, urelax, urestrict) where

import GHC.TypeLits
import Data.Type.Set hiding (Union, Subset, Set)
import Data.Kind (Type)
import Data.Type.Equality 
import Data.Type.Bool
import Type.Reflection (Typeable, typeOf, typeRep, SomeTypeRep(..))
import Data.Functor.Identity (Identity(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Set as S (Set, empty, member, insert)

type instance Cmp (n :: k) (m :: k) = BetterCmpType n m

data T (a :: k)

type family BetterCmpType (a :: k1) (b :: k2) :: Ordering where
  BetterCmpType a b = CmpTypeNonDet (T a) (T b)

newtype TSet k = MkTSet [k]

type Set xs = MkTSet (Nub (Sort xs))

type family IsElem (a :: k) (xs :: TSet k) :: Bool where
    IsElem a (MkTSet xs) = IsElemForLists a xs

type family IsElemForLists (x :: k) (xs :: [k]) :: Bool where
  IsElemForLists x '[] = 'False
  IsElemForLists y (x : xs) = (y == x) || IsElemForLists y xs

-- Первое множество является сабсетом второго
type family IsSubset (xs :: TSet k) (ys :: TSet k) :: Bool where 
    IsSubset (MkTSet xs) (MkTSet ys) = IsSubsetForLists xs ys

type family IsSubsetForLists (xs :: [k]) (ys :: [k]) :: Bool where 
    IsSubsetForLists '[] ys = 'True
    IsSubsetForLists xs '[] = 'False
    IsSubsetForLists (x : xs) (y : ys) = If (x == y) (IsSubsetForLists xs ys) (IsSubsetForLists (x : xs) ys)

data Union :: (k -> *) -> TSet k -> * where
  MkUnion :: (Typeable a, IsElem a xs ~ True) => f a -> Union f xs

-- ulift взять элемент и обернуть его в union, например из f Int получить Union f [Int, Bool, Char]
-- union = ulift (Identity True) :: Union Identity (Data.Union.Set '[Int, Bool, Char])
ulift :: (Typeable a, IsElem a as ~ True) => f a -> Union f as
ulift = MkUnion

-- umatch это наоборот, взять union и попробовать скастовать его в элемент, например из Union f [Int, Bool, Char] 
--    мы можем попробовать получить f Int, f Bool, или f Char. Результат будет Just, если там ожидаемый тип, и Nothing, 
--    если какой-то другой. Попытка достать что-то не из этого списка типов должна на этапе компиляции отлавливаться.
-- Union f [Int, Bool, Char] -> Just (f k ::Int) | Just (f k :: Bool) | Just (f k :: Char) | Nothing
umatch :: forall f a as. (Typeable a, IsElem a as ~ True) => Union f as -> Maybe (f a)
umatch (MkUnion (fx :: f x)) =
  (\Refl -> fx) <$> testEquality (typeRep @a) (typeRep @x)

-- добавление новых элементов
urelax :: IsSubset xs ys ~ 'True => Union f xs -> Union f ys
urelax = unsafeCoerce

-- сужение множества. Maybe появляется из-за того, что может в итоге не получиться Union – убрали элемент a из него 
-- => не выполняется IsElem
urestrict :: forall f xs ys. (KnownTSet xs, IsSubset xs ys ~ 'True) => Union f ys -> Maybe (Union f xs)
urestrict union@(MkUnion (fx :: f x)) = 
    if (S.member (SomeTypeRep (typeRep @x)) (knownTSet @_ @xs)) 
    then Just $ unsafeCoerce union 
    else Nothing

class KnownTSet (xs :: TSet k) where
  knownTSet :: S.Set SomeTypeRep

instance KnownTSet (MkTSet '[]) where
  knownTSet = S.empty

instance (KnownTSet (MkTSet xs), Typeable x) => KnownTSet (MkTSet (x:xs)) where
  knownTSet = S.insert (SomeTypeRep (typeRep @x)) (knownTSet @_ @(MkTSet xs))