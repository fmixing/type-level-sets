{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, EmptyDataDecls #-}

module Data.Union (Union, Union' (MkUnion), ulift, umatch) where

import GHC.TypeLits
import Data.Type.Set hiding (Union)
import Data.Kind (Type)
import Data.Type.Equality
import Data.Type.Bool
import Data.Typeable (Typeable, typeOf)
import Data.Functor.Identity (Identity(..))

type instance Cmp (n :: k) (m :: k) = BetterCmpType n m

data T (a :: k)

type family BetterCmpType (a :: k1) (b :: k2) :: Ordering where
  BetterCmpType a b = CmpTypeNonDet (T a) (T b)

type family IsElem (x :: k) (xs :: [k]) :: Bool where
  IsElem x '[] = 'False
  IsElem y (x : xs) = (y == x) || IsElem y xs

data Union' f xs where
  MkUnion :: (Typeable a, IsElem a xs ~ True) => f a -> Union' f xs

type Union f xs = Union' f (Nub (Sort xs))

-- 1. ulift взять элемент и обернуть его в union, например из f Int получить Union f [Int, Bool, Char]
-- 2. umatch это наоборот, взять union и попробовать скастовать его в элемент, например из Union f [Int, Bool, Char] 
--    мы можем попробовать получить f Int, f Bool, или f Char. Результат будет Just, если там ожидаемый тип, и Nothing, 
--    если какой-то другой. Попытка достать что-то не из этого списка типов должна на этапе компиляции отлавливаться.
-- 3. urestrict это попробовать скастовать union с некоторыми элементами в union с подмножеством этих элементов, 
--    такое обобщение umatch. Например, можем из Union f '[Int, Bool, Char] попробовать получить Union f '[Bool, Char].
-- 4. urelax наоборот, добавление элементов в список возможных

-- openUnion # (5 :: Int) это аналог ulift (Identity (5 :: Int)) :: Union Identity '[Bool, Int]
-- a ^? openUnion :: Maybe Int это аналог umatch a :: Maybe (Identity Int)

ulift :: (Typeable a, IsElem a (Nub (Sort as)) ~ True) => f a -> Union f as
ulift = MkUnion

-- Union f [Int, Bool, Char] -> Just (f k ::Int) | Just (f k :: Bool) | Just (f k :: Char) | Nothing
umatch :: (Typeable a, IsElem a as ~ True) => Union f as -> Maybe (f a)
umatch (MkUnion elem) = Nothing
    -- if a == (typeOf f_) then Just elem else Nothing