{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, StandaloneDeriving, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables #-}

import GHC.TypeLits
import Data.Type.Set hiding (Union)
import Data.Kind (Type)
import Data.Type.Equality
import Data.Type.Bool
import Type.Reflection (Typeable, typeOf, typeRep)
import Data.Functor.Identity (Identity(..))
import Data.Union (Union (MkUnion), ulift, umatch, urelax, urestrict)
import Data.ImprovedSet (Set, IsSubset, IsSubsetForLists)
import Data.Rec
import Data.Dependent.Map (DMap, fromList, DSum ((:=>)), (!))
import Data.Handler

a :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])
a = MkUnion (Identity True)

f :: Union Identity (Data.ImprovedSet.Set '[Bool, Char, Int]) -> ()
f _ = ()

x = f a

t1 :: Proxy (Data.ImprovedSet.Set '[Int, Bool, Char])
t1 = Proxy :: Proxy (Data.ImprovedSet.Set '[Int, Bool, Char])

t2 :: Proxy (Data.ImprovedSet.Set '[Int, Bool, Char]) -> ()
t2 _ = ()

t3 = t2 t1

-- test :: Proxy (CmpTypeNonDet Int Bool) -> Proxy ('GT)
-- test = id

-- foo :: Set '[Char, Bool]
-- foo = asSet $ Ext 'x' (Ext True Empty)

-- test :: Proxy (GHC.TypeLits.MySet '[Int, Bool, Char]) -> Proxy (GHC.TypeLits.MySet '[Int, Bool, Char])
-- test = id

-- test1 :: Proxy (GHC.TypeLits.MySet ('[] :: [*])) -> Proxy (GHC.TypeLits.MySet ('[] :: [*]))
-- test1 = id

-- test2 :: Proxy (GHC.TypeLits.MySet '[4, 5]) -> Proxy (GHC.TypeLits.MySet '[5, 4])
-- test2 = id

-- test :: (GHC.TypeLits.MySet '[Int, Bool, Char] ~ as, GHC.TypeLits.MySet '[Int, Bool, Char] ~ bs) => Proxy (as) -> Proxy (bs)
-- test = id

-- test :: Proxy (Nub (Sort '[Int, Bool, Char, Bool, Int])) -> Proxy (Nub (Sort '[Int, Bool, Char]))
-- test = id

-- union = ulift (Identity True) :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])

-- umatch union :: Maybe (Identity Bool)
-- Just (Identity True)

-- umatch union :: Maybe (Identity Char)
-- Nothing

-- p = urelax union :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char, Integer])

-- :t p
-- p :: Union
--        Identity ('Data.Union.MkTSet '[Bool, Char, Int, Integer])

-- urelax union :: Union Identity (Data.ImprovedSet.Set '[Int, Bool])

-- Couldn't match type ‘'False’ with ‘'True’
--         arising from a use of ‘urelax’

-- isJust $ (urestrict union :: Maybe (Union Identity (Data.ImprovedSet.Set '[Int])))
-- False
-- isJust $ (urestrict union :: Maybe (Union Identity (Data.ImprovedSet.Set '[Int, Bool])))
-- True

-- data MyABC f = ABC (f Int) (f Char) (f Bool)
-- Rec f '[Int, Char, Bool]

-- testHandleW :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char]) -> String
-- testHandleW x = handle_ω x handlers
--   where
--     handlers = onInt `rmerge` onBool `rmerge` onChar
--     onInt = rsingleton $ Handler $ \i -> let Identity (i' :: Int) = i in show (i' + 1)
--     onBool = rsingleton $ Handler $ \b -> let Identity b' = b in show (not b')
--     onChar = rsingleton $ Handler $ \c -> let Identity c' = c in [c']

-- *Main> union = ulift (Identity True) :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])
-- *Main> testHandleW union 
-- "False"
-- *Main> union = ulift (Identity (5 :: Int)) :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])
-- *Main> testHandleW union 
-- "6"
-- *Main> union = ulift (Identity 'z') :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])
-- *Main> testHandleW union 
-- "z"

type family ProxiesAscN (n :: Nat) :: [*] where 
    ProxiesAscN k = ProxyAscNHelper k 1

type family ProxiesDescN (n :: Nat) :: [*] where 
    ProxiesDescN 0 = '[]
    ProxiesDescN k = (Proxy k) ': (ProxiesDescN (k - 1))
    
type family ProxyAscNHelper (n :: Nat) (m :: Nat) :: [*] where
    ProxyAscNHelper 0 k = '[]
    ProxyAscNHelper n k = (Proxy k) ': (ProxyAscNHelper (n - 1) (k + 1))

-- test5 :: Proxy (Data.ImprovedSet.Set (ProxiesAscN 16)) -> Proxy (Data.ImprovedSet.Set (ProxiesDescN 16))
-- test5 = id

-- test :: (GHC.TypeLits.MySet (ProxiesAscN 14) ~ as, GHC.TypeLits.MySet (ProxiesDescN 14) ~ bs) => Proxy (as) -> Proxy (bs)
-- test = id

-- test5 :: Proxy (Sort' (ProxiesAscN 16)) -> Proxy (Sort' (ProxiesDescN 16))
-- test5 = id

-- test5 :: Proxy (GHC.TypeLits.MySet (ProxiesAscN 20)) -> Proxy (GHC.TypeLits.MySet (ProxiesDescN 20))
-- test5 = id

type family EqSet (xs :: [k]) (ys :: [k]) :: Bool where
    EqSet '[] '[] = 'True
    EqSet xs '[] = 'False
    EqSet '[] xs = 'False
    EqSet (x ': xs) (x ': ys) = EqSet xs ys
    EqSet (x ': xs) (y ': ys) = 'False

type family EqEq (xs :: [k]) (ys :: [k]) :: Bool where
    EqEq xs ys = EqInternal xs ys xs ys

type family EqInternal (xs :: [k]) (ys :: [k]) (cxs :: [k]) (cys :: [k]) :: Bool where
    EqInternal '[] '[] cxs cys = 'True
    EqInternal '[] ys cxs cys = 'False
    EqInternal xs '[] cxs cys = False
    EqInternal (x ': xs) (y ': ys) cxs cys = (CheckElemForLists x cxs cys) && (CheckElemForLists y cxs cys) && (EqInternal xs ys cxs cys)

type family CheckElemForLists (x :: k) (xs :: [k]) (ys :: [k]) :: Bool where
    CheckElemForLists x '[] '[] = 'True
    CheckElemForLists x '[] ys = 'False
    CheckElemForLists x xs '[] = 'False
    CheckElemForLists x xs ys = (Count x xs) == (Count x ys)

type family Count (x :: k) (xs :: [k]) :: Nat where
    Count x '[] = 0
    Count x (x : xs) = 1 + Count x xs
    Count x (y : xs) = Count x xs

-- type family Sort' (xs :: [Type]) :: [Type] where 
--     Sort' ('[]) = '[]
--     Sort' (x ': '[]) = '[x]
--     Sort' (x : xs) = Sort (LessElems x xs) ++ (x ': Sort (GreaterOrEqElems x xs))

-- type family LessElems (x :: Type) (xs :: [Type]) :: [Type] where
--     LessElems x '[] = '[]
--     LessElems x (y : ys) = If (Less (CmpTypeNonDet y x)) (y : LessElems x ys) (LessElems x ys)

-- type family GreaterOrEqElems (x :: Type) (xs :: [Type]) :: [Type] where
--     GreaterOrEqElems x '[] = '[]    
--     GreaterOrEqElems x (y : ys) = If (Less (CmpTypeNonDet y x)) (GreaterOrEqElems x ys) (y : GreaterOrEqElems x ys)

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

-- type family Less (o :: Ordering) :: Bool where
--     Less 'LT = 'True
--     Less _ = 'False

-- test5 :: Proxy (IsSubset (Data.ImprovedSet.Set (ProxiesAscN 20)) (Data.ImprovedSet.Set ((ProxiesAscN 10) ++ (ProxiesAscN 100)))) -> Proxy ('True)
-- test5 = id

-- test5 :: Proxy (IsSubset (Data.ImprovedSet.Set '[1, 3]) (Data.ImprovedSet.Set '[1, 2, 3])) -> Proxy ('True)
-- test5 = id

-- test :: Proxy (EqEq ((ProxiesAscN 10) ++ (ProxiesAscN 10)) ((ProxiesAscN 20))) -> Proxy ('True)
-- test = id

-- test :: Proxy (EqSet (GHC.TypeLits.MySet (ProxiesAscN 100)) (GHC.TypeLits.MySet (ProxiesDescN 100))) -> Proxy ('True)
-- test = id

main :: IO ()
main = do
    putStrLn "do"