{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, StandaloneDeriving, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, TypeApplications #-}

import GHC.TypeLits
import Data.Type.Set hiding (Union, union)
import Type.Reflection (typeRep)
import Data.Functor.Identity (Identity(..))
import Data.Union (Union (MkUnion), ulift, umatch, urelax, urestrict)
import Data.ImprovedSet (Set, IsSubset, IsSubsetForLists)
import Data.Rec
import Data.Dependent.Map (DMap, fromList, DSum ((:=>)), (!))
import Data.Handler
import Data.Maybe (isJust)

import Test.Hspec

-- компилировать с cabal new-build test:exercises-test
-- запускать с cabal new-test --enable-tests

-- Проверка, что множество не опирается на начальный порядок
a :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])
a = MkUnion (Identity True)

f :: Union Identity (Data.ImprovedSet.Set '[Bool, Char, Int]) -> ()
f _ = ()

x = f a

-- Подумать, как тестить?

foo = asSet $ Ext 'x' (Ext True Empty)

-- Проверка, что сортируется
testSet :: Proxy (GHC.TypeLits.MySet (ProxiesAscN 100)) -> Proxy (GHC.TypeLits.MySet (ProxiesDescN 100))
testSet = id

-- Проверка, что работает для пустого
testEmpty :: Proxy (GHC.TypeLits.MySet ('[] :: [*])) -> Proxy (GHC.TypeLits.MySet ('[] :: [*]))
testEmpty = id

-- Проверка, что работает для типов с рандомными кайндами
test2 :: Proxy (GHC.TypeLits.MySet '[4, 5]) -> Proxy (GHC.TypeLits.MySet '[5, 4])
test2 = id

-- union
union = ulift (Identity True) :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char])

-- проверка, что urelax расширяет множество
p = urelax union :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char, Integer])

testUrelax :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char, Integer])
testUrelax = p

-- urelax union :: Union Identity (Data.ImprovedSet.Set '[Int, Bool])
-- Couldn't match type ‘'False’ with ‘'True’
--         arising from a use of ‘urelax’

-- rec
s1 = rsingleton $ Identity (5 :: Int)
s2 = rsingleton $ Identity (True :: Bool)

testRec :: Rec Identity (Data.ImprovedSet.Set '[Int])
testRec = s1

merged = rmerge s1 s2

testMerge :: Rec Identity (Data.ImprovedSet.Set '[Int, Bool])
testMerge = merged

casted = rcast merged :: Rec Identity (Data.ImprovedSet.Set '[Bool])

testCast :: Rec Identity (Data.ImprovedSet.Set '[Bool])
testCast = casted

-- Проверка, что Union и Rec могут работать вместе
testHandleW :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char]) -> String
testHandleW x = handle_ω x handlers
  where
    handlers = onInt `rmerge` onBool `rmerge` onChar
    onInt = rsingleton $ Handler $ \i -> let Identity (i' :: Int) = i in show (i' + 1)
    onBool = rsingleton $ Handler $ \b -> let Identity b' = b in show (not b')
    onChar = rsingleton $ Handler $ \c -> let Identity c' = c in [c']

type family ProxiesAscN (n :: Nat) :: [*] where 
    ProxiesAscN k = ProxyAscNHelper k 1

type family ProxiesDescN (n :: Nat) :: [*] where 
    ProxiesDescN 0 = '[]
    ProxiesDescN k = (Proxy k) ': (ProxiesDescN (k - 1))
    
type family ProxyAscNHelper (n :: Nat) (m :: Nat) :: [*] where
    ProxyAscNHelper 0 k = '[]
    ProxyAscNHelper n k = (Proxy k) ': (ProxyAscNHelper (n - 1) (k + 1))

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

-- проверка, что работает 1. ++ 2. subset для большого числа элементов
testSubset1 :: Proxy (IsSubset (Data.ImprovedSet.Set (ProxiesAscN 20)) (Data.ImprovedSet.Set ((ProxiesAscN 10) ++ (ProxiesAscN 100)))) -> Proxy ('True)
testSubset1 = id

-- проверка, что при непоследовательном сабсете тоже работает
testSubset2 :: Proxy (IsSubset (Data.ImprovedSet.Set '[1, 3]) (Data.ImprovedSet.Set '[1, 2, 3])) -> Proxy ('True)
testSubset2 = id

testSubset3 :: Proxy (IsSubset (Data.ImprovedSet.Set '[1, 10]) (Data.ImprovedSet.Set '[1, 3, 10, 100])) -> Proxy ('True)
testSubset3 = id

main :: IO ()
main = hspec $ do
  describe "Union" $ do
    it "umatch" $ do
        (umatch union :: Maybe (Identity Bool)) `shouldBe` Just (Identity True)
        (umatch union :: Maybe (Identity Char)) `shouldBe` Nothing
    it "urestrict" $ do
        (isJust $ (urestrict union :: Maybe (Union Identity (Data.ImprovedSet.Set '[Int])))) `shouldBe` False
        (isJust $ (urestrict union :: Maybe (Union Identity (Data.ImprovedSet.Set '[Int, Bool])))) `shouldBe` True
  describe "Rec" $ do
    it "rsingleton && rget" $
        (rget s1 :: Identity Int) `shouldBe` Identity 5
    it "rmerge" $ do
        let MkRec w = merged
        (w ! (typeRep @Int)) `shouldBe` (Identity 5)
        (w ! (typeRep @Bool)) `shouldBe` Identity True
    it "rput" $ do
        let put = rput (Identity (4 :: Int)) s1
        (rget put :: Identity Int) `shouldBe` Identity 4
    it "cast" $ do
        (rget casted :: Identity Bool) `shouldBe` Identity True
        let newMerged = rmerge casted (rsingleton (Identity (1 :: Int)))
        (rget newMerged :: Identity Int) `shouldBe` Identity 1
    it "rreplace" $ do
        let replaced = rreplace (rsingleton $ Identity (4 :: Int)) merged
        (rget replaced :: Identity Int) `shouldBe` Identity 4
        (rget replaced :: Identity Bool) `shouldBe` Identity True
  describe "Handle" $
    it "handleW" $ do
        (testHandleW (ulift (Identity True) :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char]))) `shouldBe` "False"
        (testHandleW (ulift (Identity (5 :: Int)) :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char]))) `shouldBe` "6"
        (testHandleW (ulift (Identity 'z') :: Union Identity (Data.ImprovedSet.Set '[Int, Bool, Char]))) `shouldBe` "z" 