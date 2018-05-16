{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, StandaloneDeriving, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes #-}

import GHC.TypeLits
import Data.Type.Set hiding (Union)
import Data.Kind (Type)
import Data.Type.Equality
import Data.Type.Bool
import Data.Typeable (Typeable, typeOf)
import Data.Functor.Identity (Identity(..))
import Data.Union (Set, Union (MkUnion), ulift, umatch, urelax, urestrict)

a :: Union Identity (Data.Union.Set '[Int, Bool, Char])
a = MkUnion (Identity True)

f :: Union Identity (Data.Union.Set '[Bool, Char, Int]) -> ()
f _ = ()

x = f a

-- foo :: Set '[Char, Bool]
foo = asSet $ Ext 'x' (Ext True Empty)

test :: Proxy (Nub (Sort '[Int, Bool, Char, Bool, Int])) -> Proxy (Nub (Sort '[Int, Bool, Char]))
test = id

-- union = ulift (Identity True) :: Union Identity (Data.Union.Set '[Int, Bool, Char])

-- umatch union :: Maybe (Identity Bool)
-- Just (Identity True)

-- umatch union :: Maybe (Identity Char)
-- Nothing

-- p = urelax union :: Union Identity (Data.Union.Set '[Int, Bool, Char, Integer])

-- :t p
-- p :: Union
--        Identity ('Data.Union.MkTSet '[Bool, Char, Int, Integer])

-- urelax union :: Union Identity (Data.Union.Set '[Int, Bool])

-- Couldn't match type ‘'False’ with ‘'True’
--         arising from a use of ‘urelax’

-- isJust $ (urestrict union :: Maybe (Union Identity (Data.Union.Set '[Int])))
-- False
-- isJust $ (urestrict union :: Maybe (Union Identity (Data.Union.Set '[Int, Bool])))
-- True


main :: IO ()
main = do
    putStrLn "do"