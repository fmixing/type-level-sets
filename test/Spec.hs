{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, StandaloneDeriving, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes #-}

import GHC.TypeLits
import Data.Type.Set hiding (Union)
import Data.Kind (Type)
import Data.Type.Equality
import Data.Type.Bool
import Data.Typeable (Typeable, typeOf)
import Data.Functor.Identity (Identity(..))
import Data.Union (Union, Union'( MkUnion ))

a :: Union Identity '[Int, Bool, Char]
a = MkUnion (Identity True)

f :: Union Identity '[Bool, Char, Int] -> ()
f _ = ()

x = f a

-- foo :: Set '[Char, Bool]
foo = asSet $ Ext 'x' (Ext True Empty)

test :: Proxy (Nub (Sort '[Int, Bool, Char, Bool, Int])) -> Proxy (Nub (Sort '[Int, Bool, Char]))
test = id

main :: IO ()
main = do
    putStrLn "do"