{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances #-}

module Data.Handler (Handler (..), handle_1, handle_ω) where

import Type.Reflection (TypeRep, Typeable, typeRep)
import Data.Rec
import Data.Union
import Data.Dependent.Map ((!))

newtype Handler r f x = Handler (f x -> r)

-- handle_1 это first-class encoding для доступа к элементу
handle_1 :: forall f xs r . Rec f xs -> Union (Handler r f) xs -> r
handle_1 (MkRec map) (MkUnion (Handler (func :: (f x -> r)))) = func $ map ! (typeRep @x)

-- rec аналог case, на каждый элемент мапы - тип - он имеет функцию 
handle_ω :: Union f xs -> Rec (Handler r f) xs -> r
handle_ω (MkUnion (fx :: f x)) (MkRec map) = let Handler func = map ! (typeRep @x) in func fx