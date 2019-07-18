{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effpee.Validation where

import Control.Applicative
import Data.Eq             (Eq (..))
import Data.Functor
import Data.Monoid         (Monoid, mempty, mappend)
import Data.Semigroup      (Semigroup, (<>))
import Effpee
import GHC.Show

-- >>> Error "a string"
-- Error "a string"
-- it :: IsString e => Validation e a
data Validation e a = Error e | Valid a deriving (Functor, Show, Eq)

-- MONOMORPHIC

-- >>> Valid "apple" `orV` Valid "banana" == Valid "apple"
-- True
-- it :: ghc-prim-0.5.2.0:GHC.Types.Bool
-- >>> e0 = "error message"
-- e0 :: IsString p => p
-- >>> e1 = "second error"
-- e1 :: IsString p => p
-- >>> Error e0 `orV` Error e1 == Error (e0 <> e1)
-- True
-- it :: ghc-prim-0.5.2.0:GHC.Types.Bool
orV :: Semigroup e => Validation e a -> Validation e a -> Validation e a
orV = todo "Effpee.Validation#orV"

orVList :: Validation e a -> Validation e a -> Validation [e] a
orVList = todo "Effpee.Validation#orVList"

-- TYPECLASS POLYMORPHIC WAY
-- it requires more lines of code but we can use all the Functor, Applicative, and
-- Alternative methods on our `Validation e a` values.
instance Semigroup e => Applicative (Validation e) where
  pure :: a -> Validation e a
  pure something = Valid something

  (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  (<*>) (Error e) _ = Error e
  (<*>) (Valid f) (Valid thing) = Valid (f thing)
  (<*>) (Valid f) (Error e) = Error e

instance Monoid e => Alternative (Validation e) where
  empty :: Validation e a
  empty = Error mempty

  (<|>) :: Validation e a -> Validation e a -> Validation e a
  (<|>) (Error e1) (Error e2) = Error (mappend e1 e2)
  (<|>) (Valid v1) (Valid v2) = Valid v1
  (<|>) (Error e1) _ = Error e1
  (<|>) _ (Error e1) = Error e1
