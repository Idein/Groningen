
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# OPTIONS_GHC -Wall           #-}

module Groningen.Type
  ( Type(..)
  , IsType
  , typeOf
  , typeRepOf
  , hasSameTypeAs
  , (:~:)(..)
  ) where

import           Type.Reflection    (Typeable, TypeRep, typeRep)
import           Data.Type.Equality ((:~:)(..), testEquality)
import           Data.Extensible    (Assoc)
import           GHC.TypeLits       (Symbol)
import           Data.Proxy

data Type
  = TyUnit
  | TyInt
  | TyFloat
  | TyBool
  | TyString
  | TyList Type
  | TyDict
  | TyDict' [Assoc Symbol Type]

type IsType (ty :: Type) = Typeable ty

-- 名前がいかにもBooleanを返しそうなのは良くないか
hasSameTypeAs
    :: (IsType ty, IsType ty')
    => proxy ty
    -> proxy' ty'
    -> Maybe (ty :~: ty')
x `hasSameTypeAs` y = testEquality (typeRepOf x) (typeRepOf y)

typeOf :: forall ty proxy. IsType ty => proxy ty -> Type
typeOf p
    | Just Refl <- p `hasSameTypeAs` Proxy @'TyUnit   = TyUnit
    | Just Refl <- p `hasSameTypeAs` Proxy @'TyInt    = TyInt
    | Just Refl <- p `hasSameTypeAs` Proxy @'TyFloat  = TyFloat
    | Just Refl <- p `hasSameTypeAs` Proxy @'TyBool   = TyBool
    | Just Refl <- p `hasSameTypeAs` Proxy @'TyString = TyString
    | otherwise = undefined

typeRepOf :: IsType ty => proxy ty -> TypeRep ty
typeRepOf _ = typeRep


