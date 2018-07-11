
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# OPTIONS_GHC -Wall           #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Groningen.Type
  ( Type(..)
  , IsType
  , Ty(..)
  , typeOf
  , hasSameTypeAs
  , (:~:)(..)
  ) where

import           Type.Reflection          (Typeable, TypeRep, typeRep)
import           Data.Type.Equality       ((:~:)(..), testEquality)
import           Data.Extensible
import           GHC.TypeLits
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Singletons.Prelude  (Sing(..))

data Type
  = TyUnit
  | TyInt
  | TyFloat
  | TyBool
  | TyString
  | TyList Type
  | TyDict
  | TyDict' [Assoc Symbol Type]

-------------------------------------------------------------------------------
-- Singleton
-------------------------------------------------------------------------------

-- | Representation of 'Type'

-- TODO: 名前
data Ty
  = TUnit
  | TInt
  | TFloat
  | TBool
  | TString
  | TList Ty
  | TDict
  | TDict' [TyAssoc]
  deriving (Show, Eq)

data TyAssoc = TyAssoc String Ty
  deriving (Show, Eq)

data instance Sing (kv :: Assoc Symbol Type) where
  (:>-) :: Sing (k :: Symbol) -> Sing (ty :: Type) -> Sing (k >: ty)
instance (KnownSymbol k, SingI (ty :: Type)) => SingI (k >: ty) where
  sing = sing :>- sing

data instance Sing (ty :: Type) where
  SUnit      :: Sing 'TyUnit
  SInt       :: Sing 'TyInt
  SFloat     :: Sing 'TyFloat
  SBool      :: Sing 'TyBool
  SString    :: Sing 'TyString
  SList      :: Sing ty -> Sing ('TyList ty)
  SDict      :: Sing 'TyDict
  SDict'     :: Sing xs -> Sing ('TyDict' xs)

instance SingI 'TyUnit   where sing = SUnit
instance SingI 'TyInt    where sing = SInt
instance SingI 'TyFloat  where sing = SFloat
instance SingI 'TyBool   where sing = SBool
instance SingI 'TyString where sing = SString
instance SingI 'TyDict   where sing = SDict
instance SingI ty => SingI ('TyList ty)  where sing = SList sing
instance SingI xs => SingI ('TyDict' xs) where sing = SDict' sing

instance SingKind (Assoc Symbol Type) where
  type Demote (Assoc Symbol Type) = TyAssoc
  fromSing (s@SSym :>- sty) = TyAssoc (symbolVal s) (fromSing sty)
  toSing (TyAssoc key ty) = case (someSymbolVal key, toSing ty) of
    (SomeSymbol p, SomeSing s) -> SomeSing (singByProxy p :>- s)

instance SingKind Type where
  type Demote Type = Ty
  fromSing :: Sing (ty :: Type) -> Ty
  fromSing = \case
    SUnit    -> TUnit
    SInt     -> TInt
    SFloat   -> TFloat
    SBool    -> TBool
    SString  -> TString
    SDict    -> TDict
    SList s  -> TList (fromSing s)
    SDict' s -> TDict' (fromSing s)
  toSing :: Ty -> SomeSing Type
  toSing = \case
    TUnit     -> SomeSing SUnit
    TInt      -> SomeSing SInt
    TFloat    -> SomeSing SFloat
    TBool     -> SomeSing SBool
    TString   -> SomeSing SString
    TDict     -> SomeSing SDict
    TList ty  -> case toSing ty of SomeSing s -> SomeSing (SList s)
    TDict' xs -> case toSing xs of SomeSing s -> SomeSing (SDict' s)

deriving instance Show (Sing (ty :: Type))
deriving instance Show (Sing (kv :: Assoc Symbol Type))
deriving instance Show (Sing (xs :: [Assoc Symbol Type]))

-------------------------------------------------------------------------------

type IsType (ty :: Type) = (Typeable ty, SingI ty)

-- 名前がいかにもBooleanを返しそうなのは良くないかも
hasSameTypeAs
    :: (IsType ty, IsType ty')
    => proxy ty
    -> proxy' ty'
    -> Maybe (ty :~: ty')
x `hasSameTypeAs` y = testEquality (typeRepOf x) (typeRepOf y)
  where
    typeRepOf :: IsType ty => proxy ty -> TypeRep ty
    typeRepOf _ = typeRep

typeOf :: forall ty proxy. IsType ty => proxy ty -> Ty
typeOf = fromSing . singByProxy

