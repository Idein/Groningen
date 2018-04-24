
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE StrictData     #-}
{-# OPTIONS_GHC -Wall       #-}

module Groningen.Environment
  ( Register
  , VAR
  , Environment
  , mkVar
  , empty
  , insert
  , delete
  , lookup
  , map
  -- decompose(不要？)
  --, Some(..)
  --, intoMap
  ) where

import           Data.Constraint    (Dict(..))
import qualified Data.IntMap        as M
import           Data.Type.Equality ((:~:)(..), testEquality)
import           Prelude            hiding (lookup, map)
import           Type.Reflection    (Typeable, TypeRep, typeRep)

import           Groningen.Type

type Register = Int
data VAR (ty :: Type) where
  VAR :: Typeable ty => Register -> VAR ty
instance Show (VAR ty) where show (VAR n)          = "VAR " ++ show n
instance Eq (VAR ty)   where VAR n == VAR m        = n == m
instance Ord (VAR ty)  where VAR n `compare` VAR m = n `compare` m

mkVar :: Typeable ty => Int -> VAR ty
mkVar = VAR

data Some (f :: Type -> *) where
  Some :: { _dict  :: Dict (Typeable ty)
          , _inner :: f ty
          } -> Some f

newtype Environment (f :: Type -> *) =
  Environment                       -- ^ 公開しない
    { _intoMap :: M.IntMap (Some f) -- ^ 公開して良い?
    }

empty :: Environment f
empty = Environment M.empty

insert :: VAR ty -> f ty -> Environment f -> Environment f
insert (VAR k) v (Environment m) = Environment $ M.insert k (Some Dict v) m

delete :: VAR ty -> Environment f -> Environment f
delete (VAR k) (Environment m) = Environment $ M.delete k m

lookup :: forall ty f. VAR ty -> Environment f -> Maybe (f ty)
lookup var@(VAR k) (Environment m) = case M.lookup k m of
  Nothing -> Nothing
  Just (Some Dict x) ->
    case testEquality (typeOf var) (typeOf x) of
      Just Refl -> Just x
      Nothing   -> Nothing
  where
    typeOf :: Typeable (ty :: Type) => proxy ty -> TypeRep ty
    typeOf _ = typeRep

map :: forall f g. (forall ty. Typeable ty => f ty -> g ty) -> Environment f -> Environment g
map f (Environment m) = Environment $ M.map f' m
  where f' (Some d@Dict x) = Some d (f x)

-- TODO updateとかalterとかを実装する

