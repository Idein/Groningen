
module Groningen.Environment
  ( Register
  , VAR(..)
  , Environment(..)
  , Some(..)
  , ShowTy(..)
  , mkVar
  , freshVar
  , empty
  , member
  , size
  , insert
  , delete
  , lookup
  , lookupE
  , map
  ) where

import           Prelude              hiding (lookup, map)
import qualified Data.IntMap          as M
import           Data.Functor.Classes

import           Groningen.Type

-------------------------------------------------------------------------------
-- VAR
-------------------------------------------------------------------------------

type Register = Int
data VAR (ty :: Type) where
  VAR :: IsType ty => Register -> VAR ty
instance Show (VAR ty) where showsPrec d (VAR n)   = showsUnaryWith showsPrec "VAR " d n
instance ShowTy VAR    where showTy v              = show v
instance Eq (VAR ty)   where VAR n == VAR m        = n == m
instance Ord (VAR ty)  where VAR n `compare` VAR m = n `compare` m

mkVar :: IsType ty => Int -> VAR ty
mkVar = VAR

freshVar :: IsType ty => Environment f -> VAR ty
freshVar (Environment m)
  | null (M.keys m) = VAR 0
  | otherwise = VAR $ maximum (M.keys m) + 1

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

data Some (f :: Type -> *) where
  Some :: IsType ty => f ty -> Some f

newtype Environment (f :: Type -> *) =
  Environment                       -- 公開しない
    { _intoMap :: M.IntMap (Some f) -- 公開して良い?
    }

class ShowTy (f :: Type -> *) where
  {-# MINIMAL showTy #-}
  showTy :: f ty -> String
  showsPrecTy :: ShowTy f => Int -> f ty -> ShowS
  showsPrecTy _ x s = showTy x ++ s

instance ShowTy f => Show (Some f) where
  show (Some x) = "Some (" ++ showTy x ++ ")"

instance ShowTy f => Show (Environment f) where
  show (Environment x) = "Environment " ++ show (M.toList x)

-------------------------------------------------------------------------------

empty :: Environment f
empty = Environment M.empty

size :: Environment f -> Int
size (Environment m) = M.size m

member :: VAR ty -> Environment f -> Bool
member (VAR k) (Environment m) = M.member k m

insert :: VAR ty -> f ty -> Environment f -> Environment f
insert (VAR k) v (Environment m) = Environment $ M.insert k (Some v) m

delete :: VAR ty -> Environment f -> Environment f
delete (VAR k) (Environment m) = Environment $ M.delete k m

lookup :: forall ty f. VAR ty -> Environment f -> Maybe (f ty)
lookup var env = either (const Nothing) Just $ lookupE var env

lookupE :: forall ty f. VAR ty -> Environment f -> Either String (f ty)
lookupE var@(VAR k) (Environment m) = case M.lookup k m of
  Nothing -> Left "lookup: unbounded variable"
  Just (Some x) ->
    -- ここはunsafeCoerceで良いのでは
    -- 変なことしない限り環境が狂うことはないはず
    case x `hasSameTypeAs` var of
      Just Refl -> Right x
      Nothing   -> Left "lookup: type mismatch (bug)"

map :: forall f g. (forall ty. IsType ty => f ty -> g ty) -> Environment f -> Environment g
map f (Environment m) = Environment $ M.map f' m
  where f' (Some x) = Some (f x)

