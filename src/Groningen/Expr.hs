
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -Wall                  #-}

module Groningen.Expr
  (
  -- Value
    Val(..)
  -- Expression
  , Exp(..)
  -- Program
  , Com(..)
  , Groningen
  , MonadGroningen
  , GroningenState(..)
    , regCount, regCommands -- lens
  , initialState
  , fix
  , delay
  , runGroningen
  ) where

import           Control.Arrow          (second)
import           Control.Lens           (makeLenses, use, view)
import           Control.Lens.Operators
import           Control.Monad.State    (State, runState)
import           Data.Function (on)
import           Data.String            (IsString(..))
import           Data.Typeable          (Typeable)

import           Groningen.Environment  (Environment)
import qualified Groningen.Environment  as Env
import           Groningen.Type

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

data Val (ty :: Type) where
  VInt    :: { unVInt    :: Integer  } -> Val 'TyInt
  VBool   :: { unVBool   :: Bool     } -> Val 'TyBool
  VString :: { unVString :: String   } -> Val 'TyString
  VArray  :: { unVArray  :: [Val ty] } -> Val ('TyArray ty)
deriving instance Show (Val ty)
instance Eq (Val 'TyInt)    where (==) = (==) `on` unVInt
instance Eq (Val 'TyBool)   where (==) = (==) `on` unVBool
instance Eq (Val 'TyString) where (==) = (==) `on` unVString
instance Eq (Val ty) => Eq (Val ('TyArray ty)) where
  (==) = (==) `on` unVArray

instance Num (Val 'TyInt) where
  VInt n + VInt m = VInt (n+m)
  VInt n - VInt m = VInt (n-m)
  VInt n * VInt m = VInt (n*m)
  abs (VInt n)    = VInt (abs n)
  signum (VInt n) = VInt (signum n)
  fromInteger     = VInt

instance IsString (Val 'TyString) where
  fromString = VString

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

data Exp (ty :: Type) where
  -- value
  Var    :: Env.VAR ty  -> Exp ty
  Int    :: Integer -> Exp 'TyInt
  Bool   :: Bool -> Exp 'TyBool
  String :: String -> Exp 'TyString
  -- Int
  Plus   :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Minus  :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Times  :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Mod    :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Abs    :: Exp 'TyInt -> Exp 'TyInt
  Sign   :: Exp 'TyInt -> Exp 'TyInt
  -- Boolean operation
  If     :: Exp 'TyBool -> Exp ty -> Exp ty -> Exp ty
  Eq     :: Eq (Val ty) => Exp ty -> Exp ty -> Exp 'TyBool -- これ多相にするのはまずい？
  -- Array
  Nil    :: Exp ('TyArray ty)
  Cons   :: Exp ty -> Exp ('TyArray ty) -> Exp ('TyArray ty)

  -- tekitou primitives
  Show    :: Exp 'TyInt -> Exp 'TyString
  LengthS :: Exp 'TyString -> Exp 'TyInt
  ConcatS :: Exp 'TyString -> Exp 'TyString -> Exp 'TyString
deriving instance Show (Exp ty)

instance Num (Exp 'TyInt) where
  (+) = Plus
  (-) = Minus
  (*) = Times
  abs = Abs
  signum = Sign
  fromInteger = Int

instance IsString (Exp 'TyString) where
  fromString = String

-------------------------------------------------------------------------------
-- Program
-------------------------------------------------------------------------------

-- クロックに同期して動くレジスタ
-- これの集まりをプログラムとしてみなす
data Com ty = Com
  { ini   :: Val ty -- ^ 初期値
  , recur :: Exp ty -- ^ 漸化式
  } deriving Show

-- プログラムを合成するモナド
type Groningen ty = MonadGroningen (Exp ty)
type MonadGroningen = State GroningenState
data GroningenState = GroningenState
  { _regCount    :: Env.Register
  , _regCommands :: Environment Com
  }
makeLenses ''GroningenState

initialState :: GroningenState
initialState = GroningenState
  { _regCount    = 0
  , _regCommands = Env.empty
  }

-- Operations on State
----------------------

getRegCount :: MonadGroningen Env.Register
getRegCount = use regCount

genNewReg :: MonadGroningen Env.Register
genNewReg = getRegCount <* incRegCount

genNewVar :: Typeable ty => MonadGroningen (Env.VAR ty)
genNewVar = Env.mkVar <$> genNewReg

incRegCount :: MonadGroningen ()
incRegCount = regCount %= succ

addCom :: Typeable ty => Env.VAR ty -> Com ty -> MonadGroningen ()
addCom reg com = regCommands %= Env.insert reg com

-- Construct Groningen
----------------------

fix :: Typeable ty => Val ty -> (Exp ty -> Groningen ty) -> Groningen ty
fix i f = do
  x  <- genNewVar
  a' <- f $ Var x
  addCom x Com { ini = i, recur = a' }
  return $ Var x

-- 自己参照せずレジスタを作る
delay :: Typeable ty => Val ty -> Exp ty -> Groningen ty
delay a a' = do
  x <- genNewVar
  addCom x Com { ini = a, recur = a' }
  return $ Var x

-- モナドを走らせてプログラムを作る
runGroningen :: Groningen ty -> (Exp ty, Environment Com)
runGroningen m = second (view regCommands) $ runState m initialState

