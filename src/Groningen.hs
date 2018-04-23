
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -Wall                  #-}

module Groningen
  (
  -- Type
    Type(..)
  -- Value
  , Val
  , IntVal
  , StringVal
  , SomeVal
  -- Expression
  , Register
  , VAR
  , Exp(..)
  , IntExp
  , StringExp
  , SomeExp
  -- Construct Expression
  , Com
  , Groningen
  , MonadGroningen
  , GroningenState -- コンストラクタは隠す
  , fix
  , delay
  , runGroningen
  -- Evaluation
  , eval
  ) where

import           Control.Arrow          (second)
import           Control.Lens           (makeLenses, use, view)
import           Control.Lens.Operators
import           Control.Monad.State    (State, runState)
import           Data.Map               (Map, (!))
import qualified Data.Map               as M
import           Data.String            (IsString(..))

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

----------------------------------------
-- Type
----------------------------------------

data Type
  = TyInt
  | TyString

----------------------------------------
-- Value
----------------------------------------

data Val (ty :: Type) where
  VInt    :: Integer -> Val 'TyInt
  VString :: String -> Val 'TyString
deriving instance Eq   (Val ty)
deriving instance Ord  (Val ty)
deriving instance Show (Val ty)

type IntVal = Val 'TyInt
instance Num IntVal where
  VInt n + VInt m = VInt (n+m)
  VInt n - VInt m = VInt (n-m)
  VInt n * VInt m = VInt (n*m)
  abs (VInt n) = VInt (abs n)
  signum (VInt n) = VInt (signum n)
  fromInteger = VInt

type StringVal = Val 'TyString
instance IsString StringVal where
  fromString = VString

data SomeVal where
  SomeVal :: Val ty -> SomeVal
deriving instance Show SomeVal

----------------------------------------
-- Expression
----------------------------------------

type Register = Int
data VAR (ty :: Type) = VAR Register
  deriving (Show, Eq, Ord)

data Exp (ty :: Type) where
  Var    :: VAR ty  -> Exp ty
  Int    :: Integer -> Exp 'TyInt
  String :: String -> Exp 'TyString
  Plus   :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Minus  :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Times  :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Abs    :: Exp 'TyInt -> Exp 'TyInt
  Sign   :: Exp 'TyInt -> Exp 'TyInt

  -- tekitou primitives
  Show    :: Exp 'TyInt -> Exp 'TyString
  LengthS :: Exp 'TyString -> Exp 'TyInt
  ConcatS :: Exp 'TyString -> Exp 'TyString -> Exp 'TyString
deriving instance Show (Exp ty)

type IntExp = Exp 'TyInt
instance Num IntExp where
  (+) = Plus
  (-) = Minus
  (*) = Times
  abs = Abs
  signum = Sign
  fromInteger = Int

type StringExp = Exp 'TyString
instance IsString StringExp where
  fromString = String

data SomeExp where
  SomeExp :: Exp ty -> SomeExp
deriving instance Show SomeExp

----------------------------------------
-- Program
----------------------------------------

-- クロックに同期して動くレジスタ
-- これの集まりをプログラムとしてみなす
data Com = Com
  { ini   :: SomeVal -- ^ 初期値
  , recur :: SomeExp -- ^ 漸化式
  } deriving Show

-- プログラムを合成するモナド
type Groningen ty = MonadGroningen (Exp ty)
type MonadGroningen = State GroningenState
data GroningenState = GroningenState
  { _regCount    :: Register
  , _regCommands :: Map Register Com
  }
makeLenses ''GroningenState

initialState :: GroningenState
initialState = GroningenState
  { _regCount    = 0
  , _regCommands = M.empty
  }

-- Operations on State
----------------------

getRegCount :: MonadGroningen Register
getRegCount = use regCount

genNewReg :: MonadGroningen Register
genNewReg = getRegCount <* incRegCount

incRegCount :: MonadGroningen ()
incRegCount = regCount %= succ

addCom :: Register -> Com -> MonadGroningen ()
addCom reg com = regCommands %= M.insert reg com

-- Construct Groningen
----------------------

fix :: Val ty -> (Exp ty -> Groningen ty) -> Groningen ty
fix i f = do
  x  <- genNewReg
  a' <- f $ Var $ VAR x
  addCom x Com { ini = SomeVal i, recur = SomeExp a' }
  return $ Var (VAR x)

-- 自己参照せずレジスタを作る
delay :: Val ty -> Exp ty -> Groningen ty
delay a a' = do
  x <- genNewReg
  addCom x Com { ini = SomeVal a, recur = SomeExp a' }
  return $ Var $ VAR x

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

-- モナドを走らせてプログラムを作る
runGroningen :: Groningen ty -> (Exp ty, Map Register Com)
runGroningen m = second (view regCommands) $ runState m initialState

evalInt :: Map Register SomeVal -> IntExp -> Integer
evalInt env = \case
  Int n -> n
  Var (VAR x) -> case env ! x of
                    SomeVal (VInt n) -> n
                    _ -> error "evalInt: ill-typed environment"
  Plus  a1 a2 -> evalInt env a1 + evalInt env a2
  Minus a1 a2 -> evalInt env a1 - evalInt env a2
  Times a1 a2 -> evalInt env a1 * evalInt env a2
  Abs a       -> abs $ evalInt env a
  Sign a      -> signum $ evalInt env a
  LengthS s   -> fromIntegral $ length $ evalString env s

evalString :: Map Register SomeVal -> StringExp -> String
evalString env = \case
  String s -> s
  Show a -> show $ evalInt env a
  ConcatS s1 s2 -> evalString env s1 ++ evalString env s2
  Var (VAR x) -> case env ! x of
                    SomeVal (VString s) -> s
                    _ -> error "evalInt: ill-typed environment"

evalSomeExp :: Map Register SomeVal -> SomeExp -> SomeVal
evalSomeExp env (SomeExp e) = case e of
  Var (VAR x) -> env ! x
  String{}    -> evalS e
  Show{}      -> evalS e
  ConcatS{}   -> evalS e
  Int{}       -> evalI e
  Plus{}      -> evalI e
  Minus{}     -> evalI e
  Times{}     -> evalI e
  Abs{}       -> evalI e
  Sign{}      -> evalI e
  LengthS{}   -> evalI e
  where
    evalS = SomeVal . VString . evalString env
    evalI = SomeVal . VInt . evalInt env

evalRegs :: Map Register Com -> [Map Register SomeVal]
evalRegs p =
  let initialEnv   = M.map ini p
      updater      = M.map recur p
      oneClock env = M.map (evalSomeExp env) updater
  in iterate oneClock initialEnv

eval :: (Exp ty, Map Register Com) -> [SomeVal]
eval (a, p) = map (evalSomeExp `flip` SomeExp a) $ evalRegs p

