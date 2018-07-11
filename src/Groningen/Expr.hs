
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wall #-}

module Groningen.Expr
  (
  -- Value
    Val(..)
  , SomeVal(..)
  -- Expression
  , Exp(..)
  , SomeExp(..)
  , Fun(..)
  , map'
  , for'
  -- Program
  , Groningen
  , MonadGroningen
  , Com(..)
  , initialState
  , fix
  , delay
  , runGroningen
  ) where

import           GHC.TypeLits           (Symbol, KnownSymbol)
import           Control.Arrow
import           Control.Lens           (makeLenses, use, view)
import           Control.Lens.Operators
import           Control.Monad.State    (State, runState)
import           Data.Function          (on)
import           Data.String            (IsString(..))
import           Data.HashMap.Strict    (HashMap)
import           Data.Extensible        hiding (State)
import           Data.Proxy

import           Groningen.Environment  (Environment, ShowTy(..))
import qualified Groningen.Environment  as Env
import           Groningen.Type

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

data Val (ty :: Type) where
  VUnit   :: Val 'TyUnit
  VInt    :: { unVInt    :: Integer }
          -> Val 'TyInt
  VFloat  :: { unVFloat  :: Double }
          -> Val 'TyFloat
  VBool   :: { unVBool   :: Bool }
          -> Val 'TyBool
  VString :: { unVString :: String }
          -> Val 'TyString
  VList   :: { unVList   :: [Val ty] }
          -> Val ('TyList ty)
  VDict   :: { unVDict   :: Dictionary }
          -> Val 'TyDict
  VDict'  :: (WrapForall Show (Field Val) xs)
          => { unVDict'  :: ValDict xs }
          -> Val ('TyDict' xs)
deriving instance Show (Val ty)
instance ShowTy Val where showTy = show
instance Eq  (Val 'TyInt) where (==) = (==) `on` unVInt
instance Ord (Val 'TyInt) where compare = compare `on` unVInt
instance Eq  (Val 'TyBool) where (==) = (==) `on` unVBool
instance Ord (Val 'TyBool) where compare = compare `on` unVBool
instance Eq  (Val 'TyString) where (==) = (==) `on` unVString
instance Ord (Val 'TyString) where compare = compare `on` unVString
instance Eq  (Val ty) => Eq  (Val ('TyList ty)) where (==) = (==) `on` unVList
instance Ord (Val ty) => Ord (Val ('TyList ty)) where compare = compare `on` unVList

instance Num (Val 'TyInt) where
  VInt n + VInt m = VInt (n+m)
  VInt n - VInt m = VInt (n-m)
  VInt n * VInt m = VInt (n*m)
  abs (VInt n)    = VInt (abs n)
  signum (VInt n) = VInt (signum n)
  fromInteger     = VInt

instance Num (Val 'TyFloat) where
  VFloat n + VFloat m = VFloat (n+m)
  VFloat n - VFloat m = VFloat (n-m)
  VFloat n * VFloat m = VFloat (n*m)
  abs (VFloat n)      = VFloat (abs n)
  signum (VFloat n)   = VFloat (signum n)
  fromInteger         = VFloat . fromInteger
instance Fractional (Val 'TyFloat) where
  fromRational = VFloat . fromRational
  VFloat x / VFloat y = VFloat (x/y)

instance ty ~ 'TyString => IsString (Val ty) where
  fromString = VString

type Dictionary = HashMap String SomeVal
data SomeVal where
  SomeVal :: (Eq (Val ty), IsType ty) -- むむ
          => Val ty -> SomeVal
deriving instance Show SomeVal

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

data Exp (ty :: Type) where
  Var    :: Env.VAR ty  -> Exp ty
  -- Literal
  ----------
  Unit   :: Exp 'TyUnit
  Int    :: Integer -> Exp 'TyInt
  Float  :: Double -> Exp 'TyFloat
  Bool   :: Bool -> Exp 'TyBool
  String :: String -> Exp 'TyString
  -- Seq
  Seq    :: Exp ty -> Exp ty' -> Exp ty'
  -- Int
  ----------
  Plus   :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Minus  :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Times  :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Mod    :: Exp 'TyInt -> Exp 'TyInt -> Exp 'TyInt
  Abs    :: Exp 'TyInt -> Exp 'TyInt
  Sign   :: Exp 'TyInt -> Exp 'TyInt
  -- Float
  PlusF  :: Exp 'TyFloat -> Exp 'TyFloat -> Exp 'TyFloat
  MinusF :: Exp 'TyFloat -> Exp 'TyFloat -> Exp 'TyFloat
  TimesF :: Exp 'TyFloat -> Exp 'TyFloat -> Exp 'TyFloat
  DivF   :: Exp 'TyFloat -> Exp 'TyFloat -> Exp 'TyFloat
  AbsF   :: Exp 'TyFloat -> Exp 'TyFloat
  SignF  :: Exp 'TyFloat -> Exp 'TyFloat
  Exp    :: Exp 'TyFloat -> Exp 'TyFloat
  Log    :: Exp 'TyFloat -> Exp 'TyFloat
  Sin    :: Exp 'TyFloat -> Exp 'TyFloat
  Cos    :: Exp 'TyFloat -> Exp 'TyFloat
  Asin   :: Exp 'TyFloat -> Exp 'TyFloat
  Acos   :: Exp 'TyFloat -> Exp 'TyFloat
  Atan   :: Exp 'TyFloat -> Exp 'TyFloat
  Sinh   :: Exp 'TyFloat -> Exp 'TyFloat
  Cosh   :: Exp 'TyFloat -> Exp 'TyFloat
  Asinh  :: Exp 'TyFloat -> Exp 'TyFloat
  Acosh  :: Exp 'TyFloat -> Exp 'TyFloat
  Atanh  :: Exp 'TyFloat -> Exp 'TyFloat
  -- Cast
  Truncate :: Exp 'TyFloat -> Exp 'TyInt
  Round    :: Exp 'TyFloat -> Exp 'TyInt
  Ceiling  :: Exp 'TyFloat -> Exp 'TyInt
  Floor    :: Exp 'TyFloat -> Exp 'TyInt
  ToFloat  :: Exp 'TyInt   -> Exp 'TyFloat
  -- Boolean
  ----------
  If     :: Exp 'TyBool -> Exp ty -> Exp ty -> Exp ty
  Eq     :: (IsType ty, Eq (Val ty)) => Exp ty -> Exp ty -> Exp 'TyBool
  Lt     :: (IsType ty, Ord (Val ty)) => Exp ty -> Exp ty -> Exp 'TyBool
         -- インタプリタのためにEq, Ord制約が必要
  And    :: Exp 'TyBool -> Exp 'TyBool -> Exp 'TyBool
  Or     :: Exp 'TyBool -> Exp 'TyBool -> Exp 'TyBool
  -- Array
  --------
  List   :: [Exp ty] -> Exp ('TyList ty)
  Nil    :: Exp ('TyList ty)
  Cons   :: Exp ty -> Exp ('TyList ty) -> Exp ('TyList ty)
  Tail   :: Exp ('TyList ty) -> Exp ('TyList ty)
  Index  :: Exp ('TyList ty) -> Exp 'TyInt -> Exp ty
  Map    :: Fun ty ty' -> Exp ('TyList ty) -> Exp ('TyList ty')
  -- Dict
  -------
  Dict   :: HashMap String SomeExp -> Exp 'TyDict
  -- 動的に型検査をするのでIsTypeが必要
  Lookup :: IsType ty => String -> Exp 'TyDict -> Exp ty
  TypedDict :: (Forall (Instance1 Show (Field Exp)) xs
               ,Forall (Instance1 Show (Field Val)) xs)
            => ExpDict xs -> Exp ('TyDict' xs)
  Lookup' :: (Associate field ty xs, KnownSymbol field)
          => Proxy field -> Exp ('TyDict' xs) -> Exp ty
  -- 動作を見るために適当に足したprimitives
  -----------------------------------------
  Show    :: Exp 'TyInt -> Exp 'TyString
  LengthS :: Exp 'TyString -> Exp 'TyInt
  ConcatS :: Exp 'TyString -> Exp 'TyString -> Exp 'TyString
deriving instance Show (Exp ty)
instance ShowTy Exp where showTy = show

data Fun tyArg tyRet where
  Fun :: (IsType tyArg, IsType tyRet)
      => (Env.VAR tyArg -> Exp tyRet) -> Fun tyArg tyRet
instance Show (Fun tyArg tyRet) where
  show Fun{} = "<fun>"

instance Num (Exp 'TyInt) where
  (+) = Plus
  (-) = Minus
  (*) = Times
  abs = Abs
  signum = Sign
  fromInteger = Int

instance Num (Exp 'TyFloat) where
  (+) = PlusF
  (-) = MinusF
  (*) = TimesF
  abs = AbsF
  signum = SignF
  fromInteger = Float . fromInteger
instance Fractional (Exp 'TyFloat) where
  fromRational = Float . fromRational
  (/) = DivF
instance Floating (Exp 'TyFloat) where
  pi    = Float pi
  exp   = Exp
  log   = Log
  sin   = Sin
  cos   = Cos
  asin  = Asin
  acos  = Acos
  atan  = Atan
  sinh  = Sinh
  cosh  = Cosh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh

instance ty ~ 'TyString => IsString (Exp ty) where
  fromString = String

data SomeExp where
  SomeExp :: (Eq (Val ty), IsType ty)
          => Exp ty -> SomeExp
deriving instance Show SomeExp

-------------------------------------------------------------------------------
-- Dict
-------------------------------------------------------------------------------

type ExpDict (xs :: [Assoc Symbol Type]) = RecordOf Exp xs
type ValDict (xs :: [Assoc Symbol Type]) = RecordOf Val xs
instance Wrapper Exp where type Repr Exp ty = Exp ty; _Wrapper = id
instance Wrapper Val where type Repr Val ty = Val ty; _Wrapper = id

-------------------------------------------------------------------------------

map' :: (IsType ty, IsType ty')
     => (Exp ty -> Exp ty') -> Exp ('TyList ty) -> Exp ('TyList ty')
map' = flip for'

for' :: (IsType ty, IsType ty')
     => Exp ('TyList ty) -> (Exp ty -> Exp ty') -> Exp ('TyList ty')
for' xs f = Map (Fun f') xs
  where f' = f . Var

-------------------------------------------------------------------------------
-- Program
-------------------------------------------------------------------------------

-- クロックに同期して動くレジスタ
-- これの集まりをプログラムとしてみなす
data Com ty = Com
  { ini   :: Val ty -- ^ 初期値
  , recur :: Exp ty -- ^ 漸化式
  } deriving Show
instance ShowTy Com where showTy = show

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

genNewVar :: IsType ty => MonadGroningen (Env.VAR ty)
genNewVar = Env.mkVar <$> genNewReg

incRegCount :: MonadGroningen ()
incRegCount = regCount %= succ

addCom :: IsType ty => Env.VAR ty -> Com ty -> MonadGroningen ()
addCom reg com = regCommands %= Env.insert reg com

-- Construct Groningen
----------------------

fix :: IsType ty => Val ty -> (Exp ty -> Groningen ty) -> Groningen ty
fix i f = do
    x  <- genNewVar
    a' <- f $ Var x
    addCom x Com { ini = i, recur = a' }
    return $ Var x

-- 自己参照せずレジスタを作る
delay :: IsType ty => Val ty -> Exp ty -> Groningen ty
delay a a' = do
    x <- genNewVar
    addCom x Com { ini = a, recur = a' }
    return $ Var x

-- モナドを走らせてプログラムを作る
runGroningen :: Groningen ty -> (Exp ty, Environment Com)
runGroningen m = second (view regCommands) $ runState m initialState

