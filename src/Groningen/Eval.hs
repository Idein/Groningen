
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Groningen.Eval (eval) where

import           Control.Lens          (view)
import           Data.Extensible       (itemAssoc)
import qualified Data.HashMap.Strict   as M

import           Groningen.Environment (Environment)
import qualified Groningen.Environment as Env
import           Groningen.Expr
import           Groningen.Type

eval :: (Exp ty, Environment Com) -> [Val ty]
eval (a, p) = map (evalExp `flip` a) $ evalRegs p

evalRegs :: Environment Com -> [Environment Val]
evalRegs envCom =
  let initialEnv   = Env.map ini envCom
      updater      = Env.map recur envCom
      oneClock env = Env.map (evalExp env) updater
  in iterate oneClock initialEnv

evalExp :: Environment Val -> Exp ty -> Val ty
evalExp env e0 = case e0 of
  -- Var
  Var v         -> either error id $ Env.lookupE v env
  -- Constant
  Unit          -> VUnit
  Int n         -> VInt n
  Float f       -> VFloat f
  Bool b        -> VBool b
  String s      -> VString s
  -- Seq
  Seq e1 e2     -> evalExp env e1 `seq` evalExp env e2
  -- Int
  Plus  e1 e2   -> evalExp env e1 + evalExp env e2
  Minus e1 e2   -> evalExp env e1 - evalExp env e2
  Times e1 e2   -> evalExp env e1 * evalExp env e2
  Mod   e1 e2   -> VInt $ unVInt (evalExp env e1) `mod` unVInt (evalExp env e2)
  Abs e         -> abs $ evalExp env e
  Sign e        -> signum $ evalExp env e
  -- Float
  PlusF  e1 e2 -> evalExp env e1 + evalExp env e2
  MinusF e1 e2 -> evalExp env e1 - evalExp env e2
  TimesF e1 e2 -> evalExp env e1 * evalExp env e2
  DivF   e1 e2 -> evalExp env e1 / evalExp env e2
  AbsF   e     -> VFloat . abs    . unVFloat $ evalExp env e
  SignF  e     -> VFloat . signum . unVFloat $ evalExp env e
  Exp    e     -> VFloat . exp    . unVFloat $ evalExp env e
  Log    e     -> VFloat . log    . unVFloat $ evalExp env e
  Sin    e     -> VFloat . sin    . unVFloat $ evalExp env e
  Cos    e     -> VFloat . cos    . unVFloat $ evalExp env e
  Asin   e     -> VFloat . asin   . unVFloat $ evalExp env e
  Acos   e     -> VFloat . acos   . unVFloat $ evalExp env e
  Atan   e     -> VFloat . atan   . unVFloat $ evalExp env e
  Sinh   e     -> VFloat . sinh   . unVFloat $ evalExp env e
  Cosh   e     -> VFloat . cosh   . unVFloat $ evalExp env e
  Asinh  e     -> VFloat . asinh  . unVFloat $ evalExp env e
  Acosh  e     -> VFloat . acosh  . unVFloat $ evalExp env e
  Atanh  e     -> VFloat . atanh  . unVFloat $ evalExp env e
  -- Cast
  Truncate e -> VInt   . truncate   . unVFloat $ evalExp env e
  Round    e -> VInt   . round      . unVFloat $ evalExp env e
  Ceiling  e -> VInt   . ceiling    . unVFloat $ evalExp env e
  Floor    e -> VInt   . floor      . unVFloat $ evalExp env e
  ToFloat  e -> VFloat . realToFrac . unVInt   $ evalExp env e
  -- Boolean
  If eb e1 e2   -> if unVBool (evalExp env eb)
                   then evalExp env e1
                   else evalExp env e2
  Eq e1 e2      -> VBool $ evalExp env e1 == evalExp env e2
  Lt e1 e2      -> VBool $ evalExp env e1 <  evalExp env e2
  And e1 e2     -> VBool $ unVBool (evalExp env e1) && unVBool (evalExp env e2)
  Or e1 e2      -> VBool $ unVBool (evalExp env e1) || unVBool (evalExp env e2)
  -- Array
  --Nil           -> VList []
  Cons e1 e2    -> VList $ evalExp env e1 : unVList (evalExp env e2)
  Tail e        -> VList $ tail $ unVList $ evalExp env e
  List es       -> VList $ map (evalExp env) es
  Index e1 e2   -> let es = unVList $ evalExp env e1
                       i  = fromIntegral $ unVInt $ evalExp env e2
                   in if length es > i
                      then es !! i
                      else error "evalExp: index out of bounds"
  Map (Fun f) e -> case evalExp env e of
                     VList vs -> VList $ map `flip` vs $ \v ->
                       let var = Env.freshVar env
                       in  evalExp (Env.insert var v env) (f var)
  -- Untyped Dictionary
  Dict dic      -> VDict $ M.map evalSomeExp dic
    where evalSomeExp (SomeExp e) = SomeVal (evalExp env e)
  Lookup s e    -> case M.lookup s (unVDict (evalExp env e)) of
                     Nothing -> error $ "evalExp: key `" ++ s ++ "`not found"
                     Just (SomeVal v) ->
                       case v `hasSameTypeAs` e0 of
                         Just Refl -> v
                         Nothing   -> error "evalExp: type mismatch"
  -- Typed Dictionary
  TypedDict dic -> VDict' $ hmapOnTypeDic (\_ -> evalExp env) dic
  Lookup' p e   -> view (itemAssoc p) . unVDict' $ evalExp env e
  -- tekitou primitives
  LengthS e     -> fromIntegral . length . unVString $ evalExp env e
  Show e        -> VString . show . unVInt $ evalExp env e
  ConcatS e1 e2 -> VString $ unVString (evalExp env e1)
                          ++ unVString (evalExp env e2)

