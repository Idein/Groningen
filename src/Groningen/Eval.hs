
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall   #-}

module Groningen.Eval (eval) where

import           Data.Maybe            (fromMaybe)

import           Groningen.Environment (Environment)
import qualified Groningen.Environment as Env
import           Groningen.Expr

eval :: (Exp ty, Environment Com) -> [Val ty]
eval (a, p) = map (evalExp `flip` a) $ evalRegs p

evalRegs :: Environment Com -> [Environment Val]
evalRegs envCom =
  let initialEnv   = Env.map ini envCom
      updater      = Env.map recur envCom
      oneClock env = Env.map (evalExp env) updater
  in iterate oneClock initialEnv

evalExp :: Environment Val -> Exp ty -> Val ty
evalExp env = \case
  -- Var
  Var v         -> fromMaybe (error "にゃん...") $ Env.lookup v env
  -- Constant
  Int n         -> VInt n
  Bool b        -> VBool b
  String s      -> VString s
  -- Arith
  Plus  e1 e2   -> evalExp env e1 + evalExp env e2
  Minus e1 e2   -> evalExp env e1 - evalExp env e2
  Times e1 e2   -> evalExp env e1 * evalExp env e2
  Mod   e1 e2   -> VInt $ unVInt (evalExp env e1) `mod` unVInt (evalExp env e2)
  Abs e         -> abs $ evalExp env e
  Sign e        -> signum $ evalExp env e
  -- Boolean operation
  If eb e1 e2   -> if unVBool (evalExp env eb)
                   then evalExp env e1
                   else evalExp env e2
  Eq e1 e2      -> VBool $ evalExp env e1 == evalExp env e2
  -- Array
  Nil           -> VArray []
  Cons e es     -> VArray $ evalExp env e : unVArray (evalExp env es)
  -- tekitou primitives
  LengthS e     -> fromIntegral $ length $ unVString $ evalExp env e
  Show e        -> VString $ show $ unVInt $ evalExp env e
  ConcatS e1 e2 -> VString $ unVString (evalExp env e1)
                          ++ unVString (evalExp env e2)

