{-# LANGUAGE OverloadedLabels #-}

module Groningen
  ( module X
  , Tuple2
  , Tuple3
  , fst'
  , snd'
  , trd'
  , tuple2
  , tuple3
  , unTuple2
  , unTuple3
  ) where

import           Groningen.Environment as X hiding (empty, member, size, insert, delete, lookup, lookupE, map)
import           Groningen.Eval        as X
import           Groningen.Expr        as X
import           Groningen.Type        as X
import           Data.Extensible

type Tuple2 ty1 ty2 = 'TyDict'
  '[ "_1" >: ty1
   , "_2" >: ty2
   ]
type Tuple3 ty1 ty2 ty3 = 'TyDict'
  '[ "_1" >: ty1
   , "_2" >: ty2
   , "_3" >: ty3
   ]

fst' :: (IsTypeDic xs, Associate "_1" ty xs) => Exp ('TyDict' xs) -> Exp ty
fst' e = Lookup' #_1 e

snd' :: (IsTypeDic xs, Associate "_2" ty xs) => Exp ('TyDict' xs) -> Exp ty
snd' e = Lookup' #_2 e

trd' :: (IsTypeDic xs, Associate "_3" ty xs) => Exp ('TyDict' xs) -> Exp ty
trd' e = Lookup' #_3 e

tuple2 :: (IsType ty1, IsType ty2)
       => Exp ty1 -> Exp ty2 -> Exp (Tuple2 ty1 ty2)
tuple2 e1 e2 = TypedDict
             $ #_1 @= e1
            <! #_2 @= e2
            <! nil

tuple3 :: (IsType ty1, IsType ty2, IsType ty3)
       => Exp ty1 -> Exp ty2 -> Exp ty3 -> Exp (Tuple3 ty1 ty2 ty3)
tuple3 e1 e2 e3 = TypedDict
                $ #_1 @= e1
               <! #_2 @= e2
               <! #_3 @= e3
               <! nil

unTuple2 :: (IsType ty1, IsType ty2)
         => Exp (Tuple2 ty1 ty2) -> (Exp ty1, Exp ty2)
unTuple2 e = (fst' e, snd' e)

unTuple3 :: (IsType ty1, IsType ty2, IsType ty3)
         => Exp (Tuple3 ty1 ty2 ty3) -> (Exp ty1, Exp ty2, Exp ty3)
unTuple3 e = (fst' e, snd' e, trd' e)

