
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wall        #-}

module Groningen.Type where

data Type
  = TyInt
  | TyBool
  | TyString
  | TyArray Type
  deriving (Show, Eq, Ord)

type family Repr (ty :: Type) where
  Repr 'TyInt = Integer
  Repr 'TyString = String
  Repr ('TyArray ty) = [Repr ty]

