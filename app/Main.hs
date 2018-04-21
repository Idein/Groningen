
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall                  #-}

module Main where

import           Groningen
import           Data.Map   (Map)
import qualified Data.Map   as M ()

-------------------------------------------------------------------------------

fib :: Groningen 'TyInt
fib = fix 1 $ \x -> do
  y <- delay 0 x
  return (x + y)

fibCode :: (Exp 'TyInt, Map Register Com)
fibCode = runGroningen fib

fibSym :: [SomeVal]
fibSym = take 10 $ eval fibCode

-------------------------------------------------------------------------------

shift123 :: Groningen 'TyInt
shift123 = fix 1 $ \x -> do
  y <- delay 3 x
  z <- delay 2 y
  return z

shift123Code :: (Exp 'TyInt, Map Register Com)
shift123Code = runGroningen shift123

shift123Sym :: [SomeVal]
shift123Sym = take 10 $ eval shift123Code

-------------------------------------------------------------------------------

fibShow :: Groningen 'TyString
fibShow = Show <$> fib

fibWord :: Groningen 'TyString
fibWord = fix "A" $ \x -> ConcatS x <$> delay "B" x

-------------------------------------------------------------------------------

main :: IO ()
main = do
  print $ take 10 $ eval $ runGroningen fibShow
  print $ take 05 $ eval $ runGroningen fibWord

