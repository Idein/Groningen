
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall                  #-}

module Main where

import           Groningen hiding (map)

-------------------------------------------------------------------------------

fib :: Groningen 'TyInt
fib = fix 1 $ \x -> do
  y <- delay 0 x
  return (x + y)

fibCode :: (Exp 'TyInt, Environment Com)
fibCode = runGroningen fib

fibSym :: [Val 'TyInt]
fibSym = take 10 $ eval fibCode

-------------------------------------------------------------------------------

shift123 :: Groningen 'TyInt
shift123 = fix 1 $ \x -> do
  y <- delay 3 x
  z <- delay 2 y
  return z

shift123Code :: (Exp 'TyInt, Environment Com)
shift123Code = runGroningen shift123

shift123Sym :: [Val 'TyInt]
shift123Sym = take 10 $ eval shift123Code

-------------------------------------------------------------------------------

fibShow :: Groningen 'TyString
fibShow = Show <$> fib

fibWord :: Groningen 'TyString
fibWord = fix "A" $ \x -> ConcatS x <$> delay "B" x

-------------------------------------------------------------------------------

fizzbuzz :: Groningen 'TyString
fizzbuzz = do
  cnt <- counter
  return $
    If (isFizzBuzz cnt) "FizzBuzz" $
    If (isFizz cnt) "Fizz" $
    If (isBuzz cnt) "Buzz" $
    Show cnt

isFizzBuzz :: Exp 'TyInt -> Exp 'TyBool
isFizzBuzz x = (x `Mod` 15) `Eq` 0

isFizz :: Exp 'TyInt -> Exp 'TyBool
isFizz x = (x `Mod` 3) `Eq` 0

isBuzz :: Exp 'TyInt -> Exp 'TyBool
isBuzz x = (x `Mod` 5) `Eq` 0

-------------------------------------------------------------------------------

counter :: Groningen 'TyInt
counter = fix 1 $ \x -> return (x+1)

main :: IO ()
main = do
  print $ map unVString $ take 10 $ eval $ runGroningen fibShow
  print $ map unVString $ take 10 $ eval $ runGroningen fibWord
  mapM_ (putStrLn . unVString) $ take 30 $ eval $ runGroningen fizzbuzz

