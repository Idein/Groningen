{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module Main where

import           Data.String       (fromString)
import qualified Data.Text         as T
import           Data.Extensible
import qualified Shelly
import           Test.Hspec

import           Groningen
import qualified Groningen.Compile as Foo
import qualified Groningen.Compile.ConstFold as Opt
import qualified Groningen.Compile.LocalCSE as Opt

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "compile into C and run" $ do
      specify "dict1" $ testSample dictEx1
        ["10","20","10","20","10","20","10","20","10"]
      specify "fib" $ testSample fib
        ["1","1","2","3","5","8","13","21","34"]
      specify "fizzbuzz" $ testSample fizzbuzz
        ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz"
        ,"Buzz","11","Fizz","13","14","FizzBuzz","16","17"]
    runIO $ writeHoge 30 fizzbuzz


writeHoge :: IsType ty => Int -> Groningen ty -> IO ()
writeHoge n g = Shelly.shelly $ compile n "./C-experiments/hoge.c" g

-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

-- 'length expected' must be more than 0
testSample :: IsType ty => Groningen ty -> [String] -> Expectation
testSample g expected = runSample (length expected) g `shouldReturn` expected

runSample :: IsType ty => Int -> Groningen ty -> IO [String]
runSample num g = do
    let file = "sample.c"
    Shelly.shelly $ Shelly.withTmpDir $ \dir -> do
      lib <- Shelly.absPath "C-experiments/mylib.h"
      Shelly.cd dir
      Shelly.cp lib dir
      compile num (fromString file) g
      Shelly.run_ "gcc" ["-Wall", fromString file]
      Shelly.silently $ lines . T.unpack <$> Shelly.run "sh"  ["-c", "./a.out"]

compile :: IsType ty => Int -> Shelly.FilePath -> Groningen ty -> Shelly.Sh ()
compile num file g = Shelly.writefile file $ fromString $ show $
    Foo.compileKProg num $ optimize $ Foo.fromGroningen g
  where
    optimize = Opt.localCSE . Opt.constFold . Opt.localCSE . Opt.constFold

-------------------------------------------------------------------------------
-- Samples
-------------------------------------------------------------------------------

-- fib
------

fib :: Groningen 'TyInt
fib = fix 1 $ \x -> do
    y <- delay 0 x
    return (x + y)

-- fizzbuzz
-----------

fizzbuzz :: Groningen 'TyString
fizzbuzz = fizzbuzzAux =<< (1+) <$> counter

fizzbuzzAux :: Exp 'TyInt -> Groningen 'TyString
fizzbuzzAux cnt = do
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

counter :: Groningen 'TyInt
counter = fix 0 $ \x -> return (x+1)

-- Dict
-------

type DictEx1 = 'TyDict'
  '[ "x" >: 'TyInt
   , "y" >: 'TyInt
   ]

dictValEx1 :: Val DictEx1
dictValEx1 = VDict'
        $ #x @= 10
       <! #y @= 20
       <! nil

dictEx0 :: Groningen DictEx1
dictEx0 = fix dictValEx1 $ \e -> do
  return $ TypedDict
         $ #x @= Lookup' #y e
        <! #y @= Lookup' #x e
        <! nil

dictEx1 :: Groningen 'TyInt
dictEx1 = Lookup' #x <$> dictEx0

