
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all       #-}

module Main where

import           Control.Monad         (forM_)
import           Data.Extensible
import qualified Data.HashMap.Strict   as M
import qualified Data.IntMap           as IntMap
import           Groningen
import qualified Groningen.Compile as Foo

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

strEx :: Groningen 'TyString
strEx = delay "hoge" "fuga"

-------------------------------------------------------------------------------

zeroOne :: Groningen 'TyInt
zeroOne = fix 1 $ \n -> return $ If (n `Eq` 1) 0 1 - 1 + 1

-------------------------------------------------------------------------------

nestedIf :: Groningen 'TyInt
nestedIf = fix 1 $ \n -> do
    return $
      If (n `Eq` 0)
         1
         (If (n `Eq` 1) 2 0)

-------------------------------------------------------------------------------

fizz :: Groningen 'TyString
fizz = fizzAux =<< counter

fizzAux :: Exp 'TyInt -> Groningen 'TyString
fizzAux cnt = do
  return $ If ((cnt `Mod` 3) `Eq` 0) "Fizz" (Show cnt)

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

arrayExample :: Groningen ('TyList 'TyInt)
arrayExample = fix (VList []) $ \x -> do
    cnt <- counter
    return $ Cons cnt x

indexExample :: Groningen 'TyInt
indexExample = do
    cnt <- counter
    array <- arrayExample
    return $ If (cnt `Eq` 0) (-1) (array `Index` (cnt-1))

-------------------------------------------------------------------------------

jsonExample :: Groningen 'TyDict
jsonExample = do
    cnt <- counter
    let dic = M.fromList [ "hoge" .= cnt
                         , "name" .= String "example" ]
        z = Dict dic
    return z

lookupExamle1 :: Groningen 'TyInt
lookupExamle1 = Lookup "hoge" <$> jsonExample

(.=) :: (IsType ty, Eq (Val ty)) => String -> Exp ty -> (String, SomeExp)
key .= e = (key, SomeExp e)

-------------------------------------------------------------------------------

type ExampleType = 'TyDict'
  '[ "cnt" >: 'TyInt
   , "val" >: 'TyString
   ]

typedJsonExample :: Groningen ExampleType
typedJsonExample = do
    cnt <- (1+) <$> counter
    val <- fizzbuzzAux cnt
    let json = TypedDict
             $ #cnt @= cnt
            <! #val @= val
            <! nil
    return json

typedLookupExample :: Exp ExampleType -> Groningen 'TyString
typedLookupExample json = do
    let cnt = Show $ Lookup' #cnt json
        val = Lookup' #val json
    return $ cnt `ConcatS` " -> " `ConcatS` val

-------------------------------------------------------------------------------

example1 :: Groningen 'TyInt
example1 = do
    let colors = List
               [ List [  0,   0,   0 :: Exp 'TyInt]
               , List [255, 255, 255]
               , List [255, 255,   0]
               , List [255,   0, 255]
               , List [  0, 255, 255]
               , List [255,   0,   0]
               , List [  0, 255,   0]
               , List [  0,   0, 255]
               , List [255, 255, 255]
               , List [255, 255, 128]
               , List [255, 128, 255]
               , List [128, 255, 255]
               , List [255, 128, 128]
               , List [128, 255, 128]
               , List [128, 128, 255]
               , List [255, 128,   0]
               , List [  0, 255, 128]
               , List [128,   0, 255]
               ]
    (captured_image, resized_image) <- unTuple2 <$> _PrimCapture
    (categories, objects) <- unTuple2 <$> _PrimDetect resized_image

    let e = for' objects $ \object ->
                let conf = Lookup' #conf object
                    rect = Lookup' #rect object
                    c    = Lookup' #c    object

                    x_min = Index rect 0
                    y_min = Index rect 1
                    x_max = Index rect 2
                    y_max = Index rect 3

                    w = x_max - x_min
                    h = y_max - y_min

                    cx = x_min + w {- / 2 -} -- TODO Divの実装
                    cy = y_min + h {- / 2 -}

                    y_max' = If (w `Lt` h) (cy + w {- / 2 -}) y_max
                    y_min' = If (w `Lt` h) (cy - w {- / 2 -}) y_min
                    x_max' = If (w `Lt` h) x_max (cy + h {- / 2 -})
                    x_min' = If (w `Lt` h) x_min (cy - h {- / 2 -})

                    aspect_limit = 1.414 :: Double -- TODO sqrt

                in If (Index categories c `Eq` "") `flip` Unit $
                  let face_image = resize $ crop captured_image x_min' y_min' x_max' y_max'
                  in undefined conf w aspect_limit h cx cy face_image

    _ <- undefined colors categories objects e
    return 1

crop :: a
crop = undefined

resize :: a
resize = undefined

_PrimCapture :: Groningen (Tuple2 Image Image)
_PrimCapture = undefined
_PrimDetect :: Exp Image -> Groningen (Tuple2 ('TyList 'TyString) ('TyList Object))
_PrimDetect = undefined
_PrimPrint :: Exp 'TyString -> Exp 'TyUnit
_PrimPrint = undefined
_PrimPredict :: Exp Image -> Exp 'TyInt
_PrimPredict = undefined

type Image = 'TyList ('TyList 'TyInt)
type Object = 'TyDict'
  '[ "conf" >: 'TyInt
   , "rect" >: 'TyList 'TyInt
   , "c"    >: 'TyInt
   ]


-------------------------------------------------------------------------------
-- Tuple

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

-------------------------------------------------------------------------------

main :: IO ()
main = do
  print $ Foo.compileKProg 10 $ Foo.fromGroningen fizzbuzz

workWell :: IO ()
workWell = do
  print $ Foo.compileKProg 10 $ Foo.fromGroningen fib
  putStrLn "========"
  print $ Foo.compileKProg 10 $ Foo.fromGroningen shift123

evalExamples :: IO ()
evalExamples = do
    -- [fizzbuzz]
    print $ map unVString $ take 30 $ eval $ runGroningen fizzbuzz
    -- [array]
    print $ map (map unVInt . unVList) $ take 10 $ eval $ runGroningen arrayExample
    print $ map unVInt $ take 10 $ eval $ runGroningen indexExample
    -- [Record]
    print $ take 10 $ eval $ runGroningen jsonExample
    print $ take 10 $ eval $ runGroningen lookupExamle1
    -- [Typed Record]
    print $ take 10 $ eval $ runGroningen typedJsonExample
    mapM_ (putStrLn . unVString) $ take 30 $ eval $ runGroningen $ typedLookupExample =<< typedJsonExample

