{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

-- コンストラクタとかは後で隠す
module Groningen where

import Control.Monad.State
import Control.Monad.Trans.Writer

-- 変数
newtype Var = Var Int deriving Show

-- とりあえず，データは全て算術式とする
-- 将来的に幽霊型で安全性を担保したい
data AExp = AInt Integer
          | AVar Var
          | APlus AExp AExp
          | AMinus AExp AExp
          | ATimes AExp AExp
          | AAbs AExp
          | ASign AExp
					| APrim ([String] -> String) [ AExp ] 
          deriving Show

-- クロックに同期して動くレジスタ
-- これの集まりをプログラムとしてみなす
data Com = Com {
  -- 初期値
  ini :: Integer,
  -- 漸化式
  recur :: AExp
} deriving Show

-- プログラムを合成するモナド？
-- StateTでプログラムを書き出し，Stateでレジスタを数える
-- リストの末尾にどんどん追加していくのは流石に頭が悪いので，
-- 差分リストとかに置き換えた方が良い
type Groningen = StateT [Com] (State Int) AExp

-- 自己参照してレジスタを作る
fix :: Integer -> (AExp -> Groningen) -> Groningen
fix a f = do
  -- 現在の状態を覚えておく
  p <- get
  x <- lift get
  -- プログラムを出力させずにとりあえずfを呼び出してみて，
  -- レジスタがいくら確保されるか確かめる
  f $ AInt 42
  y <- lift get
  -- 巻き戻し
  put p
  lift $ put x
  a' <- f $ AVar $ Var y
  -- 一回実行してみて，大丈夫そうだった位置にレジスタを置く
  modify ( ++ [ Com { ini = a, recur = a' } ])
  lift $ put $ succ y
  return $ AVar $ Var y

-- 自己参照せずレジスタを作る
delay :: Integer -> AExp -> Groningen
delay a a' = do
  x <- lift get
  modify ( ++ [ Com { ini = a, recur = a' } ])
  lift $ modify succ
  return $ AVar $ Var x

-- モナドを走らせてプログラムを作る
runGroningen :: Groningen -> (AExp, [Com])
runGroningen m = fst $ flip runState 0 $ runStateT m []

-- 算術式を評価
evalAExp :: [Integer] -> AExp -> Integer
evalAExp env (AInt n) = n
evalAExp env (AVar (Var x)) = env !! x
evalAExp env (APlus a1 a2) = evalAExp env a1 + evalAExp env a2
evalAExp env (AMinus a1 a2) = evalAExp env a1 - evalAExp env a2
evalAExp env (ATimes a1 a2) = evalAExp env a1 * evalAExp env a2
evalAExp env (AAbs a) = abs $ evalAExp env a
evalAExp env (ASign a) = signum $ evalAExp env a

-- プログラムを評価
eval :: (AExp, [Com]) -> [Integer]
eval (a, p) =
  map (flip evalAExp a)
  $ iterate (\r -> map (evalAExp r . recur) p)
  $ map ini p


instance Num AExp where
  ( + ) = APlus
  ( - ) = AMinus
  ( * ) = ATimes
  abs = AAbs
  signum = ASign
  fromInteger = AInt

