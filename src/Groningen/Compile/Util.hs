
module Groningen.Compile.Util where

import           Control.Monad.State

local :: State s a -> State s a
local m = do
    sBak <- get
    x <- m
    put sBak
    return x

