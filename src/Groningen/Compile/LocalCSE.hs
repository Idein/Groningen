
module Groningen.Compile.LocalCSE where

import qualified Data.IntMap         as IntMap
import qualified Data.Map            as M
import           Control.Monad.State
import           Groningen.Compile

type Env = M.Map KExp Var

localCSE :: KProg -> KProg
localCSE KProg{..} = KProg
  { kprogType = kprogType
  , kprogStructs = kprogStructs
  , kprogRegisters = IntMap.map localCSECom kprogRegisters
  , kprogExp = localCSEBlock' kprogExp
  }

localCSECom :: KCom -> KCom
localCSECom KCom{..} = KCom
  { comId = comId
  , initialValue = initialValue
  , oneStep = localCSEBlock' oneStep
  }

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' key val m = M.alter f key m
  where f = \case
          Nothing -> Just val
          just    -> just

localCSEBlock' :: KBlock -> KBlock
localCSEBlock' b = evalState (localCSEBlock b) M.empty

localCSEBlock :: KBlock -> State Env KBlock
localCSEBlock (KBlock insts e) = do
    insts' <- mapM localCSEInstruction insts
    e' <- localCSEExp e
    return $ KBlock insts' e'

localCSEInstruction :: Instruction -> State Env Instruction
localCSEInstruction (x := e) = do
    e' <- localCSEExp e
    modify' $ insert' e' x
    return $ x := e'

localCSEExp :: KExp -> State Env KExp
localCSEExp e = do
    e' <- case e of
      If v b1 b2 ->
        If v <$> local (localCSEBlock b1) <*> local (localCSEBlock b2)
      _ -> return e
    env <- get
    case M.lookup e' env of
      Nothing -> return e'
      Just x -> return (VarLit (V x))

local :: State s a -> State s a
local m = do
    sBak <- get
    x <- m
    put sBak
    return x

