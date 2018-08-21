
module Groningen.Compile.ConstFold where

import qualified Data.IntMap            as IntMap
import qualified Data.Map               as M
import           Control.Monad.State
import           Groningen.Compile
import           Groningen.Compile.Util

type Env = M.Map Var VarLit

constFold :: KProg -> KProg
constFold KProg{..} = KProg
  { kprogType = kprogType
  , kprogStructs = kprogStructs
  , kprogRegisters = IntMap.map constFoldCom kprogRegisters
  , kprogExp = constFoldBlock' kprogExp
  }

constFoldCom :: KCom -> KCom
constFoldCom KCom{..} = KCom
  { comId = comId
  , initialValue = initialValue
  , oneStep = constFoldBlock' oneStep
  }

constFoldBlock' :: KBlock -> KBlock
constFoldBlock' b = evalState (constFoldBlock b) M.empty

constFoldBlock :: KBlock -> State Env KBlock
constFoldBlock (KBlock insts e) = do
    insts' <- mapM constFoldInstruction insts
    e' <- constFoldExp e
    return $ KBlock insts' e'

constFoldInstruction :: Instruction -> State Env Instruction
constFoldInstruction (x := e) = do
    e' <- constFoldExp e
    case e' of
      VarLit v -> modify' $ M.insert x v
      _ -> return ()
    return (x := e')

constFoldExp :: KExp -> State Env KExp
constFoldExp = \case
    VarLit v -> VarLit <$> constFoldVarLit v
    Plus v1 v2  -> int2 (+) Plus  v1 v2
    Minus v1 v2 -> int2 (-) Minus v1 v2
    Times v1 v2 -> int2 (*) Times v1 v2
    Mod v1 v2   -> int2 mod Mod   v1 v2 -- TODO HaskellのModとCの%は微妙に挙動が違った気がする
    If v1 b1 b2 -> If <$> constFoldVarLit v1 -- TODO
                      <*> local (constFoldBlock b1)
                      <*> local (constFoldBlock b2)
    Eq ty v1 v2 -> (,) <$> constFoldVarLit v1 <*> constFoldVarLit v2 >>= \case
      (Unit, Unit) -> return $ VarLit (Bool True)
      (Int x1, Int x2)       -> return $ VarLit (Bool (x1==x2))
      (Double x1, Double x2) -> return $ VarLit (Bool (x1==x2))
      (String x1, String x2) -> return $ VarLit (Bool (x1==x2))
      (v1', v2')             -> return $ Eq ty v1' v2'
    Show v1 -> constFoldVarLit v1 >>= \case
      Int x -> return $ VarLit (String (show x))
      v1' -> return $ Show v1'
    Member v1 s -> Member `flip` s <$> constFoldVarLit v1
      -- TODO なんかできないかな
  where
    int2 f g v1 v2 = ((,) <$> constFoldVarLit v1 <*> constFoldVarLit v2) >>= \case
      (Int i1, Int i2) -> return $ VarLit (Int (f i1 i2))
      (v1', v2') -> return $ g v1' v2'

constFoldVarLit :: VarLit -> State Env VarLit
constFoldVarLit (V x) = do
    env <- get
    case M.lookup x env of
      Nothing -> return (V x)
      Just v -> return v
constFoldVarLit v = return v

