{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-all #-}

module Groningen.Compile where

import           Control.Arrow              (second)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Singletons
import qualified Data.IntMap                as M
import           GHC.Stack                  (HasCallStack)

import           Text.PrettyPrint.HughesPJClass

import qualified Groningen                  as G

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

notImplemented :: Show a => String -> a -> b
notImplemented func x = error $ "Groningen.Compile: " ++ func ++ ": " ++ show x

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

----------------------------------------
-- Type
----------------------------------------

data CType
  = CVoid
  | CInt
  | CBool
  | CDouble
  | CString
  -- ... TODO
  deriving (Show, Eq, Ord)

cTypeOf :: forall ty proxy. G.IsType ty => proxy ty -> CType
cTypeOf = toCType . G.typeOf

toCType :: G.Ty -> CType
toCType = \case
    G.TInt    -> CInt
    G.TBool   -> CBool
    G.TFloat  -> CDouble
    G.TString -> CString
    ty -> notImplemented "toCType" ty

----------------------------------------
-- Exp
----------------------------------------

data Var
  = GrobalVar
      { varId   :: Int
      , varName :: String
      , varType :: CType
      }
  | TmpVar
      { varId   :: Int
      , varName :: String
      , varType :: CType
      , alreadyDeclared :: Bool
      }
  deriving (Show, Eq)

isDeclared :: Var -> Bool
isDeclared GrobalVar{} = True
isDeclared TmpVar{alreadyDeclared} = alreadyDeclared

declared :: Var -> Var
declared v@GrobalVar{} = v
declared v@TmpVar{} = v { alreadyDeclared = True }

data VarLit
  = V Var
  | Unit
  | Int Integer
  | Double Double
  | Bool Bool
  | String String
  deriving (Show, Eq)

-- | K-normal form expression
data KExp
  -- Literal
  = VarLit VarLit
  -- Int Op
  | Plus  VarLit VarLit
  | Minus VarLit VarLit
  | Times VarLit VarLit
  | Mod   VarLit VarLit
  -- Bool
  | If VarLit KBlock KBlock
  | Eq CType VarLit VarLit
  -- tekitou
  | Show VarLit
  deriving (Show, Eq)

data Instruction = Var := KExp
  deriving (Show, Eq)

----------------------------------------
-- Block / Program
----------------------------------------

data KBlock = KBlock [Instruction] KExp
  deriving (Show, Eq)

data KCom = KCom
  { comId        :: Var
  , initialValue :: VarLit
  , oneStep      :: KBlock
  } deriving (Show, Eq)

data KProg = KProg
  { aprogType      :: CType
  , aprogRegisters :: M.IntMap KCom
  , aprogExp       :: KBlock
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- From Groningen
-------------------------------------------------------------------------------

data KState = KState
  { _varCount :: Int
  , _instructions :: [Instruction]
  } deriving (Show, Eq)
makeLenses ''KState

initialK :: KState
initialK = KState
    { _varCount     = 0
    , _instructions = []
    }

fromGroningen :: G.IsType ty => G.Groningen ty -> KProg
fromGroningen g = KProg
    { aprogType       = cTypeOf e
    , aprogRegisters  = M.mapWithKey f coms
    , aprogExp        = toKBlock e
    }
  where
    (e, gcoms) = G.runGroningen g
    coms = G._intoMap gcoms

    f :: Int -> G.Some G.Com -> KCom
    f n (G.Some gcom) = KCom
        { comId        = mkGrobalVar n gcom
        , initialValue = val $ G.ini gcom
        , oneStep      = toKBlock (G.recur gcom)
        }

    val :: G.IsType ty => G.Val ty -> VarLit
    val = \case
        G.VUnit -> Unit
        G.VInt n -> Int n
        G.VFloat n -> Double n
        G.VBool n -> Bool n
        G.VString n -> String n
        v -> notImplemented "fromGExp" v

toKBlock :: G.IsType ty => G.Exp ty -> KBlock
toKBlock e =
    uncurry (flip KBlock)
      $ second (reverse . view instructions)
      $ runState (toKBlockSub e) initialK

toKBlockSub :: forall ty.
       (HasCallStack, G.IsType ty)
    => G.Exp ty -> State KState KExp
toKBlockSub = \case
    -- Var
    -------
    G.Var v@(G.VAR x) -> return $ VarLit $ V $ mkGrobalVar x v
    -- Lit
    -------
    G.Int n    -> return $ VarLit $ Int n
    G.Float n  -> return $ VarLit $ Double n
    G.Bool n   -> return $ VarLit $ Bool n
    G.String n -> return $ VarLit $ String n
    -- Arith
    ---------
    G.Plus  e1 e2 -> int2 Plus  e1 e2
    G.Minus e1 e2 -> int2 Minus e1 e2
    G.Times e1 e2 -> int2 Times e1 e2
    G.Mod   e1 e2 -> int2 Mod   e1 e2
    -- Bool
    G.If b e1 e2  -> do
        xb <- bind CBool =<< toKBlockSub b
        return $ If xb (toKBlock e1) (toKBlock e2)
    G.Eq e1 e2
      | cTypeOf e1 == CInt -> int2 (Eq CInt) e1 e2
    -- tekitou
    G.Show e1 -> Show <$> (bind CInt =<< toKBlockSub e1)

    -- TODO
    --------
    e -> notImplemented "foo" e
  where
    int2 f e1 e2 = do
        x1 <- bind CInt =<< toKBlockSub e1
        x2 <- bind CInt =<< toKBlockSub e2
        return $ f x1 x2

bind :: CType -> KExp -> State KState VarLit
bind t e = do
    x <- genNewVar t
    addInst (x := e)
    return $ V x

genNewVar :: CType -> State KState Var
genNewVar t = do
    i <- varCount <+= 1
    return $ TmpVar
      { varId   = i
      , varName = mkTmpVarName i
      , varType = t
      , alreadyDeclared = False
      }

addInst :: Instruction -> State KState ()
addInst inst = instructions %= (inst:)

mkGrobalVar :: G.IsType ty => Int -> proxy ty -> Var
mkGrobalVar x p = GrobalVar
    { varId   = x
    , varName = mkGrobalVarName x
    , varType = cTypeOf p
    }
mkGrobalVarName :: Int -> String
mkGrobalVarName n = "var" ++ show n

mkTmpVarName :: Int -> String
mkTmpVarName n = "tmp" ++ show n

-------------------------------------------------------------------------------
-- Compile
-------------------------------------------------------------------------------

compileKProg :: Int -> KProg -> Doc
compileKProg numLoop (KProg ty coms ret) = vcat
    [ text "#include \"mylib.h\""
    , text "int main()"
    , withBrace $ vcat
        [ vcat $ map prettyComInit xs
        , text "int counter = 0;"
        , text "while(1)"
        , withBrace $ vcat
            [ result
            , vcat $ map prettyComStep xs
            , vcat $ map prettyUpdate xs
            --, text "sleep(1);"
            , text "counter++;"
            , text $ "if (counter>=" ++ show numLoop ++ ") break;"
            ]
        ]
    ]
  where
    xs = map snd $ M.toAscList coms
    result = withBrace $ vcat $
        map pInstruction inst
        ++
        [ pInstruction (resVar := e)
        , printResIfPossible
        ]
      where
        KBlock inst e = ret
        resVar = TmpVar (-1) "res" ty False
        printResIfPossible = case ty of
          CInt -> sentence $
            text "printf"
            <> parens (text "\"%d\\n\"," <+> pVar resVar)
          CString -> sentence $
            text "printf"
            <> parens (text "\"%s\\n\"," <+> pStrVarBody resVar)
          _ -> mempty

pType :: CType -> Doc
pType = \case
    CVoid   -> text "void"
    CInt    -> text "int"
    CBool   -> text "bool"
    CDouble -> text "double"
    CString -> text "String"
    t       -> notImplemented "pType" t

pVar :: Var -> Doc
pVar = text . varName

pVarPtr :: Var -> Doc
pVarPtr x = text "&" <> pVar x

pStrVarBody :: Var -> Doc
pStrVarBody x
  | varType x == CString = pVar x <> text ".buf"
  | otherwise = error $ "pStrVarBody: " ++ show x

pStrVarSize :: Var -> Doc
pStrVarSize x
  | varType x == CString = pVar x <> text ".size"
  | otherwise = error $ "pStrVarBody: " ++ show x

pVarLit :: VarLit -> Doc
pVarLit = \case
    V v       -> pVar v
    Int n     -> pPrint n
    Double f  -> pPrint f
    Bool b    -> text $ if b then "true" else "false"
    String s  -> text (show s)
    Unit      -> error "Pretty VarLit: Unit" -- どうすんだこれ

pKExp :: KExp -> Doc
pKExp = \case
    VarLit vl   -> pVarLit vl
    Plus  e1 e2 -> op "+" e1 e2
    Minus e1 e2 -> op "-" e1 e2
    Times e1 e2 -> op "*" e1 e2
    Mod   e1 e2 -> op "%" e1 e2
    Eq _  e1 e2 -> op "==" e1 e2
    e           -> notImplemented "pKExp" e
  where
    op s e1 e2 = pVarLit e1 <+> text s <+> pVarLit e2

pDeclareVar :: Var -> Doc
pDeclareVar x = sentence (pType t <+> pVar x)
             <+> initialize
  where
    t = varType x
    initialize = case t of
      CString -> sentence $
        function "InitializeString"
          [ pVarPtr x
          , text "NULL"
          ]
      _ -> mempty

pDeclareVarWith :: Var -> VarLit -> Doc
pDeclareVarWith x v0
  | varType x == CVoid =
      text $ "/* void " ++ varName x ++ " */"
  | otherwise = case varType x of
      CString -> sentences
        [ pType CString <+> pVar x
        , function "InitializeString"
            [ pVarPtr x
            , pVarLit v0
            ]
        ]
      t -> sentences
        [ pType t <+> pVar x <+> text "=" <+> pVarLit v0
        ]

pBindKExp :: Var -> KExp -> Doc
pBindKExp x e = case (varType x, e) of
  (_, If v b1 b2) -> vcat
      [ text "if" <> parens (pVarLit v)
      , pBindKBlock x b1
      , text "else"
      , pBindKBlock x b2
      ]
  -- 次の２つのケースを合わせたい．
  (CString, VarLit valStr) -> sentences
      [ function "BindString"
          [ pVar x
          , case valStr of
              V y -> pStrVarBody y
              String s -> text (show s)
          ]
      ]
  (CString, Show valInt) -> sentences
      [ function "snprintf"
          [ pStrVarBody x
          , pStrVarSize x
          , text (show "%d")
          , case valInt of
              V y   -> pVar y
              Int n -> text (show n)
          ]
      ]
  _ -> sentences
      [ pVar x <+> text "=" <+> pKExp e
      ]

pBindKBlock :: Var -> KBlock -> Doc
pBindKBlock x (KBlock inst e) =
    withBrace $ vcat $ map pInstruction (inst ++ [x := e])

pInstruction :: Instruction -> Doc
pInstruction (x := e)
  | isDeclared x = pBindKExp x e
  | otherwise    = pDeclareVar x $+$ pBindKExp (declared x) e

prettyComInit :: KCom -> Doc
prettyComInit (KCom x v0 _)
  | varType x == CVoid = mempty
  | otherwise = pDeclareVarWith x v0
            $+$ pDeclareVar (x { varName = varName x ++ "_Next" })

prettyComStep :: KCom -> Doc
prettyComStep (KCom x _ b) =
    pBindKBlock x { varName = varName x ++ "_Next" } b

prettyUpdate :: KCom -> Doc
prettyUpdate (KCom x@GrobalVar{} _ _) =
    pBindKExp x (VarLit (V x { varName = varName x ++  "_Next" }))

--------
-- Util
--------

withBrace :: Doc -> Doc
withBrace x = lbrace $+$ nest 4 x $+$ rbrace

sentence :: Doc -> Doc
sentence = (<>semi)

sentences :: [Doc] -> Doc
sentences xs = vcat $ map sentence xs

function :: String -> [Doc] -> Doc
function func args = text func <> parens (hsep $ punctuate comma args)

