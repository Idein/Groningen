{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wall #-}


module Groningen.Compile where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.IntMap                            (IntMap)
import qualified Data.IntMap                            as IntMap
import           Data.List                              (intercalate)
import           Data.Map                               (Map)
import qualified Data.Map                               as M
import           Data.Maybe                             (fromJust)
import           GHC.Stack
import           GHC.TypeLits                           (symbolVal)

import qualified Text.PrettyPrint.HughesPJClass         as PP
import           Text.PrettyPrint.HughesPJClass.Monadic

import qualified Groningen                              as G

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

notImplemented :: (HasCallStack, Show a) => a -> b
notImplemented x =
    error $ prettySrcLocSimple loc ++ ": Not implemented: " ++ ": " ++ show x
  where
    loc = snd $ head $ getCallStack callStack

prettySrcLocSimple :: SrcLoc -> String
prettySrcLocSimple SrcLoc{..} = intercalate ":"
    [ srcLocFile
    , show srcLocStartLine
    , show srcLocStartCol
    ]

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
  | CList CType
  | CStruct StructDef
  -- ... TODO
  deriving (Show, Eq, Ord)

newtype StructDef = StructDef { unStructdef :: [(String, CType)] }
  deriving (Show, Eq, Ord)

cTypeOf :: forall ty proxy. G.IsType ty => proxy ty -> CType
cTypeOf = toCType . G.typeOf
  where
    toCType :: G.Ty -> CType
    toCType = \case
      G.TInt    -> CInt
      G.TBool   -> CBool
      G.TFloat  -> CDouble
      G.TString -> CString
      G.TDict' ts -> CStruct . StructDef
                      $ map (\(G.TyAssoc s t) -> (s, toCType t)) ts
      ty -> notImplemented ty

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
  deriving (Show, Eq, Ord)

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
  | Struct String [(String, VarLit)]
  deriving (Show, Eq, Ord)

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
  --
  | Member VarLit String
  deriving (Show, Eq, Ord)

data Instruction = Var := KExp
  deriving (Show, Eq, Ord)

----------------------------------------
-- Block / Program
----------------------------------------

data KBlock = KBlock [Instruction] KExp
  deriving (Show, Eq, Ord)

data KCom = KCom
  { comId        :: Var
  , initialValue :: VarLit
  , oneStep      :: KBlock
  } deriving (Show, Eq, Ord)

data KProg = KProg
  { kprogType      :: CType
  , kprogStructs   :: Map StructDef String
  , kprogRegisters :: IntMap KCom
  , kprogExp       :: KBlock
  } deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- From Groningen
-------------------------------------------------------------------------------

data KBlockState = KBlockState
  { _kvarCount      :: Int
  , _kinstructions  :: [Instruction]
  }
makeLenses ''KBlockState

initialKBlockState :: KBlockState
initialKBlockState = KBlockState
  { _kvarCount      = -1
  , _kinstructions  = []
  }

data KProgState = KProgState
  { _kstructsCount  :: Int
  , _kstructs       :: Map StructDef String
  , _kblock         :: KBlockState
  }
makeLenses ''KProgState

initialKProgState :: KProgState
initialKProgState = KProgState
  { _kstructsCount  = -1
  , _kstructs       = M.empty
  , _kblock         = initialKBlockState
  }

fromGroningen :: forall ty. G.IsType ty => G.Groningen ty -> KProg
fromGroningen g = flip evalState initialKProgState $ do
    kprogType <- return ty
    kprogRegisters <-
      fmap IntMap.fromAscList $
        forM (IntMap.toAscList coms) $ \(i,G.Some gcom) -> do
          let comId = mkGrobalVar i gcom
          initialValue <- kValue $ G.ini gcom
          oneStep <- toKBlock $ G.recur gcom
          return (i, KCom{..})
    kprogExp <- toKBlock e
    kprogStructs <- use kstructs
    return KProg{..}
  where
    (e, gcoms) = G.runGroningen g
    coms = G._intoMap gcoms
    ty = cTypeOf e

toKBlock :: forall ty. G.IsType ty
          => G.Exp ty -> State KProgState KBlock
toKBlock e = do
    instBak <- use $ kblock.kinstructions
    kblock.kinstructions .= []
    --kblockBak <- use kblock
    --kblock .= initialKBlockState
    k <- toKBlockSub e
    insts <- use (kblock.kinstructions)
    --kblock .= kblockBak
    kblock.kinstructions .= instBak
    return $ KBlock (reverse insts) k

toKBlockSub :: forall ty. (G.IsType ty)
            => G.Exp ty -> State KProgState KExp
toKBlockSub e = case e of
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
        bThen <- toKBlock e1
        bElse <- toKBlock e2
        return $ If xb bThen bElse
        -- TODO toKBlockダメだ
    G.Eq e1 e2
      | cTypeOf e1 == CInt -> int2 (Eq CInt) e1 e2
      | otherwise -> notImplemented e
    -- tekitou
    G.Show e1 -> Show <$> (bind CInt =<< toKBlockSub e1)

    -- Dict
    G.TypedDict es -> do
      let CStruct def' = cTypeOf e
          (keys, tys) = unzip $ unStructdef def'
          ms = G.mapOnTypeDic (\_key x -> toKBlockSub x) es
      name <- getStructName def'
      xs <- forM (zip tys ms) $ \(ty,m) -> bind ty =<< m
      return $ VarLit $ Struct name (zip keys xs)
    G.Lookup' (symbolVal -> key) e1 -> do
      x <- bind (cTypeOf e1) =<< toKBlockSub e1
      return $ Member x key

    -- TODO
    --------
    _ -> notImplemented e
  where
    int2 f e1 e2 = do
        x1 <- bind CInt =<< toKBlockSub e1
        x2 <- bind CInt =<< toKBlockSub e2
        return $ f x1 x2

kValue :: G.IsType ty => G.Val ty -> State KProgState VarLit
kValue v = case v of
    G.VUnit -> return Unit
    G.VInt n -> return $ Int n
    G.VFloat n -> return $ Double n
    G.VBool n -> return $ Bool n
    G.VString n -> return $ String n
    G.VDict' xs -> do
        let CStruct def' = cTypeOf v
            (keys, ms) = unzip $ G.mapOnTypeDic
                (\key x -> (key, kValue x))
                xs
        name <- getStructName def'
        vs <- sequence ms
        return $ Struct name (zip keys vs)
    _ -> notImplemented v

getStructName :: StructDef -> State KProgState String
getStructName def' = uses kstructs (M.lookup def') >>= \case
    Nothing -> do
      n <- ("s"++) . show <$> (kstructsCount <+= 1)
      kstructs %= M.insert def' n
      return n
    Just n -> return n

bind :: CType -> KExp -> State KProgState VarLit
bind t e = do
    x <- genNewVar t
    addInst (x := e)
    return $ V x

genNewVar :: CType -> State KProgState Var
genNewVar t = do
    i <- kblock.kvarCount <+= 1
    return $ TmpVar
      { varId   = i
      , varName = mkTmpVarName i
      , varType = t
      , alreadyDeclared = False
      }

addInst :: Instruction -> State KProgState ()
addInst inst = kblock.kinstructions %= (inst:)

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

data CompileEnv = CompileEnv
  { _structNameMap :: Map StructDef String
  }
makeLenses ''CompileEnv

newtype Compile a = Compile { unCompile :: Reader CompileEnv a }
  deriving (Functor, Applicative, Monad, MonadReader CompileEnv)

initialEnv :: CompileEnv
initialEnv = CompileEnv
  { _structNameMap = M.empty
  }

compileKProg :: Int -> KProg -> Doc
compileKProg n p =
    PP.vcat
      [ PP.text "#include \"mylib.h\""
      , declareStruct
      , main
      ]
  where
    env = CompileEnv
          { _structNameMap = kprogStructs p
          }
    main = runReader (unCompile (compileKProgM n p)) env
    structs' = M.toList (kprogStructs p)
    declareStruct =
      PP.vcat $ flip map structs' $ \(StructDef defs, name) ->
        runReader `flip` env $ unCompile $ vcat $ sequence
          [ text "struct" <+> text name
          , withBrace (
              vcat $ forM defs $ \(x,ty) ->
                let v = TmpVar 0 x ty False
                in pDeclareVar v
            ) <> pure PP.semi
          ]

compileKProgM :: Int -> KProg -> Compile Doc
compileKProgM numLoop KProg{..} = vcat $ sequence
    [ text "int main()"
    , withBrace $ vcat $ sequence
        [ vcat $ mapM prettyComInit xs
        , text "int counter = 0;"
        , text "while(1)"
        , withBrace $ vcat $ sequence
            [ result
            , vcat $ mapM prettyComStep xs
            , vcat $ mapM prettyUpdate xs
            --, text "sleep(1);"
            , text "counter++;"
            , text $ "if (counter>=" ++ show numLoop ++ ") break;"
            ]
        ]
    ]
  where
    xs = map snd $ IntMap.toList kprogRegisters
    result = withBrace $ vcat $ sequence $
        map pInstruction insts
        ++
        [ pInstruction (resVar := e)
        , printResIfPossible
        ]
      where
        KBlock insts e = kprogExp
        resVar = TmpVar (-1) "res" kprogType False
        printResIfPossible = case kprogType of
          CInt -> sentence $
            function "printf"
            [ text "\"%d\\n\""
            , pVar resVar
            ]
          CString -> sentence $
            function "printf"
            [ text "\"%s\\n\""
            , pStrVarBody resVar
            ]
          _ -> return mempty

pType :: CType -> Compile Doc
pType = \case
    CVoid -> text "void"
    CInt -> text "int"
    CBool -> text "bool"
    CDouble -> text "double"
    CString -> text "String"
    CStruct s -> do
      name <- views structNameMap (fromJust . M.lookup s)
      text "struct" <+> text name
    t -> notImplemented t

pVar :: Var -> Compile Doc
pVar = text . varName

pVarPtr :: Var -> Compile Doc
pVarPtr x = text "&" <> pVar x

pStrVarBody :: Var -> Compile Doc
pStrVarBody x
  | varType x == CString = pVar x <> text ".buf"
  | otherwise = error $ "pStrVarBody: " ++ show x

pStrVarSize :: Var -> Compile Doc
pStrVarSize x
  | varType x == CString = pVar x <> text ".size"
  | otherwise = error $ "pStrVarBody: " ++ show x

pVarLit :: VarLit -> Compile Doc
pVarLit = \case
    V v       -> pVar v
    Int n     -> return $ pPrint n
    Double f  -> return $ pPrint f
    Bool b    -> text $ if b then "true" else "false"
    String s  -> text (show s)
    Struct name body ->
      let pBody = forM body $ \(field,val) ->
                    text ("."++field) <+> text "=" <+> pVarLit val
      in parens (text "struct" <+> text name)
            <+> (braces . cat $ punctuate PP.comma pBody)
    Unit      -> error "Pretty VarLit: Unit"

pKExp :: KExp -> Compile Doc
pKExp = \case
    VarLit vl   -> pVarLit vl
    Plus  e1 e2 -> ope "+" e1 e2
    Minus e1 e2 -> ope "-" e1 e2
    Times e1 e2 -> ope "*" e1 e2
    Mod   e1 e2 -> ope "%" e1 e2
    Eq _  e1 e2 -> ope "==" e1 e2
    --
    Member (V x) member -> pVar x <> text "." <> text member
    e           -> notImplemented e
  where
    ope s e1 e2 = pVarLit e1 <+> text s <+> pVarLit e2

pDeclareVar :: Var -> Compile Doc
pDeclareVar x = sentences [ pType t <+> pVar x ]
             <+> initialize
  where
    t = varType x
    initialize = case t of
      CString -> sentence $
        function "InitializeString"
          [ pVarPtr x
          , text "NULL"
          ]
      _ -> return mempty

pDeclareVarWith :: Var -> VarLit -> Compile Doc
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
      t -> sentence $ pType t <+> pVar x <+> text "=" <+> pVarLit v0

pBindKExp :: Var -> KExp -> Compile Doc
pBindKExp x e = case (varType x, e) of
  (_, If v b1 b2) -> vcat $ sequence
      [ text "if" <> parens (pVarLit v)
      , pBindKBlock x b1
      , text "else"
      , pBindKBlock x b2
      ]
  -- 次の２つのケースを合わせたい．
  (CString, VarLit valStr) -> sentence $
      function "BindString"
        [ pVar x
        , case valStr of
            V y -> pStrVarBody y
            String s -> text (show s)
            _ -> undefined
            -- TODO
        ]
  (CString, Show valInt) -> sentence $
      function "snprintf"
        [ pStrVarBody x
        , pStrVarSize x
        , text (show "%d")
        , case valInt of
            V y   -> pVar y
            Int n -> text (show n)
            _ -> undefined
            -- TODO
        ]
  _ -> sentence $ pVar x <+> text "=" <+> pKExp e

pBindKBlock :: Var -> KBlock -> Compile Doc
pBindKBlock x (KBlock inst e) =
    withBrace $ vcat $ mapM pInstruction (inst ++ [x := e])

pInstruction :: Instruction -> Compile Doc
pInstruction (x := e)
  | isDeclared x = pBindKExp x e
  | otherwise    = pDeclareVar x $+$ pBindKExp (declared x) e

prettyComInit :: KCom -> Compile Doc
prettyComInit (KCom x v0 _)
  | varType x == CVoid = return mempty
  | otherwise = pDeclareVarWith x v0
            $+$ pDeclareVar (x { varName = varName x ++ "_Next" })

prettyComStep :: KCom -> Compile Doc
prettyComStep (KCom x _ b) =
    pBindKBlock x { varName = varName x ++ "_Next" } b

prettyUpdate :: KCom -> Compile Doc
prettyUpdate (KCom x@GrobalVar{} _ _) =
    pBindKExp x (VarLit (V x { varName = varName x ++  "_Next" }))
prettyUpdate KCom{comId} = error $ "prettyUpdate: " ++ show comId

--------
-- Util
--------

sentence :: Monad m => m Doc -> m Doc
sentence = fmap (PP.<> PP.semi)

sentences :: Monad m => [m Doc] -> m Doc
sentences = vcat . mapM sentence

function :: Monad m => String -> [m Doc] -> m Doc
function f xs = aux <$> sequence xs
  where
    aux args = PP.text f PP.<> PP.parens (PP.hsep $ PP.punctuate PP.comma args)

