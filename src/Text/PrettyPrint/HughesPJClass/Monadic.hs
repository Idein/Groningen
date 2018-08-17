
module Text.PrettyPrint.HughesPJClass.Monadic
  ( Doc
  , Pretty(..)
  , text
  , cat
  , punctuate
  , vcat
  , parens
  , braces
  , withBrace
  , (<>)
  , (<+>)
  , ($+$)
  ) where

import           Text.PrettyPrint.HughesPJClass (Doc, Pretty(..))
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Control.Applicative            (liftA2)

cat :: Monad m => m [Doc] -> m Doc
cat xs = PP.cat <$> xs

punctuate :: Monad m => Doc -> m [Doc] -> m [Doc]
punctuate sep xs = PP.punctuate sep <$> xs

vcat :: Monad m => m [Doc] -> m Doc
vcat xs = PP.vcat <$> xs

braces :: Monad m => m Doc -> m Doc
braces = fmap PP.braces

withBrace :: Monad m => m Doc -> m Doc
withBrace x = do
  d <- x
  return $ PP.lbrace PP.$+$ PP.nest 4 d PP.$+$ PP.rbrace

text :: Monad m => String -> m Doc
text = return . PP.text

(<+>) :: Monad m => m Doc -> m Doc -> m Doc
(<+>) = liftA2 (PP.<+>)

(<>) :: Monad m => m Doc -> m Doc -> m Doc
(<>) = liftA2 (PP.<>)

($+$) :: Monad m => m Doc -> m Doc -> m Doc
($+$) = liftA2 (PP.$+$)

parens :: Monad m => m Doc -> m Doc
parens = fmap PP.parens

