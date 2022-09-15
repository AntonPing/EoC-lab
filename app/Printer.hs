{-# LANGUAGE OverloadedStrings #-}
module Printer where

import Data.Text as T
import Syntax

import Prettyprinter as PP
import Prettyprinter.Render.Text as PPRT
import Interp (Value (VLit, VClos))

bracketed :: [Doc ann] -> Doc ann
bracketed =  PP.encloseSep "(" ")" " "

docRender :: Doc ann -> T.Text
docRender = renderStrict . layoutPretty defaultLayoutOptions

instance Show Literal where
    show e = T.unpack $ docRender (pretty e)

instance Show Prim where
    show e = T.unpack $ docRender (pretty e)

instance Show EffPrim where
    show e = T.unpack $ docRender (pretty e)

instance Pretty a => Show (Expr a) where
    show e = T.unpack $ docRender (pretty e)

instance Pretty a => Show (Value a) where
    show e = T.unpack $ docRender (pretty e)

-- instance Pretty Name where

instance Pretty Literal where
    pretty (LInt n) = pretty n
    pretty (LReal x) = pretty x
    pretty (LBool p) = if p then "true" else "false"
    pretty (LChar c) = pretty c
    pretty LUnit = "()"

instance Pretty Prim where
    pretty IAdd = "iadd"
    pretty ISub = "isub"
    pretty INeg = "ineg"

instance Pretty EffPrim where
    pretty IOReadInt = "read-int"
    pretty IOWriteInt = "write-int"

instance Pretty a => Pretty (Expr a) where
    pretty (ELit b) = pretty b
    pretty (EVar x) = pretty x
    pretty (ELam xs e) =
        "(λ." <+> PP.sep (fmap pretty xs) <+> "->" <+> pretty e <> ")"
    pretty (EApp es) =
        PP.encloseSep "(" ")" " " (fmap pretty es)
    pretty (EOpr prim args) =
        PP.encloseSep "(" ")" " " (pretty prim: fmap pretty args)
    pretty (EEff prim args) =
        PP.encloseSep "(" ")" " " (pretty prim: fmap pretty args)
    pretty (ELet x e1 e2) =
        "todo: let"
        --PP.encloseSep "(" ")" " " (fmap pretty es)
    {-
    pretty t@ELet{} =
        "let" <+> PP.vsep xs <+> PP.softline <+> "in" <+> pretty t
        where
            (xs',t') = letUnfold t
            xs = fmap (\(x,y) -> pretty x <+> "=" <+> pretty y) xs'
    -}
instance Pretty a => Pretty (Value a) where
    pretty (VLit lit) = pretty lit
    pretty VClos {} = "<closure>"

{-
instance Pretty LitType where
    pretty TInt = "Int"
    pretty TReal = "Real"
    pretty TBool = "Bool"
    pretty TUnit = "Unit"
    pretty TVoid = "Void"

instance Pretty Kind where
    pretty Star = "*"
    pretty (KArr k1 k2) = pretty k1 <+> "->" <+> pretty k2

instance Pretty Type where
    pretty (TVar x) = pretty x
    pretty (TLam x k t) =
        "(Λ." <+> pretty x <+> ":" <+> pretty k <+> "->" <+> pretty t <> ")"
        -- todo : fold and unfold for TLam
    pretty (TApp t1 t2) = "(" <+> pretty t1 <+> pretty t2 <+> ")"
    pretty (TLit lit) = pretty lit
    pretty t@TArr{} = PP.encloseSep "(" ")" " -> "
        (fmap pretty (tyArrUnfold t))
    pretty (TTup xs) = PP.encloseSep "(" ")" "," (fmap pretty xs)
    pretty (TSum xs) = PP.encloseSep "<" ">" "," (fmap pretty xs)
    pretty (TForall xs ty) =
        "forall" <+> PP.sep (fmap pretty xs) <+> pretty ty
-}

{-

instance Pretty Pattern where
    pretty (PVar x) = pretty x
    pretty (PCon x xs) = P.encloseSep "(" ")" " "
                        (pretty x: fmap pretty xs)
    pretty (PTup xs) = tupled (fmap pretty xs)
    pretty (PLit lit) = pretty lit
    pretty PWild = "_"
-}