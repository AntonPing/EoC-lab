{-# LANGUAGE OverloadedStrings #-}
module Printer where

import Data.Text as T
import Syntax

import Prettyprinter as PP
import Prettyprinter.Render.Text as PPRT
import Text.Parsec (newline)
import Prettyprinter.Render.String (renderString)
-- import Interp

bracketed :: [Doc ann] -> Doc ann
bracketed =  PP.encloseSep "(" ")" " "

tupled :: [Doc ann] -> Doc ann
tupled =  PP.encloseSep "(" ")" " "

docRender :: Doc ann -> T.Text
docRender = renderStrict . layoutPretty defaultLayoutOptions

docRenderString :: Doc ann -> String
docRenderString = renderString . layoutPretty defaultLayoutOptions

prettyPrint :: Pretty a => a -> IO ()
prettyPrint x = putStr $ docRenderString $ pretty x

prettyPrintLn :: Pretty a => a -> IO ()
prettyPrintLn x = putStrLn $ docRenderString $ pretty x

instance Show Literal where
    show e = T.unpack $ docRender (pretty e)

instance Show Prim where
    show e = T.unpack $ docRender (pretty e)

instance Pretty a => Show (Expr a) where
    show e = T.unpack $ docRender (pretty e)

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
    pretty (ICmp Eq) = "cmpeq"
    pretty (ICmp Ne) = "cmpne"
    pretty (ICmp Gr) = "cmpgr"
    pretty (ICmp Ls) = "cmpls"
    pretty (ICmp Ge) = "cmpge"
    pretty (ICmp Le) = "cmple"
    pretty BNot = "bnot"
    pretty BAnd = "band"
    pretty BOr = "bor"
    pretty BXor = "bxor"
    pretty IOReadInt = "read-int"
    pretty IOWriteInt = "write-int"

instance Pretty a => Pretty (Def a) where
    pretty Def{ func, args, body } =
        pretty func <> PP.tupled (fmap pretty args) <+> "=" <+> pretty body <> ";"

instance Pretty a => Pretty (Expr a) where
    pretty (ELit b) = pretty b
    pretty (EVar x) = pretty x
    pretty (ELam xs body) =
        "(fn" <+> PP.tupled (fmap pretty xs) <+> "=>" <+> pretty body <> ")"
    pretty (EApp func args) =
        pretty func <> PP.tupled (fmap pretty args)
    pretty (EOpr prim args) =
        pretty prim <> PP.tupled (fmap pretty args)
    pretty (ELet x e1 e2) =
        "let" <+> pretty x <+> "=" <+> pretty e1 <> ";" <> PP.hardline <>
        align (pretty e2)
    pretty (EFix defs e2) =
        "letrec" <> PP.hardline <>
            indent 2 (PP.vsep (fmap pretty defs)) <> PP.hardline <>
        "in" <> PP.hardline <>
            indent 2 (pretty e2) <> PP.hardline <>
        "end" <> PP.hardline
    pretty (EIfte cond trbr flbr) =
        "if" <+> pretty cond <> PP.softline <>
        "then" <+> pretty trbr <> PP.softline <>
        "else" <+> pretty flbr <> PP.softline

{-
instance Pretty a => Pretty (Value a) where
    pretty (VLit lit) = pretty lit
    pretty (VClos _ _ _) = "<closure>"
    pretty (VFix _ _ _ _) = "<fixed closure>"
-}

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