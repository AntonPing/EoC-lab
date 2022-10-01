{-# LANGUAGE OverloadedStrings #-}
module Printer where

import Data.Text as T
import Syntax

import Prettyprinter as PP
import Prettyprinter.Render.Text as PPRT
import Prettyprinter.Render.String (renderString)

-- import Interp

--bracketed :: [Doc ann] -> Doc ann
--bracketed =  PP.encloseSep "(" ")" " "

--tupled :: [Doc ann] -> Doc ann
--tupled =  PP.encloseSep "(" ")" ","

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

instance Pretty ty => Show (Expr ty) where
    show e = T.unpack $ docRender (pretty e)

instance Show MonoType where
    show e = T.unpack $ docRender (pretty e)

instance Show PolyType where
    show e = T.unpack $ docRender (pretty e)

instance Show Type where
    show e = T.unpack $ docRender (pretty e)

instance Show Value where
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
    pretty Move = "move"

instance Pretty a => Pretty (Def a) where
    pretty Def{ func, args, body } =
        pretty func <> PP.tupled (fmap pretty args) <+> "=" <+> pretty body <> ";"

instance Pretty a => Pretty (Expr a) where
    pretty (ELit b) = pretty b
    pretty (EVar x) = pretty x
    pretty (EFun xs body) =
        "(fn" <+> PP.tupled (fmap pretty xs) <+> "=>" <+> pretty body <> ")"
    pretty (EApp func args) =
        pretty func <> PP.tupled (fmap pretty args)
    pretty (EOpr prim args) =
        pretty prim <> PP.tupled (fmap pretty args)
    pretty (ELet x ty e1 e2) =
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
    pretty (EAnno expr ty) =
        pretty expr <+> ":" -- <+> pretty ty

instance Pretty LitType where
    pretty LitInt = "Int"
    pretty LitReal = "Real"
    pretty LitBool = "Bool"
    pretty LitChar = "Char"
    pretty LitUnit = "Unit"
    pretty LitVoid = "Void"

instance Pretty MonoType where
    pretty (TLit lit) = pretty lit
    pretty (TVar x) = pretty x
    pretty (TCon x) = pretty x
    pretty (TFun xs x) =
        "fn" <+> PP.encloseSep "(" ")" "," (fmap pretty xs) <+> "->" <+> pretty x
    pretty (TApp x xs) =
        pretty x <> PP.encloseSep "<" ">" "," (fmap pretty xs)

instance Pretty PolyType where
    pretty (PolyType [] ty) =
        pretty ty
    pretty (PolyType xs ty) =
        "forall" <+> PP.sep (fmap pretty xs) <> "." <> pretty ty

instance Pretty Type where
    pretty (Mono ty) = pretty ty
    pretty (Poly ty) = pretty ty

{-
instance Pretty Kind where
    pretty Star = "*"
    pretty (KArr k1 k2) = pretty k1 <+> "->" <+> pretty k2
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

instance Pretty Value where
    pretty (VLit lit) = pretty lit
    pretty (VClos _ _ _) = "<closure>"
    pretty (VFix _ _ _ _) = "<fixed closure>"

instance Pretty InferError where
    pretty (CantUnify a b) = "Can't Unify" <+> pretty a <+> "and" <+> pretty b <> "!"
    pretty (VarNotInScope x) = "Variable" <+> pretty x <+> "is not in scope!"
