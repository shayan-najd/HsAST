{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsLit where

import HsExtension
import Data.ByteString (ByteString)
import Dependencies

data HsLit x
  = HsChar       (XHsChar       x) Char
  | HsCharPrim   (XHsCharPrim   x) Char
  | HsString     (XHsString     x) FastString
  | HsStringPrim (XHsStringPrim x) ByteString
  {- TTG-TODO
  | HsInt (XHsInt x)  IntegralLit
  -}
  | HsIntPrim    (XHsIntPrim    x) Integer
  | HsWordPrim   (XHsWordPrim   x) Integer
  | HsInt64Prim  (XHsInt64Prim  x) Integer
  | HsWord64Prim (XHsWord64Prim x) Integer
  {- TTG-TODO
  | HsInteger (XHsInteger x)  Integer Type
  | HsRat (XHsRat x)  FractionalLit Type
  -}
  | HsFloatPrim  (XHsFloatPrim  x) FractionalLit
  | HsDoublePrim (XHsDoublePrim x) FractionalLit
  | XLit (XXLit x)

data HsOverLit x
  = OverLit  (XOverLit x) OverLitVal
  {- TTG-TODO (HsExpr p) -}
  | XOverLit (XXOverLit x)

data OverLitVal
  = HsIntegral   !IntegralLit
  | HsFractional !FractionalLit
  | HsIsString   {-  TTG-TODO !SourceText -}
                 !FastString
