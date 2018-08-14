{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}
module ForeignCall where

import Dependencies

newtype ForeignCall = CCall CCallSpec

data Safety
  = PlaySafe
  | PlayInterruptible
  | PlayRisky

data CExportSpec
  = CExportStatic {- TTG-Todo
                  SourceText
                  -}
                  CLabelString
                  CCallConv

data CCallSpec
  =  CCallSpec CCallTarget
               CCallConv
               Safety

data CCallTarget
  = StaticTarget {- TTG-Todo
                 SourceText
                 -}
                 CLabelString
                 (Maybe UnitId)
                 Bool
  | DynamicTarget

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv

type CLabelString = FastString

data Header
  = Header {- TTG-Todo
           SourceText
           -}
           FastString

data CType
  = CType {- TTG-Todo
           SourceText
           -}
          (Maybe Header)
          FastString
          {- TTG-Todo
          (SourceText,FastString)
          -}
