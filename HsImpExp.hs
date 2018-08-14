{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsImpExp where

import Dependencies
import HsExtension
import HsDoc

type LImportDecl p = Located (ImportDecl p)
data ImportDecl p
  = ImportDecl {
      ideclExt       :: XCImportDecl p,
      {- TTG-Todo
      ideclSourceSrc :: SourceText,
      -}
      ideclName      :: Located ModuleName, -- TTG: todo: SrcLocs
      ideclPkgQual   :: Maybe StringLiteral,
      ideclSource    :: Bool,
      ideclSafe      :: Bool,
      ideclQualified :: Bool,
      ideclImplicit  :: Bool,
      ideclAs        :: Maybe (Located ModuleName), -- TTG: todo: SrcLocs
      ideclHiding    :: Maybe (Bool, Located [LIE p]) -- TTG: todo: SrcLocs
    }
  | XImportDecl (XXImportDecl p)

type LIEWrappedName name = Located (IEWrappedName name)
data IEWrappedName name      -- TTG: todo: change parameter
  = IEName    (Located name) -- TTG: todo: SrcLocs
  | IEPattern (Located name)
  | IEType    (Located name)


type LIE p = Located (IE p)
data IE p
  = IEVar            (XIEVar p)
                     (LIEWrappedName (IdP p))
  | IEThingAbs       (XIEThingAbs p)
                     (LIEWrappedName (IdP p))
  | IEThingAll       (XIEThingAll p)
                     (LIEWrappedName (IdP p))
  | IEThingWith      (XIEThingWith p)
                     (LIEWrappedName (IdP p))
                     IEWildcard
                     [LIEWrappedName (IdP p)]
                     [Located (FieldLbl (IdP p))] -- TTG: todo: SrcLocs
  | IEModuleContents (XIEModuleContents p)
                     (Located ModuleName) -- TTG: todo: SrcLocs
  | IEGroup          (XIEGroup p)
                     Int
                     HsDocString
  | IEDoc            (XIEDoc p)
                     HsDocString
  | IEDocNamed       (XIEDocNamed p)
                     String
  | XIE              (XXIE p)

data IEWildcard
  = NoIEWildcard
  | IEWildcard Int
