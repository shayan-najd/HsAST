{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsSyn (
        module HsBinds,
        module HsDecls,
        module HsExpr,
        module HsImpExp,
        module HsLit,
        module HsPat,
        module HsTypes,
        module HsDoc,
        module HsExtension,
        Fixity,
        HsModule(..),
) where

-- friends:
import Dependencies

import HsDecls
import HsBinds
import HsExpr
import HsImpExp
import HsLit
import HsExtension
import HsPat
import HsTypes
import HsDoc

data HsModule p
  = HsModule { hsmodName :: Maybe (Located ModuleName) -- TTG: todo: SrcLocs
             , hsmodExports :: Maybe (Located [LIE p]) -- TTG: todo: SrcLocs
             , hsmodImports :: [LImportDecl p]
             , hsmodDecls :: [LHsDecl p]
             , hsmodDeprecMessage :: Maybe (Located WarningTxt) -- TTG: todo: SrcLocs
             , hsmodHaddockModHeader :: Maybe LHsDocString }
