{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsTypes where

import Dependencies
import HsExtension
import {-# SOURCE #-} HsExpr (HsSplice)
import HsDoc

type LHsIPName = Located HsIPName
newtype HsIPName = HsIPName FastString

type LBangType    p = Located (BangType p)
type BangType     p = HsType p

type LHsContext   p = Located (HsContext p)
type HsContext    p = [LHsType p]


type LHsKind      p = Located (HsKind p)
type HsKind       p = HsType p

data LHsQTyVars p
  = HsQTvs { hsq_ext :: XHsQTvs p
           , hsq_explicit :: [LHsTyVarBndr p]
            }
  | XLHsQTyVars (XXLHsQTyVars p)

data HsImplicitBndrs p thing
  = HsIB { hsib_ext  :: XHsIB p thing
         , hsib_body :: thing
         }
  | XHsImplicitBndrs (XXHsImplicitBndrs p thing)

data HsWildCardBndrs p thing
  = HsWC { hswc_ext :: XHsWC p thing
         , hswc_body :: thing
         }
  | XHsWildCardBndrs (XXHsWildCardBndrs p thing)

type LHsSigType   p = HsImplicitBndrs p (LHsType p)
type LHsSigWcType p = HsWildCardBndrs p (LHsSigType p)

type LHsTyVarBndr p = Located (HsTyVarBndr p)
data HsTyVarBndr p
  = UserTyVar
         (XUserTyVar p)
         (LIdP p) -- LIP
  | KindedTyVar
         (XKindedTyVar p)
         (LIdP p) --LIP
         (LHsKind p)
  | XTyVarBndr
      (XXTyVarBndr p)

type LHsType p = Located (HsType p)
data HsType p
  = HsForAllTy
      { hst_xforall :: XForAllTy p,
        hst_bndrs   :: [LHsTyVarBndr p]
      , hst_body    :: LHsType p
      }
  | HsQualTy
      { hst_xqual :: XQualTy p
      , hst_ctxt  :: LHsContext p
      , hst_body  :: LHsType p }
  | HsTyVar             (XTyVar p)
                        Promoted
                        (LIdP p)
  | HsAppTy             (XAppTy p)
                        (LHsType p)
                        (LHsType p)
  | HsFunTy             (XFunTy p)
                        (LHsType p)
                        (LHsType p)
  | HsListTy            (XListTy p)
                        (LHsType p)
  | HsTupleTy           (XTupleTy p)
                        HsTupleSort
                        [LHsType p]
  | HsSumTy             (XSumTy p)
                        [LHsType p]
  | HsOpTy              (XOpTy p)
                        (LHsType p)
                        (LIdP p)
                        (LHsType p)
  | HsParTy             (XParTy p)
                        (LHsType p)
  | HsIParamTy          (XIParamTy p)
                        (LHsIPName)
                        (LHsType p)
  | HsStarTy            (XStarTy p)
                        Bool
  | HsKindSig           (XKindSig p)
                        (LHsType p)
                        (LHsKind p)
  | HsSpliceTy          (XSpliceTy p)
                        (HsSplice p)
  | HsDocTy             (XDocTy p)
                        (LHsType p)
                        LHsDocString
  | HsBangTy            (XBangTy p)
                        HsSrcBang
                        (LHsType p)
  | HsRecTy             (XRecTy p)
                        [LConDeclField p]
  | HsExplicitListTy    (XExplicitListTy p)
                        Promoted
                        [LHsType p]
  | HsExplicitTupleTy   (XExplicitTupleTy p)
                        [LHsType p]
  | HsTyLit             (XTyLit p)
                        HsTyLit
  | HsWildCardTy        (XWildCardTy p)
  | XHsType             (XXType p)

data HsTyLit
  = HsNumTy {- TTG-Todo
            SourceText
            -}
            Integer
  | HsStrTy {- TTG-Todo
            SourceText
            -}
            FastString


data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple

data Promoted = Promoted
              | NotPromoted

type LConDeclField p = Located (ConDeclField p)
data ConDeclField p
  = ConDeclField { cd_fld_ext  :: XConDeclField p,
                   cd_fld_names :: [LFieldOcc p],
                   cd_fld_type :: LBangType p,
                   cd_fld_doc  :: Maybe LHsDocString }
  | XConDeclField (XXConDeclField p)

data HsConDetails arg rec
  = PrefixCon [arg]
  | RecCon    rec
  | InfixCon  arg arg

type LFieldOcc p = Located (FieldOcc p)
data FieldOcc p
  = FieldOcc { extFieldOcc     :: XCFieldOcc p
             , rdrNameFieldOcc :: LIdP p -- TTG-Todo new
             {- TTG-Todo
             , rdrNameFieldOcc :: Located RdrName
             -}
             }
  | XFieldOcc
      (XXFieldOcc p)

-- TTG-Todo new
data AmbiguousFieldOcc p
  = Unambiguous (Located p) -- TTG: todo: SrcLocs
  | Ambiguous   (Located p) -- TTG: todo: SrcLocs

{- TTG-Todo
data AmbiguousFieldOcc p
  = Unambiguous (XUnambiguous p) (Located RdrName)
  | Ambiguous   (XAmbiguous p)   (Located RdrName)
  | XAmbiguousFieldOcc (XXAmbiguousFieldOcc p)
-}
