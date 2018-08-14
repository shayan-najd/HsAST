{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsPat  where

import Dependencies
import HsExtension
import HsLit
import {-# SOURCE #-} HsExpr
import HsTypes

type LPat p   = Located (Pat p)
data Pat p
  = WildPat     (XWildPat p)
  | VarPat      (XVarPat p)
                (Located (IdP p))
  | LazyPat     (XLazyPat p)
                (LPat p)
  | AsPat       (XAsPat p)
                (Located (IdP p)) (LPat p)
  | ParPat      (XParPat p)
                (LPat p)
  | BangPat     (XBangPat p)
                (LPat p)
  | ListPat     (XListPat p)
                [LPat p]
  | TuplePat    (XTuplePat p)
                [LPat p]
                Boxity
  | SumPat      (XSumPat p)
                (LPat p)
                ConTag
                Arity
  | ConPatIn    (Located (IdP p))
                (HsConPatDetails p)
  {- TTG-TODO
  | ConPatOut   (Located ConLike),
                [Type]
                [TyVar]
                [EvVar]
                TcEvBinds
                (HsConPatDetails p)
                HsWrapper
  -}
  | ViewPat     (XViewPat p)
                (LHsExpr p)
                (LPat p)

  | SplicePat   (XSplicePat p)
                (HsSplice p)
  | LitPat      (XLitPat p)
                (HsLit p)
  {- TTG-TODO ShNajd: Not sure
  | NPat        (XNPat p)
                (Located (HsOverLit p))
                (Maybe (SyntaxExpr p))
                (SyntaxExpr p)
  -}
  | NPlusKPat   (XNPlusKPat p)
                (Located (IdP p))
                (Located (HsOverLit p))
                (HsOverLit p)
                {- TTG-TODO
                (SyntaxExpr p)
                (SyntaxExpr p)
                -}
  | SigPat      (XSigPat p)
                (LPat p)
  {- TTG-TODO
  | CoPat       (XCoPat p)
                HsWrapper
                (Pat p)
                Type
  -}
  | XPat        (XXPat p)

type HsConPatDetails p = HsConDetails (LPat p) (HsRecFields p (LPat p))

data HsRecFields p arg
  = HsRecFields { rec_flds   :: [LHsRecField p arg],
                  rec_dotdot :: Maybe Int }


type LHsRecField    p arg = Located (HsRecField  p arg)
type HsRecField     p arg = HsRecField' (FieldOcc p) arg

type LHsRecUpdField p     = Located (HsRecUpdField p)
type HsRecUpdField  p     = HsRecField' (AmbiguousFieldOcc p) (LHsExpr p)

type LHsRecField'   p arg = Located (HsRecField' p arg)
data HsRecField' id arg
  = HsRecField (Located id) arg Bool -- TTG: todo: SrcLocs
