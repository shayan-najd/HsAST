{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsBinds where

import Dependencies
import HsExtension

import {-# SOURCE #-} HsExpr ( LHsExpr, MatchGroup, GRHSs)
import {-# SOURCE #-} HsPat  ( LPat )
import HsTypes

type LHsLocalBinds p = Located (HsLocalBinds p)
type HsLocalBinds p = HsLocalBindsLR p p

type LHsLocalBindsLR pL pR = Located (HsLocalBindsLR pL pR)
data HsLocalBindsLR pL pR
  = HsValBinds
        (XHsValBinds pL pR)
        (HsValBindsLR pL pR)
  | HsIPBinds
        (XHsIPBinds pL pR)
        (HsIPBinds pR)
  | EmptyLocalBinds (XEmptyLocalBinds pL pR)
  | XHsLocalBindsLR
        (XXHsLocalBindsLR pL pR)

type HsValBinds p = HsValBindsLR p p

data HsValBindsLR pL pR
  = ValBinds
        (XValBinds pL pR)
        (LHsBindsLR pL pR) [LSig pR]
  | XValBindsLR
      (XXValBindsLR pL pR)

type LHsBinds p = LHsBindsLR p p

type LHsBind  p = LHsBindLR  p p
type HsBind   p = HsBindLR   p p

-- | Located Haskell Bindings with separate Left and Right identifier types
type LHsBindsLR pL pR = Bag (LHsBindLR pL pR)

type LHsBindLR  pL pR = Located (HsBindLR pL pR)
data HsBindLR pL pR
  = FunBind { fun_ext :: XFunBind pL pR
            , fun_id :: LIdP pL
            , fun_matches :: MatchGroup pR (LHsExpr pR)
            {- TTG-Todo
            , fun_co_fn :: HsWrapper
            , fun_tick :: [Tickish p]
            -} }
  | PatBind { pat_ext    :: XPatBind pL pR
            , pat_lhs    :: LPat pL
            , pat_rhs    :: GRHSs pR (LHsExpr pR)
            {- TTG-todo
            , pat_ticks  :: ([Tickish p], [[Tickish p]])
            -} }
  | VarBind { var_ext    :: XVarBind pL pR
            , var_id     :: IdP pL
            , var_rhs    :: LHsExpr pR
            , var_inline :: Bool
            }
  {- TTG-Todo
  | AbsBinds {                      -- Binds abstraction; TRANSLATION
        abs_ext     :: XAbsBinds pL pR,
        abs_tvs     :: [TyVar],
        abs_ev_vars :: [EvVar],  -- ^ Includes equality constraints
        abs_exports :: [ABExport pL],
        abs_ev_binds :: [TcEvBinds],
        abs_binds    :: LHsBinds pL,
        abs_sig :: Bool  -- See Note [The abs_sig field of AbsBinds]
    }
  -}
  | PatSynBind (XPatSynBind pL pR)
               (PatSynBind pL pR)
  | XHsBindsLR (XXHsBindsLR pL pR)


data PatSynBind pL pR
  = PSB { psb_ext  :: XPSB pL pR,
          psb_id   :: LIdP pL,
          psb_args :: HsPatSynDetails (LIdP pR),
          psb_def  :: LPat pR,
          psb_dir  :: HsPatSynDir pR
     }
   | XPatSynBind (XXPatSynBind pL pR)

data HsIPBinds p
  = IPBinds
        (XIPBinds p)
        [LIPBind p]
  | XHsIPBinds (XXHsIPBinds p)

type LIPBind p = Located (IPBind p)
data IPBind p
  = IPBind
        (XCIPBind p)
        (Either (LHsIPName) (IdP p))
        (LHsExpr p)
  | XIPBind (XXIPBind p)

type LSig p = Located (Sig p)
data Sig p
  = TypeSig
       (XTypeSig p)
       [LIdP p]
       (LHsSigWcType p)
  | PatSynSig (XPatSynSig p) [LIdP p] (LHsSigType p)
  | ClassOpSig (XClassOpSig p) Bool [LIdP p] (LHsSigType p)
  {- TTG-Todo Not sure
  | IdSig (XIdSig p) Id
  -}
  | FixSig (XFixSig p) (FixitySig p)
  | InlineSig   (XInlineSig p)
                (LIdP p)
                InlinePragma
  | SpecSig     (XSpecSig p)
                (LIdP p)
                [LHsSigType p]
                InlinePragma
  | SpecInstSig (XSpecInstSig p)
                {- TTG-Todo
                SourceText
                -}
                (LHsSigType p)
  | MinimalSig (XMinimalSig p)
                {- TTG-Todo
                SourceText
                -}
               (LBooleanFormula (LIdP p))
  | SCCFunSig  (XSCCFunSig p)
                {- TTG-Todo
                SourceText
                -}
               (LIdP p)
               (Maybe (LStringLiteral))
  | CompleteMatchSig (XCompleteMatchSig p)
                     {- TTG-Todo
                     SourceText
                     -}
                     (Located [LIdP p]) -- TTG: todo: SrcLocs
                     (Maybe (LIdP p))
  | XSig (XXSig p)


type LFixitySig p = Located (FixitySig p)
data FixitySig p
  = FixitySig (XFixitySig p) [LIdP p] Fixity
  | XFixitySig (XXFixitySig p)

type HsPatSynDetails arg = HsConDetails arg [RecordPatSynField arg]

data RecordPatSynField a
  = RecordPatSynField {
      recordPatSynSelectorId :: a
      , recordPatSynPatVar :: a
      }

data HsPatSynDir p
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup p (LHsExpr p))
