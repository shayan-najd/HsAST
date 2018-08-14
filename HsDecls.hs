{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsDecls where

import Dependencies
import HsExtension
import {-# SOURCE #-} HsExpr
import HsDoc
import HsTypes
import HsBinds
import ForeignCall

type LHsDecl p = Located (HsDecl p)
data HsDecl p
  = TyClD      (XTyClD p)      (TyClDecl p)
  | InstD      (XInstD p)      (InstDecl  p)
  | DerivD     (XDerivD p)     (DerivDecl p)
  | ValD       (XValD p)       (HsBind p)
  | SigD       (XSigD p)       (Sig p)
  | DefD       (XDefD p)       (DefaultDecl p)
  | ForD       (XForD p)       (ForeignDecl p)
  | WarningD   (XWarningD p)   (WarnDecls p)
  | AnnD       (XAnnD p)       (AnnDecl p)
  | RuleD      (XRuleD p)      (RuleDecls p)
  | SpliceD    (XSpliceD p)    (SpliceDecl p)
  | DocD       (XDocD p)       (DocDecl)
  | RoleAnnotD (XRoleAnnotD p) (RoleAnnotDecl p)
  | XHsDecl    (XXHsDecl p)

data HsGroup p
  = HsGroup {
        hs_ext    :: XCHsGroup p,
        hs_valds  :: HsValBinds p,
        hs_splcds :: [LSpliceDecl p],
        hs_tyclds :: [TyClGroup p],
        hs_derivds :: [LDerivDecl p],
        hs_fixds  :: [LFixitySig p],
        hs_defds  :: [LDefaultDecl p],
        hs_fords  :: [LForeignDecl p],
        hs_warnds :: [LWarnDecls p],
        hs_annds  :: [LAnnDecl p],
        hs_ruleds :: [LRuleDecls p],
        hs_docs   :: [LDocDecl]
    }
  | XHsGroup (XXHsGroup p)

type LSpliceDecl p = Located (SpliceDecl p)
data SpliceDecl p
  = SpliceDecl
        (XSpliceDecl p)
        (LHsSplice p)
        SpliceExplicitFlag
  | XSpliceDecl (XXSpliceDecl p)

type LTyClDecl p = Located (TyClDecl p)
data TyClDecl p
  = FamDecl { tcdFExt :: XFamDecl p
            , tcdFam :: FamilyDecl p }
  | SynDecl { tcdSExt   :: XSynDecl p
            , tcdLName  :: LIdP p
            , tcdTyVars :: LHsQTyVars p
            , tcdFixity :: LexicalFixity
            , tcdRhs    :: LHsType p }
  | DataDecl { tcdDExt     :: XDataDecl p
             , tcdLName    :: LIdP p
             , tcdTyVars   :: LHsQTyVars p
             , tcdFixity   :: LexicalFixity
             , tcdDataDefn :: HsDataDefn p }
  | ClassDecl { tcdCExt    :: XClassDecl p,
                tcdCtxt    :: LHsContext p,
                tcdLName   :: LIdP p,
                tcdTyVars  :: LHsQTyVars p,
                tcdFixity  :: LexicalFixity,
                tcdFDs     :: [LFunDep (LIdP p)], --TTG: todo: fix parameter
                tcdSigs    :: [LSig p],
                tcdMeths   :: LHsBinds p,
                tcdATs     :: [LFamilyDecl p],
                tcdATDefs  :: [LTyFamDefltEqn p],
                tcdDocs    :: [LDocDecl] }
  | XTyClDecl (XXTyClDecl p)

data TyClGroup p
  = TyClGroup { group_ext    :: XCTyClGroup p
              , group_tyclds :: [LTyClDecl p]
              , group_roles  :: [LRoleAnnotDecl p]
              , group_instds :: [LInstDecl p] }
  | XTyClGroup (XXTyClGroup p)

type LFamilyResultSig p = Located (FamilyResultSig p)
data FamilyResultSig p
  = NoSig (XNoSig p)
  | KindSig  (XCKindSig p) (LHsKind p)
  | TyVarSig (XTyVarSig p) (LHsTyVarBndr p)
  | XFamilyResultSig (XXFamilyResultSig p)


type LFamilyDecl p = Located (FamilyDecl p)
data FamilyDecl p
  = FamilyDecl { fdExt            :: XCFamilyDecl p
               , fdInfo           :: FamilyInfo p
               , fdLName          :: LIdP p
               , fdTyVars         :: LHsQTyVars p
               , fdFixity         :: LexicalFixity
               , fdResultSig      :: LFamilyResultSig p
               , fdInjectivityAnn :: Maybe (LInjectivityAnn p) }
  | XFamilyDecl (XXFamilyDecl p)


type LInjectivityAnn p = Located (InjectivityAnn p)
data InjectivityAnn p
  = InjectivityAnn (LIdP p) [LIdP p]

data FamilyInfo p
  = DataFamily
  | OpenTypeFamily
  | ClosedTypeFamily (Maybe [LTyFamInstEqn p])


data HsDataDefn p
  = HsDataDefn { dd_ext    :: XCHsDataDefn p,
                 dd_ND     :: NewOrData,
                 dd_ctxt   :: LHsContext p,
                 dd_cType  :: Maybe (Located CType), -- TTG: todo: SrcLocs
                 dd_kindSig:: Maybe (LHsKind p),
                 dd_cons   :: [LConDecl p],
                 dd_derivs :: HsDeriving p
               }
  | XHsDataDefn (XXHsDataDefn p)

type HsDeriving p = Located [LHsDerivingClause p]

type LHsDerivingClause p = Located (HsDerivingClause p)
data HsDerivingClause p
  = HsDerivingClause
    { deriv_clause_ext :: XCHsDerivingClause p
    , deriv_clause_strategy :: Maybe (LDerivStrategy p)
    , deriv_clause_tys :: Located [LHsSigType p] -- TTG: todo: SrcLocs
    }
  | XHsDerivingClause (XXHsDerivingClause p)

data NewOrData
  = NewType
  | DataType

type LConDecl p = Located (ConDecl p)
data ConDecl p
  = ConDeclGADT
      { con_g_ext   :: XConDeclGADT p
      , con_names   :: [LIdP p]
      , con_forall  :: Located Bool -- TTG: todo: SrcLocs
      , con_qvars   :: LHsQTyVars p
      , con_mb_cxt  :: Maybe (LHsContext p)
      , con_args    :: HsConDeclDetails p
      , con_res_ty  :: LHsType p
      , con_doc     :: Maybe LHsDocString
      }
  | ConDeclH98
      { con_ext     :: XConDeclH98 p
      , con_name    :: LIdP p
      , con_forall  :: Located Bool -- TTG: todo: SrcLocs
      , con_ex_tvs :: [LHsTyVarBndr p]
      , con_mb_cxt :: Maybe (LHsContext p)
      , con_args   :: HsConDeclDetails p
      , con_doc       :: Maybe LHsDocString
      }
  | XConDecl (XXConDecl p)

type HsConDeclDetails p
   = HsConDetails (LBangType p) (Located [LConDeclField p]) -- TTG: todo: SrcLocs

type LTyFamInstEqn p = Located (TyFamInstEqn p)
type TyFamInstEqn p = FamInstEqn p (LHsType p)

type HsTyPats p = [LHsType p]

type LTyFamDefltEqn p = Located (TyFamDefltEqn p)
type TyFamDefltEqn p = FamEqn p (LHsQTyVars p) (LHsType p)

type LTyFamInstDecl p = Located (TyFamInstDecl p)
newtype TyFamInstDecl p = TyFamInstDecl { tfid_eqn :: TyFamInstEqn p }

type LDataFamInstDecl p = Located (DataFamInstDecl p)
newtype DataFamInstDecl p
  = DataFamInstDecl { dfid_eqn :: FamInstEqn p (HsDataDefn p) }

type LFamInstEqn p rhs = Located (FamInstEqn p rhs)
type FamInstEqn p rhs
  = HsImplicitBndrs p (FamEqn p (HsTyPats p) rhs)

data FamEqn p pats rhs
  = FamEqn
       { feqn_ext    :: XCFamEqn p pats rhs
       , feqn_tycon  :: LIdP p
       , feqn_pats   :: pats
       , feqn_fixity :: LexicalFixity
       , feqn_rhs    :: rhs
       }
  | XFamEqn (XXFamEqn p pats rhs)

type LClsInstDecl p = Located (ClsInstDecl p)
data ClsInstDecl p
  = ClsInstDecl
      { cid_ext     :: XCClsInstDecl p
      , cid_poly_ty :: LHsSigType p
      , cid_binds         :: LHsBinds p
      , cid_sigs          :: [LSig p]
      , cid_tyfam_insts   :: [LTyFamInstDecl p]
      , cid_datafam_insts :: [LDataFamInstDecl p]
      , cid_overlap_mode  :: Maybe (Located OverlapMode) -- TTG: todo: SrcLocs
      }
  | XClsInstDecl (XXClsInstDecl p)

type LInstDecl p = Located (InstDecl p)
data InstDecl p
  = ClsInstD
      { cid_d_ext :: XClsInstD p
      , cid_inst  :: ClsInstDecl p }
  | DataFamInstD
      { dfid_ext  :: XDataFamInstD p
      , dfid_inst :: DataFamInstDecl p }
  | TyFamInstD
      { tfid_ext  :: XTyFamInstD p
      , tfid_inst :: TyFamInstDecl p }
  | XInstDecl (XXInstDecl p)

type LDerivDecl p = Located (DerivDecl p)
data DerivDecl p = DerivDecl
        { deriv_ext          :: XCDerivDecl p
        , deriv_type         :: LHsSigWcType p
        , deriv_strategy     :: Maybe (LDerivStrategy p)
        , deriv_overlap_mode :: Maybe (Located OverlapMode) -- TTG: todo: SrcLocs
        }
  | XDerivDecl (XXDerivDecl p)


type LDerivStrategy p = Located (DerivStrategy p)
data DerivStrategy p
  = StockStrategy
  | AnyclassStrategy
  | NewtypeStrategy
  | ViaStrategy (XViaStrategy p)


type LDefaultDecl p = Located (DefaultDecl p)
data DefaultDecl p
  = DefaultDecl (XCDefaultDecl p) [LHsType p]
  | XDefaultDecl (XXDefaultDecl p)

type LForeignDecl p = Located (ForeignDecl p)
data ForeignDecl p
  = ForeignImport
      { fd_i_ext  :: XForeignImport p
      , fd_name   :: LIdP p
      , fd_sig_ty :: LHsSigType p
      , fd_fi     :: ForeignImport }
  | ForeignExport
      { fd_e_ext  :: XForeignExport p
      , fd_name   :: LIdP p
      , fd_sig_ty :: LHsSigType p
      , fd_fe     :: ForeignExport }
  | XForeignDecl (XXForeignDecl p)

data ForeignImport
  = CImport  (Located CCallConv) -- TTG: todo: SrcLocs
             (Located Safety)    -- TTG: todo: SrcLocs
             (Maybe Header)
             CImportSpec
             {- TTG-Todo
             (Located SourceText)
             -}

data CImportSpec = CLabel    CLabelString
                 | CFunction CCallTarget
                 | CWrapper

data ForeignExport = CExport  (Located CExportSpec) -- TTG: todo: SrcLocs
                              {- TTG-Todo
                              (Located SourceText)
                              -}

type LRuleDecls p = Located (RuleDecls p)
data RuleDecls p = HsRules { rds_ext   :: XCRuleDecls p
                              {- TTG-Todo
                              , rds_src   :: SourceText
                              -}
                              , rds_rules :: [LRuleDecl p] }
  | XRuleDecls (XXRuleDecls p)

type LRuleDecl p = Located (RuleDecl p)
data RuleDecl p
  = HsRule
        (XHsRule p)
        {- TTG-Todo
        (Located (SourceText,RuleName))
        -}
        (Located RuleName) -- TTG-Todo new
                           -- TTG: todo: SrcLocs
        Activation
        [LRuleBndr p]
        (LHsExpr p)
        (LHsExpr p)

  | XRuleDecl (XXRuleDecl p)

type LRuleBndr p = Located (RuleBndr p)
data RuleBndr p
  = RuleBndr (XCRuleBndr p)  (LIdP p)
  | RuleBndrSig (XRuleBndrSig p) (LIdP p) (LHsSigWcType p)
  | XRuleBndr (XXRuleBndr p)

type LDocDecl = Located (DocDecl)
data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString

type LWarnDecls p = Located (WarnDecls p)
data WarnDecls p
  = Warnings { wd_ext      :: XWarnings p
             {- TTG-Todo
             , wd_src      :: SourceText
             -}
             , wd_warnings :: [LWarnDecl p] }
  | XWarnDecls (XXWarnDecls p)

type LWarnDecl p = Located (WarnDecl p)
data WarnDecl p
  = Warning (XWarning p) [LIdP p] WarningTxt
  | XWarnDecl (XXWarnDecl p)

type LAnnDecl p = Located (AnnDecl p)
data AnnDecl p
  = HsAnnotation (XHsAnnotation p)
                 {- TTG-Todo
                 SourceText
                 -}
                 (AnnProvenance (IdP p)) (LHsExpr p)
  | XAnnDecl (XXAnnDecl p)

data AnnProvenance name
  = ValueAnnProvenance (Located name)
  | TypeAnnProvenance (Located name)
  | ModuleAnnProvenance

type LRoleAnnotDecl p = Located (RoleAnnotDecl p)
data RoleAnnotDecl p
  = RoleAnnotDecl  (XCRoleAnnotDecl p)
                   (LIdP p)
                   [Located (Maybe Role)] -- TTG: todo: SrcLocs
  | XRoleAnnotDecl (XXRoleAnnotDecl p)
