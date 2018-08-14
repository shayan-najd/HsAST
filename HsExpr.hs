{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsExpr where

import Dependencies
import HsExtension

import HsLit
import HsPat
import HsTypes
import HsBinds
import HsDecls

type LHsExpr p = Located (HsExpr p)
data HsExpr p
  = HsVar       (XVar p)
                (LIdP p)

  {- TTG-TODO
  | HsUnboundVar (XUnboundVar p)
                 UnboundVar
  | HsConLikeOut (XConLikeOut p)
                 ConLike
  -}
  | HsRecFld    (XRecFld p) (AmbiguousFieldOcc p)
  | HsOverLabel (XOverLabel p)
                (Maybe (IdP p)) FastString
  | HsIPVar     (XIPVar p)
                HsIPName
  | HsOverLit   (XOverLitE p)
                (HsOverLit p)
  | HsLit       (XLitE p)
                (HsLit p)
  | HsLam       (XLam p)
                (MatchGroup p (LHsExpr p))
  | HsLamCase   (XLamCase p) (MatchGroup p (LHsExpr p))
  | HsApp       (XApp p) (LHsExpr p) (LHsExpr p)
  | HsAppType   (XAppTypeE p) (LHsExpr p)
  | OpApp       (XOpApp p)
                (LHsExpr p)
                (LHsExpr p)
                (LHsExpr p)
  | NegApp      (XNegApp p)
                (LHsExpr p)
                {- TTG-TODO
                (SyntaxExpr p)
                -}
  | HsPar       (XPar p)
                (LHsExpr p)
  | SectionL    (XSectionL p)
                (LHsExpr p)
                (LHsExpr p)
  | SectionR    (XSectionR p)
                (LHsExpr p)
                (LHsExpr p)
  | ExplicitTuple
                (XExplicitTuple p)
                [LHsTupArg p]
                Boxity
  | ExplicitSum
                (XExplicitSum p)
                ConTag
                Arity
                (LHsExpr p)
  | HsCase      (XCase p)
                (LHsExpr p)
                (MatchGroup p (LHsExpr p))
  | HsIf        (XIf p)
                {- TTG-TODO
                (Maybe (SyntaxExpr p))
                -}
                (LHsExpr p)
                (LHsExpr p)
                (LHsExpr p)
  | HsMultiIf   (XMultiIf p) [LGRHS p (LHsExpr p)]
  | HsLet       (XLet p)
                (LHsLocalBinds p)
                (LHsExpr  p)
  | HsDo        (XDo p)
                {- TTG-TODO Not Sure
                (HsStmtContext Name)
                -}
                (Located [ExprLStmt p]) -- TTG: todo: SrcLocs

  | ExplicitList
                (XExplicitList p)
                {- TTG-TODO
                (Maybe (SyntaxExpr p))
                -}
                [LHsExpr p]
  | RecordCon   (XRecordCon p)
                (LIdP p)
                (HsRecordBinds p)
  | RecordUpd   (XRecordUpd p)
                (LHsExpr p)
                [LHsRecUpdField p]
  | ExprWithTySig
                (XExprWithTySig p)
                (LHsExpr p)
  | ArithSeq    (XArithSeq p)
                {- TTG-TODO
                (Maybe (SyntaxExpr p))
                -}
                (ArithSeqInfo p)
  | HsSCC       (XSCC p)
                {- TTG-TODO
                SourceText
                -}
                StringLiteral
                (LHsExpr p)
  | HsCoreAnn   (XCoreAnn p)
                {- TTG-TODO
                SourceText
                -}
                StringLiteral
                (LHsExpr p)
  | HsBracket   (XBracket p) (HsBracket p)
  {- TTG-TODO
  | HsRnBracketOut
      (XRnBracketOut p)
      (HsBracket GhcRn)
      [PendingRnSplice]
  | HsTcBracketOut
      (XTcBracketOut p)
      (HsBracket GhcRn)
      [PendingTcSplice]
  -}
  | HsSpliceE   (XSpliceE p) (HsSplice p)
  | HsProc      (XProc p)
                (LPat p)
                (LHsCmdTop p)
  | HsStatic    (XStatic p)
                (LHsExpr p)
  | HsArrApp    (XArrApp p)
                (LHsExpr p)
                (LHsExpr p)
                HsArrAppType
                Bool
  | HsArrForm   (XArrForm p)
                (LHsExpr p)
                (Maybe Fixity)
                [LHsCmdTop p]

  {- TTG-TODO  ShNajd: NOT SURE
  | HsTick      (XTick p)
                (Tickish (IdP p))
                (LHsExpr p)
  | HsBinTick
     (XBinTick p)
     Int
     Int
     (LHsExpr p)
  | HsTickPragma
     (XTickPragma p)
     SourceText
     (StringLiteral,(Int,Int),(Int,Int))
     ((SourceText,SourceText),(SourceText,SourceText))
     (LHsExpr p)
  -}
  {- TTG-TODO
  | EWildPat (XEWildPat p)
  | EAsPat      (XEAsPat p)
                (LIdP p)
                (LHsExpr p)
  | EViewPat    (XEViewPat p)
                (LHsExpr p)
                (LHsExpr p)
  | ELazyPat    (XELazyPat p) (LHsExpr p)
  -}

  {- TTG-TODO
  |  HsWrap     (XWrap p)
                HsWrapper
                (HsExpr p)
  -}
  | XExpr       (XXExpr p)


type LHsTupArg id = Located (HsTupArg id)
data HsTupArg id
  = Present (XPresent id) (LHsExpr id)
  | Missing (XMissing id)
  | XTupArg (XXTupArg id)

type LHsCmd id = Located (HsCmd id)
data HsCmd id
  = HsCmdArrApp
        (XCmdArrApp id)
        (LHsExpr id)
        (LHsExpr id)
        HsArrAppType
        Bool
  | HsCmdArrForm
        (XCmdArrForm id)
        (LHsExpr id)
        LexicalFixity
        (Maybe Fixity)
        [LHsCmdTop id]

  | HsCmdApp    (XCmdApp id)
                (LHsCmd id)
                (LHsExpr id)

  | HsCmdLam    (XCmdLam id)
                (MatchGroup id (LHsCmd id))
  | HsCmdPar    (XCmdPar id)
                (LHsCmd id)
  | HsCmdCase   (XCmdCase id)
                (LHsExpr id)
                (MatchGroup id (LHsCmd id))
  | HsCmdIf     (XCmdIf id)
                {- TTG-TODO
                (Maybe (SyntaxExpr id))
                -}
                (LHsExpr id)
                (LHsCmd id)
                (LHsCmd id)
  | HsCmdLet    (XCmdLet id)
                (LHsLocalBinds id)
                (LHsCmd  id)
  | HsCmdDo     (XCmdDo id)
                (Located [CmdLStmt id]) -- TTG: todo: SrcLocs
  | HsCmdWrap   (XCmdWrap id)
                {- TTG-TODO
                HsWrapper
                -}
                (HsCmd id)
  | XCmd        (XXCmd id)

data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp

type LHsCmdTop p = Located (HsCmdTop p)
data HsCmdTop p
  = HsCmdTop (XCmdTop p)
             (LHsCmd p)
  | XCmdTop (XXCmdTop p)

type HsRecordBinds p = HsRecFields p (LHsExpr p)

data MatchGroup p body
  = MG { mg_ext     :: XMG p body
       , mg_alts    :: Located [LMatch p body] -- TTG: todo: SrcLocs
       , mg_origin  :: Origin -- TTG-todo ?!
       }
  | XMatchGroup (XXMatchGroup p body)

type LMatch id body = Located (Match id body)
data Match p body
  = Match {
        m_ext :: XCMatch p body,
        -- TTG-TODO: NameOrRdrName is strange here!
        m_ctxt :: HsMatchContext (NameOrRdrName (IdP p)),
        m_pats :: [LPat p],
        m_grhss :: (GRHSs p body)
  }
  | XMatch (XXMatch p body)

data GRHSs p body
  = GRHSs {
      grhssExt :: XCGRHSs p body,
      grhssGRHSs :: [LGRHS p body],
      grhssLocalBinds :: LHsLocalBinds p
    }
  | XGRHSs (XXGRHSs p body)

type LGRHS id body = Located (GRHS id body)
data GRHS p body = GRHS (XCGRHS p body)
                        [GuardLStmt p]
                        body
                  | XGRHS (XXGRHS p body)

type LStmt id body = Located (StmtLR id id body)
type Stmt id body = StmtLR id id body

type CmdLStmt   id = LStmt id (LHsCmd  id)
type CmdStmt    id = Stmt  id (LHsCmd  id)
type ExprLStmt  id = LStmt id (LHsExpr id)
type ExprStmt   id = Stmt  id (LHsExpr id)
type GuardLStmt id = LStmt id (LHsExpr id)
type GuardStmt  id = Stmt  id (LHsExpr id)
type GhciLStmt  id = LStmt id (LHsExpr id)
type GhciStmt   id = Stmt  id (LHsExpr id)

type LStmtLR idL idR body = Located (StmtLR idL idR body)
data StmtLR idL idR body
  = LastStmt
          (XLastStmt idL idR body)
          body
          Bool
          {- TTG-TODO
          (SyntaxExpr idR)
          -}
  | BindStmt (XBindStmt idL idR body)
             (LPat idL)
             body
             {- TTG-TODO
             (SyntaxExpr idR)
             (SyntaxExpr idR)
             -}
  {- TTG-TODO
  | ApplicativeStmt
             (XApplicativeStmt idL idR body)
             [ ( SyntaxExpr idR , ApplicativeArg idL) ]
             (Maybe (SyntaxExpr idR))
  -}
  | BodyStmt (XBodyStmt idL idR body)
             body
             {- TTG-TODO
             (SyntaxExpr idR)
             (SyntaxExpr idR)
             -}
  | LetStmt  (XLetStmt idL idR body) (LHsLocalBindsLR idL idR)
  | ParStmt  (XParStmt idL idR body)
             [ParStmtBlock idL idR]
             (HsExpr idR)
             {- TTG-TODO
             (SyntaxExpr idR)
            -}
  | TransStmt {
      trS_ext   :: XTransStmt idL idR body,
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],
      trS_bndrs :: [(IdP idR, IdP idR)],
      trS_using :: LHsExpr idR,
      trS_by :: Maybe (LHsExpr idR),
      {- TTG-TODO
      trS_ret :: SyntaxExpr idR,
      trS_bind :: SyntaxExpr idR,
      -}
      trS_fmap :: HsExpr idR }
  | RecStmt
     { recS_ext :: XRecStmt idL idR body
     , recS_stmts :: [LStmtLR idL idR body]
     , recS_later_ids :: [IdP idR]
     , recS_rec_ids :: [IdP idR]
     {- TTG-TODO
     , recS_bind_fn :: SyntaxExpr idR
     , recS_ret_fn  :: SyntaxExpr idR
     , recS_mfix_fn :: SyntaxExpr idR
     -}
      }
  | XStmtLR (XXStmtLR idL idR body)

data TransForm
  = ThenForm
  | GroupForm

data ParStmtBlock idL idR
  = ParStmtBlock
        (XParStmtBlock idL idR)
        [ExprLStmt idL]
        [IdP idR]
        {- TTG-TODO
        (SyntaxExpr idR)
        -}
  | XParStmtBlock (XXParStmtBlock idL idR)


type LHsSplice id = Located (HsSplice id)
data HsSplice id
   = HsTypedSplice
        (XTypedSplice id)
        SpliceDecoration
        (IdP id)
        (LHsExpr id)

   | HsUntypedSplice
        (XUntypedSplice id)
        SpliceDecoration
        (IdP id)
        (LHsExpr id)

   | HsQuasiQuote
        (XQuasiQuote id)
        (IdP id)
        (IdP id)
        {- TTG-TODO
        SrcSpan
        -}
        FastString
   {- TTG-TODO
   | HsSpliced
        (XSpliced id)
        ThModFinalizers
        (HsSplicedThing id)
   -}
   | XSplice (XXSplice id)

data SpliceDecoration
  = HasParens
  | HasDollar
  | NoParens

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

data HsBracket p
  = ExpBr    (XExpBr    p) (LHsExpr p)
  | PatBr    (XPatBr    p) (LPat p)
  | DecBrL   (XDecBrL   p) [LHsDecl p]
  | DecBrG   (XDecBrG   p) (HsGroup p)
  | TypBr    (XTypBr    p) (LHsType p)
  | VarBr    (XVarBr    p) Bool (IdP p)
  | TExpBr   (XTExpBr   p) (LHsExpr p)
  | XBracket (XXBracket p)

data ArithSeqInfo id
  = From            (LHsExpr id)
  | FromThen        (LHsExpr id)
                    (LHsExpr id)
  | FromTo          (LHsExpr id)
                    (LHsExpr id)
  | FromThenTo      (LHsExpr id)
                    (LHsExpr id)
                    (LHsExpr id)

data HsMatchContext id -- Not an extensible tag
  = FunRhs { mc_fun        :: Located id -- TTG: todo: SrcLocs
           , mc_fixity     :: LexicalFixity
           , mc_strictness :: SrcStrictness

           }
  | LambdaExpr
  | CaseAlt
  | IfAlt
  | ProcExpr
  | PatBindRhs
  | PatBindGuards
  | RecUpd
  | StmtCtxt (HsStmtContext id)
  | ThPatSplice
  | ThPatQuote
  | PatSyn

data HsStmtContext id
  = ListComp
  | MonadComp
  | DoExpr
  | MDoExpr
  | ArrowExpr
  | GhciStmtCtxt
  | PatGuard (HsMatchContext id)
  | ParStmtCtxt (HsStmtContext id)
  | TransStmtCtxt (HsStmtContext id)
