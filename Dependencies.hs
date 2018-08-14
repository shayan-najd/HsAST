{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module Dependencies where

data FastString -- TTG: todo: replace with String
data Bag     a  -- TTG: todo: use Data.Bag of containers
data Located a  -- TTG: todo: use either of solutions
                --            in the wiki

-- TTG-Todo: to be done
data UnitId

type FieldLabelString = FastString

data FieldLbl a = FieldLabel {
      flLabel        :: FieldLabelString,
      flIsOverloaded :: Bool,
      flSelector     :: a
    }

-- TTG-Todo: smells like a bad design
type family NameOrRdrName a

data Role = Nominal | Representational | Phantom

type PhaseNum = Int

data Activation = NeverActive
                | AlwaysActive
                | ActiveBefore {- TTG-Todo
                               SourceText
                               -}
                               PhaseNum
                | ActiveAfter  {- TTG-Todo
                               SourceText
                               -}
                               PhaseNum


type LStringLiteral = Located StringLiteral
data StringLiteral
  = StringLiteral { {- TTG-Todo
                    sl_st :: SourceText,
                    -} sl_fs :: FastString
                  }

data IntegralLit
  = IL { {- TTG-Todo
            il_text :: SourceText
       , -} il_neg :: Bool
       , il_value :: Integer
       }


data FractionalLit
  = FL { {- TTG-Todo
            fl_text :: SourceText
       , -} fl_neg :: Bool
       , fl_value :: Rational
       }

data InlinePragma
  = InlinePragma
      { {- TTG-Todo
        inl_src    :: SourceText
      , -}
        inl_inline :: InlineSpec
      , inl_sat    :: Maybe Arity
      , inl_act    :: Activation
      , inl_rule   :: RuleMatchInfo }

data RuleMatchInfo
  = ConLike
 | FunLike

data InlineSpec
  = Inline
  | Inlinable
  | NoInline
  | NoUserInline

data LexicalFixity
  = Prefix
  | Infix

data HsSrcBang =
  HsSrcBang {- TTG-Todo
            SourceText
            -}
            SrcUnpackedness
            SrcStrictness

data SrcStrictness
  = SrcLazy
  | SrcStrict
  | NoSrcStrict

data SrcUnpackedness
  = SrcUnpack
  | SrcNoUnpack
  | NoSrcUnpack

{- TTG-Todo
   ShNajd: we should rewrite to use x instead of parameter a
-}
type LBooleanFormula a = Located (BooleanFormula a)
data BooleanFormula a
  = Var a
  | And [LBooleanFormula a]
  | Or [LBooleanFormula a]
  | Parens (LBooleanFormula a)


type LFunDep a = Located (FunDep a)
type FunDep a = ([a],[a])

data SpliceExplicitFlag
  = ExplicitSplice
  | ImplicitSplice

data OverlapMode
  = NoOverlap    {- TTG-Todo
                 (Located SourceText)
                 -}
  | Overlappable {- TTG-Todo
                 (Located SourceText)
                 -}
  | Overlapping  {- TTG-Todo
                 (Located SourceText)
                 -}
  | Overlaps     {- TTG-Todo
                 (Located SourceText)
                 -}
  | Incoherent   {- TTG-Todo
                 (Located SourceText)
                 -}

data Origin = FromSource
            | Generated

type ConTag = Int

data Fixity
  = Fixity {- TTG-Todo
           SourceText
           -}
           Int
           FixityDirection

data FixityDirection
  = InfixL
  | InfixR
  | InfixN

data Boxity
  = Boxed
  | Unboxed

type Arity = Int

data WarningTxt
  = WarningTxt {- TTG-Todo
               (Located SourceText)
               -}
               [LStringLiteral]
  | DeprecatedTxt {- TTG-Todo
                  (Located SourceText)
                  -}
                  [LStringLiteral]

type RuleName = FastString
newtype ModuleName = ModuleName FastString
