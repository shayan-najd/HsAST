{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE RoleAnnotations #-}

module HsExpr where

import Dependencies ( Located )
import {-# SOURCE #-} HsPat  ( LPat )
import HsExtension

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal nominal
type role GRHSs nominal nominal
type role HsSplice nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)

type LHsExpr a = Located (HsExpr a)
type LHsSplice a = Located (HsSplice a)