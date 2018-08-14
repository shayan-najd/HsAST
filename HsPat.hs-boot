{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE RoleAnnotations #-}

module HsPat where

import Dependencies( Located )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)
