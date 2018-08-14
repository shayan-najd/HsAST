{-# OPTIONS_GHC -Wall     #-}
{-# LANGUAGE TypeFamilies #-}

module HsDoc where

import Dependencies
import Data.ByteString

type LHsDocString = Located HsDocString
newtype HsDocString = HsDocString ByteString
