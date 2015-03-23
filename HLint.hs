{-# LANGUAGE PackageImports #-}

module HLint () where

import "hint" HLint.Default
import "hint" HLint.Dollar
--import "hint" HLint.Generalise


warn = fmap f x ==> f <$> x
warn = map f x ==> f <$> x
