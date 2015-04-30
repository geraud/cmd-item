{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.CmdItemSpec (main, spec) where

import Control.Applicative       ((<$>), (<*>))
import Test.Hspec
import Test.Hspec.Laws
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.CmdItem

spec :: Spec
spec = describe "CmdItem" $ shouldSatisfyMonoidLaws (undefined :: CmdItem)

main :: IO ()
main = hspec spec

instance Arbitrary CmdItem where
    arbitrary = CmdItem <$> arbitrary <*> arbitrary
