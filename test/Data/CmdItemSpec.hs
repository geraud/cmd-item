{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.CmdItemSpec (main, spec) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative       ((<$>), (<*>))
#endif
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
