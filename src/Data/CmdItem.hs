{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.CmdItem where

import           Data.Either     (either, partitionEithers)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map, empty, lookup)
import           Data.Monoid
import           Data.Text       (Text, pack)
import qualified Data.Text       as T
import           GHC.Exts
import           Prelude         hiding (lookup)

import           Text.Templater  (template)

render :: CmdItem -> IO Text
render c = either error return (renderEither c)

renderMaybe :: CmdItem -> Maybe Text
renderMaybe c = either (const Nothing) return (renderEither c)

renderEither :: CmdItem -> Either String Text
renderEither (CmdItem {..}) =
    let (lefts, rights) = partitionEithers $ (\x -> template x (`lookup` localVars)) <$> fragments
        isOk = null lefts
    in if isOk
       then Right $ T.intercalate " " rights
       else Left $ intercalate ", " lefts

data CmdItem
    = CmdItem
        { fragments :: [Text]
        , localVars :: Map Text Text
        }
    deriving (Show, Eq)

instance Monoid CmdItem where
    mempty = CmdItem mempty mempty
    mappend (CmdItem cmdA localA) (CmdItem cmdB localB) =
        CmdItem (cmdA <> cmdB) (localA <> localB)

instance IsString (CmdItem) where
    fromString "" = mempty
    fromString st = CmdItem [pack st] mempty

instance IsList (CmdItem) where
    type Item CmdItem = (Text, Text)
    fromList [] = mempty
    fromList lst = CmdItem mempty (fromList lst)
    toList (CmdItem _ m) = toList m
