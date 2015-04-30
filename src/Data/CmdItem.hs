{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | CmdItem is a tool to build command lines.
module Data.CmdItem  where
import           Control.Applicative ((<$>))
import           Data.Either         (either, partitionEithers)
import           Data.List           (intercalate)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Monoid
import           Data.Text           (Text, pack)
import qualified Data.Text           as T
import           GHC.Exts

import           Text.Templater      (template)

data CmdItem
    = CmdItem
        { fragments :: [Text]         -- pieces of command line
        , localVars :: Map Text Text  -- context of substitution
        }
    deriving (Show, Eq)

-- | Renders a command line item to a @Text@ string
-- This will throw an error if anything goes wrong
render :: CmdItem -> IO Text
render c = either error return (renderEither c)

-- | Renders a command line item to a @Maybe Text@
renderMaybe :: CmdItem -> Maybe Text
renderMaybe c = either (const Nothing) return (renderEither c)

-- | Renders a command line item to a @Either String Text@
renderEither :: CmdItem -> Either String Text
renderEither (CmdItem {..}) =
    let (lefts, rights) = partitionEithers $ (\x -> template x (`M.lookup` localVars)) <$> fragments
        isOk = null lefts
    in if isOk
       then Right $ T.intercalate " " rights
       else Left $ intercalate ", " lefts

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
    fromList lst = CmdItem mempty (M.fromList lst)
    toList (CmdItem _ m) = M.toList m
