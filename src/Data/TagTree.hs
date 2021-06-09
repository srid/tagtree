{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.TagTree
  ( -- * Types
    Tag (..),
    TagPattern (unTagPattern),
    TagNode (..),

    -- * Create Tags
    constructTag,

    -- * Creating Tag Trees
    tagTree,

    -- * Searching Tags
    mkTagPattern,
    mkTagPatternFromTag,
    tagMatch,

    -- * Working with Tag Trees
    foldTagTree,
  )
where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import Data.TagTree.PathTree (annotatePathsWith, foldSingleParentsWith, mkTreeFromPaths)
import qualified Data.Text as T
import Data.Tree (Forest)
import System.FilePattern (FilePattern, (?==))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

-- | A hierarchical tag
--
-- Tag nodes are separated by @/@
newtype Tag = Tag {unTag :: Text}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype
    ( ToJSON,
      FromJSON
    )

--------------
-- Tag Pattern
---------------

-- | A glob-based pattern to match hierarchical tags
--
-- For example, the pattern
--
-- > foo/**
--
-- matches both the following
--
-- > foo/bar/baz
-- > foo/baz
newtype TagPattern = TagPattern {unTagPattern :: FilePattern}
  deriving
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving newtype
    ( ToJSON,
      FromJSON
    )

mkTagPattern :: Text -> TagPattern
mkTagPattern =
  TagPattern . toString

mkTagPatternFromTag :: Tag -> TagPattern
mkTagPatternFromTag (Tag t) =
  TagPattern $ toString t

tagMatch :: TagPattern -> Tag -> Bool
tagMatch (TagPattern pat) (Tag tag) =
  pat ?== toString tag

-----------
-- Tag Tree
-----------

-- | An individual component of a hierarchical tag
--
-- The following hierarchical tag,
--
-- > foo/bar/baz
--
-- has three tag nodes: @foo@, @bar@ and @baz@
newtype TagNode = TagNode {unTagNode :: Text}
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON)

deconstructTag :: HasCallStack => Tag -> NonEmpty TagNode
deconstructTag (Tag s) =
  either error id $ parse tagParser (toString s) s
  where
    tagParser :: Parser (NonEmpty TagNode)
    tagParser =
      nodeParser `sepBy1` M.char '/'
    nodeParser :: Parser TagNode
    nodeParser =
      TagNode . toText <$> M.some (M.anySingleBut '/')

constructTag :: NonEmpty TagNode -> Tag
constructTag (fmap unTagNode . toList -> nodes) =
  Tag $ T.intercalate "/" nodes

-- | Construct the tree from a list of hierarchical tags
tagTree :: ann ~ Natural => Map Tag ann -> Forest (TagNode, ann)
tagTree tags =
  fmap (annotatePathsWith $ countFor tags) $
    mkTreeFromPaths $
      toList . deconstructTag
        <$> Map.keys tags
  where
    countFor tags' path =
      fromMaybe 0 $ Map.lookup (constructTag path) tags'

foldTagTree :: ann ~ Natural => Forest (TagNode, ann) -> Forest (NonEmpty TagNode, ann)
foldTagTree tree =
  foldSingleParentsWith foldNodes <$> fmap (fmap (first (:| []))) tree
  where
    foldNodes (parent, 0) (child, count) = Just (parent <> child, count)
    foldNodes _ _ = Nothing

type Parser a = M.Parsec Void Text a

parse :: Parser a -> String -> Text -> Either Text a
parse p fn s =
  first (toText . M.errorBundlePretty) $
    M.parse (p <* M.eof) fn s
