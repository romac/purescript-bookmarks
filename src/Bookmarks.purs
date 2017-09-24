
module Bookmarks
  ( module Types
  , CreateDetails
  , Destination
  , Changes
  , create
  , get
  , getChildren
  , getMany
  , getRecent
  , getSubTree
  , getTree
  , move
  , remove
  , removeTree
  , search
  , update
  ) where

import Prelude              (Unit, (<<<), ($), (<$>), pure, map, bind, otherwise)

import Control.Monad.Aff    (Aff)
import Control.Monad.Except (runExcept)
import Data.Either          (fromRight)
import Data.Foreign         (F, Foreign)
import Data.Maybe           (Maybe, isJust, fromJust)
import Data.Nullable        (toNullable)
import Data.URI             (URI, printURI)
import Partial.Unsafe       (unsafePartial)

import Bookmarks.Types
import Bookmarks.Types      (BOOKMARKS, TreeNode(..), NodeId(..), Unmodifiable(..)) as Types

import Bookmarks.Foreign    (readRawTreeNode)
import Bookmarks.Foreign    as Foreign

type CreateDetails =
  { parentId :: Maybe NodeId
  , index    :: Maybe Number
  , title    :: Maybe String
  , url      :: Maybe URI
  }

type Destination =
  { id    :: Maybe NodeId
  , index :: Maybe Number
  }

type Changes =
  { title :: Maybe String
  , url   :: Maybe String
  }

create :: forall eff. CreateDetails -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
create = map unsafeReadTreeNode <<< Foreign.create <<< toRawCreateDetails

get :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
get = map unsafeReadTreeNode <<< Foreign.get

getMany :: forall eff. Array NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
getMany = map (map unsafeReadTreeNode) <<< Foreign.getMany

getChildren :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
getChildren = map (map unsafeReadTreeNode) <<< Foreign.getChildren

getRecent :: forall eff. Number -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
getRecent = map (map unsafeReadTreeNode) <<< Foreign.getRecent

getTree :: forall eff. Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
getTree = map (map unsafeReadTreeNode) Foreign.getTree

getSubTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
getSubTree = map (map unsafeReadTreeNode) <<< Foreign.getSubTree

search :: forall eff. String -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
search = map (map unsafeReadTreeNode) <<< Foreign.search

move :: forall eff. NodeId -> Destination -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
move id = map unsafeReadTreeNode <<< Foreign.move id <<< toRawDestination

update :: forall eff. NodeId -> Changes -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
update id = map unsafeReadTreeNode <<< Foreign.update id <<< toRawChanges

remove :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
remove = Foreign.remove

removeTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
removeTree = Foreign.removeTree

fromRawTreeNode :: RawTreeNode -> TreeNode
fromRawTreeNode (RawTreeNode raw) | isJust raw.url = unsafePartial $ fromJust do
  parentId <- raw.parentId
  index    <- raw.index
  url      <- raw.url
  pure $ Bookmark { id        : raw.id
                  , title     : raw.title
                  , dateAdded : raw.dateAdded
                  , parentId  : parentId
                  , index     : index
                  , url       : url
                  }

fromRawTreeNode (RawTreeNode raw) | otherwise = unsafePartial $ fromJust do
  children <- raw.children
  pure $ Root { id        : raw.id
              , title     : raw.title
              , dateAdded : raw.dateAdded
              , children  : fromRawTreeNode <$> children
              }

unsafeReadForeign :: forall a. (Foreign -> F a) -> Foreign -> a
unsafeReadForeign read value = unsafePartial (fromRight (runExcept (read value)))

unsafeReadRawTreeNode :: Foreign -> RawTreeNode
unsafeReadRawTreeNode = unsafeReadForeign readRawTreeNode

unsafeReadTreeNode :: Foreign -> TreeNode
unsafeReadTreeNode = fromRawTreeNode <<< unsafeReadRawTreeNode

unsafeReadTreeNodes :: Array Foreign -> Array TreeNode
unsafeReadTreeNodes = map (fromRawTreeNode <<< unsafeReadRawTreeNode)

toRawCreateDetails :: CreateDetails -> RawCreateDetails
toRawCreateDetails details = { parentId : toNullable details.parentId
                             , index    : toNullable details.index
                             , title    : toNullable details.title
                             , url      : toNullable (printURI <$> details.url)
                             }

toRawDestination :: Destination -> RawDestination
toRawDestination dest = { index : toNullable dest.index
                        , id    : toNullable dest.id
                        }

toRawChanges :: Changes -> RawChanges
toRawChanges changes = { title : toNullable changes.title
                       , url   : toNullable changes.url
                       }
