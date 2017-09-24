
module Bookmarks.Types
  ( BOOKMARKS
  , Unmodifiable(..)
  , NodeId(..)
  , TreeNode(..)
  , RawTreeNode(..)
  , RawCreateDetails
  , RawDestination
  , RawChanges
  ) where

import Control.Monad.Eff (kind Effect)
import Data.Maybe    (Maybe)
import Data.URI      (URI)
import Data.DateTime (DateTime)
import Data.Nullable (Nullable)

-- TODO: Add instances

foreign import data BOOKMARKS :: Effect

data Unmodifiable
  = Managed

newtype NodeId
  = NodeId String

data TreeNode
  = Bookmark
    { id        :: NodeId
    , title     :: String
    , dateAdded :: Maybe DateTime
    , parentId  :: NodeId
    , index     :: Number
    , url       :: URI
    }
  | Folder
    { id                :: NodeId
    , title             :: String
    , dateAdded         :: Maybe DateTime
    , parentId          :: NodeId
    , index             :: Number
    , dateGroupModified :: Maybe DateTime
    , children          :: Array TreeNode
  }
  | Root
    { id        :: NodeId
    , title     :: String
    , dateAdded :: Maybe DateTime
    , children  :: Array TreeNode
    }

newtype RawTreeNode
  = RawTreeNode
  { id                :: NodeId
  , parentId          :: Maybe NodeId
  , index             :: Maybe Number
  , url               :: Maybe URI
  , title             :: String
  , dateAdded         :: Maybe DateTime
  , dateGroupModified :: Maybe DateTime
  , unmodifiable      :: Maybe Unmodifiable
  , children          :: Maybe (Array RawTreeNode)
  }

type RawCreateDetails =
  { parentId :: Nullable NodeId
  , index    :: Nullable Number 
  , title    :: Nullable String
  , url      :: Nullable String
  }

type RawDestination =
  { id    :: Nullable NodeId
  , index :: Nullable Number
  }

type RawChanges =
  { title :: Nullable String
  , url   :: Nullable String
  }

