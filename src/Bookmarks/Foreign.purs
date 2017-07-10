
-- TODO: Use foreign-generic

module Bookmarks.Foreign
  ( create
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
  , readUnmodifable
  , readNodeId
  , readRawTreeNode
  , readRawTreeNodes
  ) where

import Prelude                   (Unit, bind, pure, ($), (<<<), (<>), (>>=))

import Control.Monad.Aff         (Aff, makeAff)
import Control.Monad.Eff         (kind Effect, Eff)
import Data.DateTime             (DateTime)
import Data.DateTime.Instant     (instant, toDateTime)
import Data.Either               (Either(..))
import Data.Foreign              (F, Foreign, ForeignError(..), readArray, readNumber, readString, readNullOrUndefined, fail)
import Data.Foreign.Index        ((!))
import Data.Function.Uncurried   (Fn1, runFn1, Fn2, runFn2, Fn3, runFn3)
import Data.Maybe                (maybe)
import Data.Time.Duration        (Milliseconds(..))
import Data.Traversable          (traverse)
import Data.URI                  (URI, runParseURI)
import Text.Parsing.StringParser (ParseError(..))

import Bookmarks.Types           (BOOKMARKS, RawChanges, RawCreateDetails, RawDestination,
                                  NodeId(..), RawTreeNode(..), Unmodifiable(..))

-- TODO: foreign methods should return Foreign, and be parsed in the Bookmarks module

foreign import createImpl :: forall eff. Fn2 (Foreign -> Eff eff Unit) RawCreateDetails (Eff eff Unit)

create :: forall eff. RawCreateDetails -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
create bookmark = makeAff (\error success -> runFn2 createImpl success bookmark)

foreign import getImpl :: forall eff. Fn2 (Foreign -> Eff eff Unit) NodeId (Eff eff Unit)

get :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
get id = makeAff (\error success -> runFn2 getImpl success id)

foreign import getManyImpl :: forall eff. Fn2 (Array Foreign -> Eff eff Unit) (Array NodeId) (Eff eff Unit)

getMany :: forall eff. Array NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getMany ids = makeAff (\error success -> runFn2 getManyImpl success ids)

foreign import getChildrenImpl :: forall eff. Fn2 (Array Foreign -> Eff eff Unit) NodeId (Eff eff Unit)

getChildren :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getChildren id = makeAff (\error success -> runFn2 getChildrenImpl success id)

foreign import getRecentImpl :: forall eff. Fn2 (Array Foreign -> Eff eff Unit) Number (Eff eff Unit)

getRecent :: forall eff. Number -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getRecent numberOfItems = makeAff (\error success -> runFn2 getRecentImpl success numberOfItems)

foreign import getTreeImpl :: forall eff. Fn1 (Array Foreign -> Eff eff Unit) (Eff eff Unit)

getTree :: forall eff. Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getTree = makeAff (\error success -> runFn1 getTreeImpl success)

foreign import getSubTreeImpl :: forall eff. Fn2 (Array Foreign -> Eff eff Unit) NodeId (Eff eff Unit)

getSubTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getSubTree id = makeAff (\error success -> runFn2 getSubTreeImpl success id)

foreign import searchImpl :: forall eff. Fn2 (Array Foreign -> Eff eff Unit) String (Eff eff Unit)

search :: forall eff. String -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
search query = makeAff (\error success -> runFn2 searchImpl success query)

-- TODO: Add searchFull

foreign import moveImpl :: forall eff. Fn3 (Foreign -> Eff eff Unit) NodeId RawDestination (Eff eff Unit)

move :: forall eff. NodeId -> RawDestination -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
move id dest = makeAff (\error success -> runFn3 moveImpl success id dest)

-- TODO: Allow passing a subset of Changes

foreign import updateImpl :: forall eff. Fn3 (Foreign -> Eff eff Unit) NodeId RawChanges (Eff eff Unit)

update :: forall eff. NodeId -> RawChanges -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
update id changes = makeAff (\error success -> runFn3 updateImpl success id changes)

foreign import removeImpl :: forall eff. Fn2 (Unit -> Eff eff Unit) NodeId (Eff eff Unit)

remove :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
remove id = makeAff (\error success -> runFn2 removeImpl success id)

foreign import removeTreeImpl :: forall eff. Fn2 (Unit -> Eff eff Unit) NodeId (Eff eff Unit)

removeTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
removeTree id = makeAff (\error success -> runFn2 removeTreeImpl success id)

readUnmodifable :: Foreign -> F Unmodifiable
readUnmodifable value = do
  str <- readString value
  case str of
     "managed" -> pure Managed
     v -> fail $ ForeignError ("Unknown value for 'unmodifiable': \"" <> v <> "\"")

readNodeId :: Foreign -> F NodeId
readNodeId value = do
  str <- readString value
  pure (NodeId str)

readRawTreeNode :: Foreign -> F RawTreeNode
readRawTreeNode value = do
  id                <- value ! "id"                >>= readNodeId
  parentId          <- value ! "parentId"          >>= readNullOrUndefined >>= traverse readNodeId
  index             <- value ! "index"             >>= readNullOrUndefined >>= traverse readNumber
  url               <- value ! "url"               >>= readNullOrUndefined >>= traverse readURI
  title             <- value ! "title"             >>= readString
  dateAdded         <- value ! "dateAdded"         >>= readNullOrUndefined >>= traverse readDateTime
  dateGroupModified <- value ! "dateGroupModified" >>= readNullOrUndefined >>= traverse readDateTime
  unmodifiable      <- value ! "unmodifiable"      >>= readNullOrUndefined >>= traverse readUnmodifable
  children          <- value ! "children"          >>= readNullOrUndefined >>= traverse readRawTreeNodes
  pure $ RawTreeNode { id, parentId, index, url, title, dateAdded, dateGroupModified, unmodifiable, children }

readRawTreeNodes :: Foreign -> F (Array RawTreeNode)
readRawTreeNodes value = readArray value >>= traverse readRawTreeNode

readURI :: Foreign -> F URI
readURI value = do
  str <- readString value
  case runParseURI str of
    Right uri -> pure uri
    Left (ParseError msg) -> fail (ForeignError msg)

readDateTime :: Foreign -> F DateTime
readDateTime value = do
  millis <- readNumber value
  maybe (fail (ForeignError "Invalid timestamp"))
        (pure <<< toDateTime)
        (instant (Milliseconds millis))

