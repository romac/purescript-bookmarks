
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

import Bookmarks.Types (BOOKMARKS, RawChanges, RawCreateDetails, RawDestination, NodeId(..), RawTreeNode(..), Unmodifiable(..))
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, runEffFn2)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), readArray, readNumber, readString, readNullOrUndefined, fail)
import Data.Foreign.Index ((!))
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.URI (URI, runParseURI)
import Prelude (Unit, bind, pure, ($), (<<<), (<>), (>>=))
import Text.Parsing.StringParser (ParseError(..))
-- TODO: foreign methods should return Foreign, and be parsed in the Bookmarks module

type EffFnAff eff a = EffFn2 eff (EffFn1 eff Error Unit) (EffFn1 eff a Unit) Unit

fromEffFnAff
  :: forall eff a
   . EffFnAff eff a
  -> Aff eff a
fromEffFnAff f = makeAff (\error success -> runEffFn2 f (mkEffFn1 error) (mkEffFn1 success))

foreign import createImpl
  :: forall eff
   . RawCreateDetails
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) Foreign

create :: forall eff. RawCreateDetails -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
create = fromEffFnAff <<< createImpl

foreign import getImpl
  :: forall eff
   . NodeId
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) Foreign

get :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
get = fromEffFnAff <<< getImpl

foreign import getManyImpl
  :: forall eff
   . Array NodeId
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) (Array Foreign)

getMany :: forall eff. Array NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getMany = fromEffFnAff <<< getManyImpl

foreign import getChildrenImpl
  :: forall eff
   . NodeId
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) (Array Foreign)

getChildren :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getChildren = fromEffFnAff <<< getChildrenImpl

foreign import getRecentImpl
  :: forall eff
   . Number
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) (Array Foreign)

getRecent :: forall eff. Number -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getRecent = fromEffFnAff <<< getRecentImpl

foreign import getTreeImpl
  :: forall eff
  . EffFnAff (bookmarks :: BOOKMARKS | eff) (Array Foreign)

getTree :: forall eff. Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getTree = fromEffFnAff getTreeImpl

foreign import getSubTreeImpl
  :: forall eff
   . NodeId
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) (Array Foreign)

getSubTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
getSubTree = fromEffFnAff <<< getSubTreeImpl

foreign import searchImpl
  :: forall eff
   . String
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) (Array Foreign)

search :: forall eff. String -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
search = fromEffFnAff <<< searchImpl

-- TODO: Add searchFull

foreign import moveImpl
  :: forall eff
   . NodeId
  -> RawDestination
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) Foreign

move :: forall eff. NodeId -> RawDestination -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
move id dest = fromEffFnAff (moveImpl id dest)

-- TODO: Allow passing a subset of Changes

foreign import updateImpl
  :: forall eff
   . NodeId
  -> RawChanges
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) Foreign

update :: forall eff. NodeId -> RawChanges -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
update id changes = fromEffFnAff (updateImpl id changes)

foreign import removeImpl
  :: forall eff
   . NodeId
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) Unit

remove :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
remove = fromEffFnAff <<< removeImpl

foreign import removeTreeImpl
  :: forall eff
   . NodeId
  -> EffFnAff (bookmarks :: BOOKMARKS | eff) Unit

removeTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
removeTree = fromEffFnAff <<< removeTreeImpl

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

