## Module Bookmarks

#### `create`

``` purescript
create :: forall eff. CreateDetails -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
```

#### `get`

``` purescript
get :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
```

#### `getMany`

``` purescript
getMany :: forall eff. Array NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
```

#### `getChildren`

``` purescript
getChildren :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
```

#### `getRecent`

``` purescript
getRecent :: forall eff. Number -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
```

#### `getTree`

``` purescript
getTree :: forall eff. Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
```

#### `getSubTree`

``` purescript
getSubTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
```

#### `search`

``` purescript
search :: forall eff. String -> Aff (bookmarks :: BOOKMARKS | eff) (Array TreeNode)
```

#### `move`

``` purescript
move :: forall eff. NodeId -> Destination -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
```

#### `update`

``` purescript
update :: forall eff. NodeId -> Changes -> Aff (bookmarks :: BOOKMARKS | eff) TreeNode
```

#### `remove`

``` purescript
remove :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
```

#### `removeTree`

``` purescript
removeTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
```


### Re-exported from Bookmarks.Types:

#### `Unmodifiable`

``` purescript
data Unmodifiable
  = Managed
```

#### `TreeNode`

``` purescript
data TreeNode
  = Bookmark { id :: NodeId, title :: String, dateAdded :: Maybe DateTime, parentId :: NodeId, index :: Number, url :: URI }
  | Folder { id :: NodeId, title :: String, dateAdded :: Maybe DateTime, parentId :: NodeId, index :: Number, dateGroupModified :: Maybe DateTime, children :: Array TreeNode }
  | Root { id :: NodeId, title :: String, dateAdded :: Maybe DateTime, children :: Array TreeNode }
```

#### `RawTreeNode`

``` purescript
newtype RawTreeNode
  = RawTreeNode { id :: NodeId, parentId :: Maybe NodeId, index :: Maybe Number, url :: Maybe URI, title :: String, dateAdded :: Maybe DateTime, dateGroupModified :: Maybe DateTime, unmodifiable :: Maybe Unmodifiable, children :: Maybe (Array RawTreeNode) }
```

#### `RawDestination`

``` purescript
type RawDestination = { id :: Nullable NodeId, index :: Nullable Number }
```

#### `RawCreateDetails`

``` purescript
type RawCreateDetails = { parentId :: Nullable NodeId, index :: Nullable Number, title :: Nullable String, url :: Nullable String }
```

#### `RawChanges`

``` purescript
type RawChanges = { title :: Nullable String, url :: Nullable String }
```

#### `NodeId`

``` purescript
newtype NodeId
  = NodeId String
```

#### `Destination`

``` purescript
type Destination = { id :: Maybe NodeId, index :: Maybe Number }
```

#### `CreateDetails`

``` purescript
type CreateDetails = { parentId :: Maybe NodeId, index :: Maybe Number, title :: Maybe String, url :: Maybe URI }
```

#### `Changes`

``` purescript
type Changes = { title :: Maybe String, url :: Maybe String }
```

#### `BOOKMARKS`

``` purescript
data BOOKMARKS :: Effect
```

