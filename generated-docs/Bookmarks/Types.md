## Module Bookmarks.Types

#### `BOOKMARKS`

``` purescript
data BOOKMARKS :: Effect
```

#### `Unmodifiable`

``` purescript
data Unmodifiable
  = Managed
```

#### `NodeId`

``` purescript
newtype NodeId
  = NodeId String
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

#### `RawCreateDetails`

``` purescript
type RawCreateDetails = { parentId :: Nullable NodeId, index :: Nullable Number, title :: Nullable String, url :: Nullable String }
```

#### `RawDestination`

``` purescript
type RawDestination = { id :: Nullable NodeId, index :: Nullable Number }
```

#### `RawChanges`

``` purescript
type RawChanges = { title :: Nullable String, url :: Nullable String }
```


