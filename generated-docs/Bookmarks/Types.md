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

#### `CreateDetails`

``` purescript
type CreateDetails = { parentId :: Maybe NodeId, index :: Maybe Number, title :: Maybe String, url :: Maybe URI }
```

#### `Destination`

``` purescript
type Destination = { id :: Maybe NodeId, index :: Maybe Number }
```

#### `Changes`

``` purescript
type Changes = { title :: Maybe String, url :: Maybe String }
```


