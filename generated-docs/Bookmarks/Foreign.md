## Module Bookmarks.Foreign

#### `create`

``` purescript
create :: forall eff. RawCreateDetails -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
```

#### `get`

``` purescript
get :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
```

#### `getMany`

``` purescript
getMany :: forall eff. Array NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
```

#### `getChildren`

``` purescript
getChildren :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
```

#### `getRecent`

``` purescript
getRecent :: forall eff. Number -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
```

#### `getTree`

``` purescript
getTree :: forall eff. Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
```

#### `getSubTree`

``` purescript
getSubTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
```

#### `search`

``` purescript
search :: forall eff. String -> Aff (bookmarks :: BOOKMARKS | eff) (Array Foreign)
```

#### `move`

``` purescript
move :: forall eff. NodeId -> RawDestination -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
```

#### `update`

``` purescript
update :: forall eff. NodeId -> RawChanges -> Aff (bookmarks :: BOOKMARKS | eff) Foreign
```

#### `remove`

``` purescript
remove :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
```

#### `removeTree`

``` purescript
removeTree :: forall eff. NodeId -> Aff (bookmarks :: BOOKMARKS | eff) Unit
```

#### `readUnmodifable`

``` purescript
readUnmodifable :: Foreign -> F Unmodifiable
```

#### `readNodeId`

``` purescript
readNodeId :: Foreign -> F NodeId
```

#### `readRawTreeNode`

``` purescript
readRawTreeNode :: Foreign -> F RawTreeNode
```

#### `readRawTreeNodes`

``` purescript
readRawTreeNodes :: Foreign -> F (Array RawTreeNode)
```


