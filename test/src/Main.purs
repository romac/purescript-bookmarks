
module Main where

import Prelude (Unit, ($), (+), (==), bind, discard)

import Control.Monad.Aff.AVar (AVAR)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.URI (runParseURI)

import Test.Unit (suite, test, timeout, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)

import Bookmarks (BOOKMARKS, NodeId(..), CreateDetails, TreeNode(..), Destination, Changes)
import Bookmarks as Bookmarks

main :: forall e. Eff ( bookmarks  :: BOOKMARKS
                      , timer      :: TIMER
                      , console    :: CONSOLE
                      , testOutput :: TESTOUTPUT
                      , avar       :: AVAR
                      | e
                      ) Unit
main = runTest do
  suite "bookmarks" do
    test "create a bookmark" $ timeout 200 do
      let title   = "[PURESCRIPT-BOOKMARKS-TEST]"
      let url     = eitherToMaybe (runParseURI "https://github.com/romac/purescript-bookmarks")
      let details = { parentId : Nothing
                    , index    : Nothing
                    , title    : Just title
                    , url      : url
                    }

      bookmark <- Bookmarks.create details
      success
      {-- case bookmark of --}
      {--   Bookmark b -> do --}
      {--     assert "title is correct" $ b.title == title --}
      {--     assert "uri is correct" $ Just b.url == url --}
      {--     success --}

      {--   _ -> failure "not a Bookmark" --}

eitherToMaybe :: forall e a. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

