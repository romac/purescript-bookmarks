"use strict";

function flip0(f) {
  return function(callback) {
    return function() {
      return f(callback);
    };
  };
}

function flip1(f) {
  return function(callback, arg) {
    return function() {
      return f(arg, callback);
    };
  };
}

function flip2(f) {
  return function(callback, arg1, arg2) {
    return function() {
      return f(arg1, arg2, callback);
    };
  };
}

exports.getImpl         = flip1(chrome.bookmarks.get);
exports.getManyImpl     = flip1(chrome.bookmarks.get);
exports.getChildrenImpl = flip1(chrome.bookmarks.getChildren);
exports.getRecentImpl   = flip1(chrome.bookmarks.getRecent);
exports.getTreeImpl     = flip0(chrome.bookmarks.getTree);
exports.getSubTreeImpl  = flip1(chrome.bookmarks.getSubTree);
exports.searchImpl      = flip1(chrome.bookmarks.search);
exports.createImpl      = flip1(chrome.bookmarks.create);
exports.moveImpl        = flip2(chrome.bookmarks.move);
exports.updateImpl      = flip2(chrome.bookmarks.update);
exports.removeImpl      = flip1(chrome.bookmarks.remove);
exports.removeTreeImpl  = flip1(chrome.bookmarks.removeTree);

