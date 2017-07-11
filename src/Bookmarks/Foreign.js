"use strict";

function call0(method, err) {
  return function() {
    return function(onError, onSuccess) {
      chrome.bookmarks[method](function(res) {
        if (res != null) {
          onSuccess(res);
        } else {
          onError(new Error(err));
        }
      });
    };
  };
}

function call1(method, err) {
  return function(arg) {
    return function(onError, onSuccess) {
      chrome.bookmarks[method](arg, function(res) {
        if (res != null) {
          onSuccess(res);
        } else {
          onError(new Error(err));
        }
      });
    };
  };
}

function call2(method, err) {
  return function(arg1) {
    return function(arg2) {
      return function(onError, onSuccess) {
        chrome.bookmarks[method](arg1, arg2, function(res) {
          if (res != null) {
            onSuccess(res);
          } else {
            onError(new Error(err));
          }
        });
      };
    };
  };
}

exports.getImpl         = call1('get', 'Could not get bookmark');
exports.getManyImpl     = call1('get', 'Could not get bookmarks');
exports.getChildrenImpl = call1('getChildren', 'Could not get children');
exports.getRecentImpl   = call1('getRecent', 'Could not get recent');
exports.getTreeImpl     = call0('getTree', 'Could not get tree');
exports.getSubTreeImpl  = call1('getSubTree', 'Could not get sub-tree');
exports.searchImpl      = call1('search', 'Could not perform search');
exports.createImpl      = call1('create', 'Could not create bookmark');
exports.moveImpl        = call2('move', 'Could not move bookmark');
exports.updateImpl      = call2('update', 'Could not update bookmark');
exports.removeImpl      = call1('remove', 'Could not remove bookmark');
exports.removeTreeImpl  = call1('removeTree', 'Could not remove tree');
