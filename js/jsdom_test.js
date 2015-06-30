var jsdom    = require("jsdom");
var jquery   = require('jquery');
var assert   = require('assert');
var testUtil = require("./Utils.js").JSDomTestUtils;
var contrLib = require("./JSContractsLib").JSContractsLib;

// jsdom.env(
//   '<body><p>Hello</p><p>cruel</p><p>World</p></body>',
//   ["http://code.jquery.com/jquery.js"],
//   function (errors, window) {
//     var doc1 = window.document // window.document.cloneNode(true);;
//     var tests1 = testUtil.JSDomTestUtils.getAllIdPairs(doc1, 3);
//     var tests2 = testUtil.JSDomTestUtils.getAllIdPairs(doc1, 3)
//     // console.log(testUtil.JSDomTestUtils.getAllIdPairs(doc1, 3));
//     console.log("doc1:", jsdom.serializeDocument(doc1));
//     console.log(JSON.stringify(tests2));
//     console.log("tests1 == tests2: " + tests1 === tests2);
//     var doc2 = window.docuemnt;
//     // console.log("doc1 ? doc2: " + (doc1 == doc2));
//   }
// );


jsdom.env(
'<!DOCTYPE html><html><head></head><body><p>Hello</p><p>cruel</p><p>World</p></body></html>',
  [],
  //["http://code.jquery.com/jquery.js"],
  function (errors, window) {
    var docGlobal = window.document;
    var receivers = testUtil.getAllIdPairs(docGlobal, 2);
    var arguments = testUtil.getAllIdPairs(docGlobal, 1);
    var mkTest = function(fun, oRec, fArgs) {
      console.log("new test attempt: ", oRec);
      return jsdom.env(
        jsdom.serializeDocument(docGlobal),
        [],
        //["http://code.jquery.com/jquery.js"],
        function (err, win) {
          var doc_ = win.document;
          console.log("========================================");
          console.log("old_doc size:", contrLib.countDomElements(doc_));
          var oldLen = contrLib.countDomElements(doc_);
          console.log("old_doc:", jsdom.serializeDocument(doc_));
          var $ = jquery(win);
          var selRec = testUtil.mkFunTarget(doc_, oRec);
          var selArgs = testUtil.mkFunTarget(doc_, fArgs);
          // fun.call($(selRec), $(selArgs));
          fun.call($(this)).call($(selRec), $(selArgs));
          //$("#0").wrap($("#0"));
          // fun.call($(this)).call($("#0"), $("#0"));
          //console.log("test done");
          console.log( "new_doc size:", contrLib.countDomElements(doc_));
          var newLen = contrLib.countDomElements(doc_);
          console.log("new_doc:", jsdom.serializeDocument(doc_));
          assert(oldLen < newLen, "New DOM is smaller than the old one.");
        }
      );
    };
    arguments.map(
      function(args){
        return receivers.map(function(ids) { return mkTest(function() {return this.wrap}, ids, args)})}
    );
  }
);
