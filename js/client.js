var http     = require('http');
var jsdom    = require("jsdom");
var jquery   = require('jquery');
var testUtil = require("./Utils.js");
var contrLib = require("./JSContractsLib");

var options = {
  hostname: 'localhost',
  port: 8000,
  path: '/',
  method: 'POST'
};

var req = http.request(options, function(res) {
  console.log('STATUS: ' + res.statusCode);
  console.log('HEADERS: ' + JSON.stringify(res.headers));
  // res.setEncoding('utf8');
  res.on('data', function (chunk) {
    console.log("chunk", chunk.toString());
    runTests(chunk.toString());
  }
);
});

req.on('error', function(e) {
  console.log('problem with request: ' + e.message);
});

// write data to request body
// req.write('data\n');
// req.write('data\n');
req.end();


var runTests = function(genDoc) {
  jsdom.env(
    genDoc,
    [],
    //["http://code.jquery.com/jquery.js"],
    function (errors, window) {
      var docGlobal = window.document;
      var receivers = testUtil.JSDomTestUtils.getAllIdPairs(docGlobal, 2);
      var arguments = testUtil.JSDomTestUtils.getAllIdPairs(docGlobal, 1);
      var mkTest = function(fun, oRec, fArgs) {
        console.log("new test attempt: ", oRec);
        return jsdom.env(
          jsdom.serializeDocument(docGlobal),
          [],
          //["http://code.jquery.com/jquery.js"],
          function (err, win) {
            var doc_ = win.document;
            console.log("========================================");
            console.log("doc_old:", jsdom.serializeDocument(doc_));
            var $ = jquery(win);
            var selRec = testUtil.JSDomTestUtils.mkFunTarget(doc_, oRec);
            var selArgs = testUtil.JSDomTestUtils.mkFunTarget(doc_, fArgs);
            // fun.call($(selRec), $(selArgs));
            fun.call($(this)).call($(selRec), $(selArgs));
            //$("#0").wrap($("#0"));
          // fun.call($(this)).call($("#0"), $("#0"));
            //console.log("test done");
            console.log("doc_new:", jsdom.serializeDocument(doc_));
          }
        );
      };
      arguments.map(
        function(args){
          return receivers.map(function(ids) { return mkTest(function() {return this.wrap;}, ids, args);});}
      );
    }
  );
};
