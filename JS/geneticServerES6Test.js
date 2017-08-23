//"use strict";

const fs         = require('fs');
const jsdom      = require("jsdom");
const instrument = require("./instrumentLib.js");

let domFile = fs.readFileSync("domFile.txt", 'utf8');
console.log("domFile:", domFile);

let functionFile = fs.readFileSync("functionFile.txt", 'utf8');
console.log("functionFile:", functionFile);

let doc = jsdom.jsdom(domFile);
let window = doc.defaultView;
let document = window.document;
let environment = { tags: new Set(),
		    names: new Set(),
		    ids: new Set(),
		    classes: new Set(),
		    selectors: new Set()
		  };

require('./domIntercept.js')(doc, environment);

let _K_ = 1;
let branchDistance = [];
let trace = [1];
let loopMap = {};

//const library = "var instrument = require(\"./instrumentLib.js\");\n";
// let jsFun = library + functionFile;
let jsFun = functionFile;

eval(jsFun);

try {
    Reflect.apply(test, this, []);
    // test.apply(this, realJSFunArgs, window, document);
} catch (e) {
    trace.push(-100); // label -100 indicates exceptional termination
    console.log("Test function is exceptionally terminated with the message: %s", e.stack);
}

let envArray = Object.keys(environment).reduce(function(previous, current) {
    previous[current] = Array.from(environment[current]);
    return previous;
}, {});


console.log("trace", trace);
console.log("branchDistance", branchDistance);
console.log("loopMap", loopMap);
console.log("environment: ", envArray);


