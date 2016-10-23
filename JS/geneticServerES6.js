"use strict";
const fs         = require('fs');
const url        = require("url");
const http       = require("http");
const path       = require("path");
const jsdom      = require("jsdom");
const _          = require('underscore');
const instrument = require("./instrumentLib.js");
const winston    = require('winston');

let document = jsdom.jsdom("<html><head></head><body><div id='test' class='class'><span>asd</span></div></body></html>");

// console.log("document.prototype: ", document.__proto__.__proto__.__proto__.__proto__);

let documentProxy = new Proxy({}, {
    get(target, propKey, receiver) {
	return function (...args) {
	    console.log(propKey + JSON.stringify(args));
	    return Reflect.apply(document.getElementById, document,  args);
        };
	// if (propKey == 'className') {
	//     console.log("className", receiver[propKey]);
	// }
	// const origMethod = receiver[propKey];
	// return function (...args) {
        //     const result = origMethod.apply(receiver, args);
	//     //Reflect.apply(target, propKey, args);
        //     console.log(propKey + JSON.stringify(args));
        //     return result;
        // };
    // return Reflect.get(target, propKey);
    }
});


// let documentProxy = new Proxy(document, {
//     get(target, propKey, receiver) {
// 	console.log("receiver", receiver);
// 	return Reflect.get(target, propKey, receiver);
//     }
//     // apply(target, receiver, args) {
//     // 	return Reflect.apply(target, receiver, args);
//     // }
    
// });

Object.setPrototypeOf(document.__proto__.__proto__.__proto__.__proto__, documentProxy);


// let environment = {tags:[], names:[], ids:[], classes:[], selectors:[]};

// let _getElementById = document.getElementById;
// document.getElementById = function() {
//     environment.ids = environment.ids.concat(arguments[0]);
//     return _getElementById.apply(this, arguments);
// };


// let _getElementsByTagName = document.getElementsByTagName;
// document.getElementsByTagName = function() {
//     environment.tags = environment.tags.concat(arguments[0]);
//     return _getElementsByTagName.apply(this, arguments);
// };


//let obj = document.getElementById('test');


// let spans = obj.getElementsByTagName('SPAN');
// console.log("spans", spans);
// console.log("environment", environment);
// console.log("obj", document.documentElement.outerHTML);
