//"use strict";

const fs         = require('fs');
const url        = require("url");
const http       = require("http");
const path       = require("path");
const jsdom      = require("jsdom");
const _          = require('underscore');
const instrument = require("./instrumentLib.js");
const winston    = require('winston');

winston.level = 'info';
//winston.level = 'debug';

let jsSig, jsFun;

http.createServer(function(request, response) {
    winston.debug("I got kicked");
    
    let pathname = url.parse(request.url, true).pathname;
    winston.debug("pathname: ", pathname);
    
    if (pathname == "/init") {
	request.on('data', function(data) {
	    // winston.debug("Received the initial function:", data.toString());

	    let rawJsFun = JSON.parse(data).jsFun;
	    winston.debug("jsFun before eval:\n", rawJsFun);

	    jsSig = JSON.parse(data).jsSig.slice(1);
	    winston.debug("test function signature:\n", jsSig);

	    const library = "var instrument = require(\"./instrumentLib.js\");\n";
	    jsFun = library + rawJsFun;
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("init response");
	response.end();    
    }

    if (pathname == "/mutation") {
	request.on('data', function(data) {
	    //winston.debug("Received the mutated function:\n", data.toString());
	    winston.debug("# New mutation iteration: ", JSON.parse(data).mutN);
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("mutation response");
	response.end();    
    }

    if (pathname == "/genetic") {
	request.on('data', function(data) {
	    //winston.debug("Received arguments which are new population:", data.toString());
	    
	    let jsFunArgs = JSON.parse(data).jsFunArgs.split("<|>");
	    winston.debug("jsFunArgs: ", jsFunArgs);
	    
	    let jsFunDom = jsFunArgs[0];
	    let realJSFunArgs = _.zip(jsSig, jsFunArgs.slice(1)).map( arg => instrument.convertArg(arg[0], arg[1]) );

	    let doc = jsdom.jsdom(jsFunDom);
	    let window = doc.defaultView;
	    let document = window.document;
	    let environment = {tags:[], names:[], ids:[], classes:[], selectors:[]};

	    require('./domIntercept.js')(doc, environment);

	    let _K_ = 1;
	    let branchDistance = [];
	    let trace = [1];
	    let loopMap = {};
	    
	    winston.debug("jsFun:\n", jsFun);
	    eval(jsFun);

	    try {
		Reflect.apply(test, this, []);
		// test.apply(this, realJSFunArgs, window, document);
	    } catch (e) {
		trace.push(-100); // label -100 indicates exceptional termination
		winston.debug("Test function is exceptionally terminated with the message: %s", e.stack);
	    }
		    
	    winston.debug("trace", trace);
	    winston.debug("branchDistance", branchDistance);
	    winston.debug("loopMap", loopMap);
	    winston.debug("environment: ", environment);
	    response.writeHeader(200, {"Content-Type": "text/plain"});
	    response.write(JSON.stringify({trace:trace, branchDistance: branchDistance, loopMap: loopMap, environment: environment}));
	    response.end();   
	});
    };
}).listen(7777);
winston.debug("Server Running on 7777");
