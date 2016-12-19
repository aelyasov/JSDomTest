//"use strict";

const fs         = require('fs');
const url        = require("url");
const http       = require("http");
const path       = require("path");
const jsdom      = require("jsdom");
const _          = require('underscore');
const instrument = require("./instrumentLib.js");
const winston    = require('winston');

// winston.level = 'info';
winston.level = 'debug';
winston.remove(winston.transports.Console);
winston.add(winston.transports.Console, {'timestamp': () => (new Date()).toJSON()});


let jsSig, jsFun, environment, window, document;

environment = { tags: new Set(),
		names: new Set(),
		ids: new Set(),
		classes: new Set(),
		selectors: new Set()
	      };

document = jsdom.jsdom();
window = document.defaultView;
require('./domIntercept.js')(window, environment, winston);


http.createServer(function(request, response) {
    let pathname = url.parse(request.url, true).pathname;
    winston.debug("Received request: ", pathname);
    
    if (pathname == "/init") {
	request.on('data', function(data) {
	    // winston.debug("Received the initial function:", data.toString());

	    let rawJsFun = JSON.parse(data).jsFun;
	    winston.debug("jsFun before eval:\n", rawJsFun);

	    let fullJsSig = JSON.parse(data).jsSig;
	    jsSig = JSON.parse(data).jsSig.slice(1);
	    winston.debug("test function signature:\n", fullJsSig);

	    const library = "var instrument = require(\"./instrumentLib.js\");\n";
	    //jsFun = library + rawJsFun;
	    jsFun = rawJsFun;	    
	});
	response.writeHead(200, {"Content-Type": "text/plain"});
	response.write("init response");
	response.end();    
    }

    if (pathname == "/mutation") {
	request.on('data', function(data) {
	    //winston.debug("Received the mutated function:\n", data.toString());
	    winston.debug("# New mutation iteration: ", JSON.parse(data).mutN);
	});
	response.writeHead(200, {"Content-Type": "text/plain"});
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

	    document = jsdom.jsdom(jsFunDom);
	    window = document.defaultView;
	    
	    let _K_ = 1;
	    let branchDistance = [];
	    let trace = [1];
	    let loopMap = {};

	    // Commented out to disable excessive logging 
	    // winston.debug("jsFun:\n", jsFun);
	    
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

	    let envArray = Object.keys(environment).reduce(function(previous, current) {
		previous[current] = Array.from(environment[current]);
		return previous;
	    }, {});
	    
	    winston.debug("environment: ", envArray);

	    let genetic_response = JSON.stringify({
		trace: trace,
		branchDistance: branchDistance,
		loopMap: loopMap,
		environment: envArray
	    });
	    winston.debug("genetic response: ", genetic_response);
	    response.writeHead(200, {"Content-Type": "text/plain"});
	    response.write(genetic_response);
	    response.end();   
	});
    };
}).listen(7777);
winston.debug("Server Running on port: 7777");