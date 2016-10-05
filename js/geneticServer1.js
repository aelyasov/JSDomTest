// location of node4: /usr/local/bin/node
// /usr/local/bin/node geneticServer1.js

var fs         = require('fs'),
    url        = require("url") ,
    http       = require("http"),
    path       = require("path"),
    jsdom      = require("jsdom"),
    _          = require('underscore'),
    instrument = require("./instrumentLib.js"),
    winston    = require('winston');
    // logger     = require('./logger.js');    

var testContext = fs.readFileSync("./globalTestContext.js", "utf-8");

var jsFun, jsMutFun, jsFunArgs, jsSig, jsFunDom, realJSFunArgs;
// winston.level = 'info';
winston.level = 'debug';

http.createServer(function(request, response){
    
    winston.debug("I got kicked");
    
    var pathname = url.parse(request.url,true).pathname;

    winston.debug("pathname: ", pathname);

    if (pathname == "/init") {
	request.on('data', function(data) {
	    winston.debug("Received the initial function:", data.toString());
	    
	    // the function has to be encompassed by parencess to be evaluated by JavaScript
	    // jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	    winston.debug("jsFun before eval:\n", JSON.parse(data).jsFun);
	    jsSig = JSON.parse(data).jsSig.slice(1);
	    winston.debug("test function signature:\n", jsSig);
	    var library = "var instrument = require(\"./instrumentLib.js\");\n"
	    jsFun = library + JSON.parse(data).jsFun;
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("init response");
	response.end();    
    }


    if (pathname == "/mutation") {
	request.on('data', function(data) {
	    winston.debug("Received the mutated function:\n", data.toString());
	    winston.debug("# New mutation iteration: ", JSON.parse(data).mutN);
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("mutation response");
	response.end();    
    }
    

    if (pathname == "/genetic") {
	request.on('data', function(data) {
	    winston.debug("Received arguments which are new population:", data.toString());

	    // the function has to be encompassed by parencess to be evaluated by JavaScript
	    // jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	    jsFunArgs = JSON.parse(data).jsFunArgs.split("<|>");
	    jsFunDom = jsFunArgs[0];
	    realJSFunArgs = _.zip(jsSig, jsFunArgs.slice(1)).map(function(arg) { return instrument.convertArg(arg[0], arg[1])} )
	    winston.debug("jsFunArgs: ", jsFunArgs);

	    //var intercept = fs.readFileSync("/home/alex/PROJECTS/FITTEST/Software/UtrechtUniv/tools/JSDomTest/js/interceptTest.js", "utf-8");

 	    jsdom.env({
		html: jsFunDom,
		scripts: ["http://code.jquery.com/jquery.js"],
		done: function (error, window){
		    
		    var document = window.document;
		    var environment = {tags:[], names:[], ids:[], classes:[], selectors:[]};
		    
		    var _getElementById = document.getElementById;
		    document.getElementById = function() {
			winston.debug("id", arguments[0]);
			environment.ids = environment.ids.concat(arguments[0]);
			return _getElementById.apply(this, arguments);
		    };

		    _getElementsByClassName = document.getElementsByClassName;
		    document.getElementsByClassName = function() {
			winston.debug("class", arguments[0]);
			environment.classes = environment.classes.concat(arguments[0].split(" "));
			return _getElementsByClassName.apply(this, arguments);
		    };
		    
		    _getElementsByName =  document.getElementsByName;
		    document.getElementsByName = function() {
			winston.debug("name", arguments[0]);
			environment.names = environment.names.concat(arguments[0]);
			return _getElementsByName.apply(this, arguments);
		    };
		    
		    _getElementsByTagName = document.getElementsByTagName;
		    document.getElementsByTagName = function() {
			winston.debug("tag", arguments[0]);
			environment.tags = environment.tags.concat(arguments[0]);
			return _getElementsByTagName.apply(this, arguments);
		    };
		    

		    // the hook for getElementsByTagNameNS ignores information about namespace
		    _getElementsByTagNameNS = document.getElementsByTagNameNS;
		    document.getElementsByTagNameNS = function() {
			winston.debug("tag", arguments[1]);
			environment.tags = environment.tags.concat(arguments[1]);
			return _getElementsByTagNameNS.apply(this, arguments);
		    };

		    var _K_ = 1;
		    var branchDistance = [];
		    var trace = [1];
		    var loopMap = {};

		    winston.debug("jsFun:\n", jsFun);
		    eval(jsFun);

		    // Makes the global test context available for the tested code
		    eval(testContext)
		    
		    try {
			// test.call(window, jsFunArgs[1], window, document);
			test.apply(window, realJSFunArgs, window, document);
		    } catch (e) {
			trace.push(-100); // label -100 indicates exceptional termination
			winston.debug("Test function is exceptionally terminated with the message: %s", e.stack)
		    }
		    
		    winston.debug("trace", trace);
		    winston.debug("branchDistance", branchDistance);
		    winston.debug("loopMap", loopMap);
		    winston.debug("environment: ", environment);
		    response.writeHeader(200, {"Content-Type": "text/plain"});
		    response.write(JSON.stringify({trace:trace, branchDistance: branchDistance, loopMap: loopMap, environment: environment}));
		    response.end();    
		}
	    });
	});
    };

    if (pathname == "/execute") {
	request.on('data', function(data) {
	    winston.debug("Received genetically approved arguments", data.toString()); 
	    jsFunArgs = JSON.parse(data).jsArgs.split("<|>"),	    
	    winston.debug("jsFunArgs: ", jsFunArgs);
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("execution response");
	response.end();    
    }
    
}).listen(7777);
winston.debug("Server Running on 7777");
