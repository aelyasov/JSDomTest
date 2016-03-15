// location of node4: /usr/local/bin/node
// /usr/local/bin/node geneticServer1.js

var fs         = require('fs'),
    url        = require("url") ,
    http       = require("http"),
    path       = require("path"),
    jsdom      = require("jsdom"),
    instrument = require("./instrumentLib.js"),
    winston    = require('winston');
    // logger     = require('./logger.js');    
    

var jsFun, jsMutFun, jsFunArgs;
winston.level = 'info';

http.createServer(function(request, response){
    
    winston.info("I got kicked");
    
    var data_ = '{ "jsFun":"function safeAdd(frameid) {var iframe = document.createElement(\\"iframe\\");var anchor = document.getElementById(\\"node\\");var frame = document.getElementById(frameid);iframe.setAttribute(\\"id\\",frameid);if (frame) {instrument._trace_.push(6);instrument._branchDistance_.push([6,Number(instrument._K_)]);instrument._trace_.push(7);frame.parentNode.removeChild(frame);} else {instrument._trace_.push(6);instrument._branchDistance_.push([6,_Number(K_)]);instrument._trace_.push(9);iframe.appendChild(anchor);}}", "jsMutFun":"var _K_ = 0;var _branchDistance_ = [];var _trace_ = [];function safeAdd(frameid,document) {var iframe = document.createElement(\\"iframe\\");var anchor = document.getElementById(\\"node\\");var frame = document.getElementById(frameid);iframe.setAttribute(\\"id\\",frameid);if (frame) {_branchDistance_.push([6,!_K_]);_trace_.push(7);frame.parentNode.removeChild(frame);} else {_branchDistance_.push([6,_K_]);_trace_.push(9);iframe.appendChild(anchor);}}"}';   

    // console.log("data_", JSON.parse(data_));
    
    // var my_url = url.parse(request.url);
    var queryObject = url.parse(request.url,true).query;
    winston.debug("queryObject:", queryObject);

    if (queryObject.init == "true") {
	request.on('data', function(data) {
	    winston.debug("Received the initial function:", data.toString());
	    
	    // the function has to be encompassed by parencess to be evaluated by JavaScript
	    // jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	    winston.debug("jsFun before eval:\n", JSON.parse(data).jsFun);
	    var library = "var instrument = require(\"./instrumentLib.js\");\n"
	    jsFun = library + JSON.parse(data).jsFun;
	    
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("init response");
	response.end();    
    }


    if (queryObject.mutation == "true") {
	request.on('data', function(data) {
	    winston.debug("Received the mutated function:\n", data.toString());
	    winston.debug("# New mutation iteration: ", JSON.parse(data).mutN);
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("mutation response");
	response.end();    
    }
    

    if (queryObject.genetic == "true") {
	request.on('data', function(data) {
	    winston.debug("Received arguments which are new population:", data.toString());

	    // the function has to be encompassed by parencess to be evaluated by JavaScript
	    // jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	    jsFunArgs = JSON.parse(data).jsFunArgs.split("<|>"),	    
	    winston.debug("jsFunArgs: ", jsFunArgs);

	    //var intercept = fs.readFileSync("/home/alex/PROJECTS/FITTEST/Software/UtrechtUniv/tools/JSDomTest/js/interceptTest.js", "utf-8");

 	    jsdom.env({
		html: jsFunArgs[0],
		scripts: ["http://code.jquery.com/jquery.js"], 
		done: function (error, window){
		    
		    var document = window.document;
		    var environment = {tags:[], names:[], ids:[], classes:[], selector:[]};
		    
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
		    var _branchDistance_ = [];
		    var _trace_ = [1];

		    winston.info("jsFun:\n", jsFun);
		    eval(jsFun);

		    try {
			test.call(window, jsFunArgs[1], window, document);
		    } catch (e) {
			_trace_.push(-100); // label -100 indicates exceptional termination
			winston.log("info", "Test function is exceptionally terminated with the message: %s", e.stack)
		    }
		    
		    winston.info("_trace_", _trace_);
		    winston.debug(" _branchDistance_",  _branchDistance_);
		    winston.debug("environment: ", environment);
		    response.writeHeader(200, {"Content-Type": "text/plain"});
		    response.write(JSON.stringify({_trace_:_trace_,  _branchDistance_: _branchDistance_}));
		    response.end();    
		}
	    });
	});
    };

    if (queryObject.execute == "true") {
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
winston.info("Server Running on 7777");
