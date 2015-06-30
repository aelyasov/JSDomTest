var fs         = require('fs'),
    url        = require("url") ,
    http       = require("http"),
    path       = require("path"),
    jsdom      = require("jsdom"),
    instrument = require("./instrumentLib.js");
    // istanbul = require('istanbul'),
    // vm       = require('vm');

var jsFun, jsMutFun, jsFunArgs;

http.createServer(function(request, response){
    
    console.log("I got kicked");
    
    var data_ = '{ "jsFun":"function safeAdd(frameid) {var iframe = document.createElement(\\"iframe\\");var anchor = document.getElementById(\\"node\\");var frame = document.getElementById(frameid);iframe.setAttribute(\\"id\\",frameid);if (frame) {instrument._trace_.push(6);instrument._branchDistance_.push([6,Number(instrument._K_)]);instrument._trace_.push(7);frame.parentNode.removeChild(frame);} else {instrument._trace_.push(6);instrument._branchDistance_.push([6,_Number(K_)]);instrument._trace_.push(9);iframe.appendChild(anchor);}}", "jsMutFun":"var _K_ = 0;var _branchDistance_ = [];var _trace_ = [];function safeAdd(frameid,document) {var iframe = document.createElement(\\"iframe\\");var anchor = document.getElementById(\\"node\\");var frame = document.getElementById(frameid);iframe.setAttribute(\\"id\\",frameid);if (frame) {_branchDistance_.push([6,!_K_]);_trace_.push(7);frame.parentNode.removeChild(frame);} else {_branchDistance_.push([6,_K_]);_trace_.push(9);iframe.appendChild(anchor);}}"}';   

    // console.log("data_", JSON.parse(data_));
    
    // var my_url = url.parse(request.url);
    var queryObject = url.parse(request.url,true).query;
    console.log("queryObject: ", queryObject);

    if (queryObject.init == "true") {
	request.on('data', function(data) {
	    console.log("Received the initial function:", data.toString());
	    
	    // the function has to be encompassed by parencess to be evaluated by JavaScript
	    // jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	    console.log("jsFun before eval: ", JSON.parse(data).jsFun);
	    eval(JSON.parse(data).jsFun);
	    // jsMutFun = eval(JSON.parse(data).jsMutFun);	    
	    
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("init response");
	response.end();    
    }


    if (queryObject.mutation == "true") {
	request.on('data', function(data) {
	    console.log("Received the mutated function:", data.toString());
	    console.log("# New mutation iteration: ", JSON.parse(data).mutN);
	    console.log("jsMutFun before eval:", JSON.parse(data).jsMutFun);
	    //eval(JSON.parse(data).jsMutFun);
	    jsMutFun = JSON.parse(data).jsMutFun;
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("mutation response");
	response.end();    
    }
    

    if (queryObject.genetic == "true") {
	request.on('data', function(data) {
	    console.log("Received arguments which are new population:", data.toString());
	    
	    // the function has to be encompassed by parencess to be evaluated by JavaScript
	    // jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	    jsFunArgs = JSON.parse(data).jsFunArgs.split("<|>"),	    
	    console.log("jsFunArgs: ", jsFunArgs);

	    var intercept = fs.readFileSync("/home/alex/PROJECTS/FITTEST/Software/UtrechtUniv/tools/JSDomTest/js/interceptTest.js", "utf-8");

 	    jsdom.env({
		html: jsFunArgs[0],
		scripts: ["http://code.jquery.com/jquery.js"], 
		// __dirname + '/intercept.js'
		// features: { 
		//     FetchExternalResources: ["script"], 
		//     ProcessExternalResources: ["script"]
		// },
		done: function (error, window){
		    
		    var document = window.document;
		    var environment = {tags:[], names:[], ids:[], classes:[], selector:[]};
		    
		    var _getElementById = document.getElementById;
		    document.getElementById = function() {
			console.log("id", arguments[0]);
			environment.ids = environment.ids.concat(arguments[0]);
			return _getElementById.apply(this, arguments);
		    };
		    
		    // eval(JSON.parse(data_).jsFun);
		    console.log("jsMutFun", jsMutFun);
		    eval(jsMutFun);

		    var old_doc = window.document;
		    console.log("old_doc: ", jsdom.serializeDocument(old_doc));
		    safeAdd.call(window, jsFunArgs[1], window, document);
		    console.log("_trace_", instrument._trace_);
		    console.log(" _branchDistance_",  instrument._branchDistance_);
		    var new_doc = window.document;
		    console.log("new_doc: ", jsdom.serializeDocument(new_doc));
		    console.log("environment: ", environment);
		    response.writeHeader(200, {"Content-Type": "text/plain"});
		    response.write(JSON.stringify({_trace_:instrument._trace_,  _branchDistance_: instrument._branchDistance_}));
		    response.end();    
		}
	    });
	});
    };

    if (queryObject.execute == "true") {
	request.on('data', function(data) {
	    console.log("Received genetically approved arguments", data.toString()); 
	    jsFunArgs = JSON.parse(data).jsArgs.split("<|>"),	    
	    console.log("jsFunArgs: ", jsFunArgs);
	});
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("execution response");
	response.end();    
    }
    
}).listen(8888);
console.log("Server Running on 8888");
