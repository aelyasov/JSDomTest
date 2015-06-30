var sys      = require("sys"),
    http     = require("http"),
    jsdom    = require("jsdom");
    istanbul = require('istanbul'),
    vm       = require('vm');

var jsFun = {};

http.createServer(function(request, response){
    //sys.puts("I got kicked");
    //console.log(request.data);
    request.on('data', function(data) {
	console.log("Received body data:", data.toString());
	
	// the function has to be encompassed by parencess to be evaluated by JavaScript
	// jsFun     = jsFun ? eval("(" + JSON.parse(data).jsFun + ")") : jsFun,
	var jsFun     = eval("(" + JSON.parse(data).jsFun + ")"),
	    jsFunArgs = JSON.parse(data).jsFunArgs.split("<|>"),
	    html      = jsFunArgs.shift(); // shift the `this` object to extract html
	    
	console.log("jsFun: ", jsFun);
	jsFun = jsFun ? JSON.parse(data).jsFun : jsFun,
	console.log("jsFun: ", jsFun);
	    
	var instrumenter = new istanbul.Instrumenter();
	var jsFunInstr = instrumenter.instrumentSync(jsFun);
	console.log(jsFunInstr);
	// console.log(eval(jsFunInstr));
	eval(jsFunInstr);	
	// console.log("jsFun", jsFun);
	console.log("jsFunArgs: ", jsFunArgs);
	console.log("html: ", html);
	
	// "../Genetic/safeAdd.js"
	jsdom.env(html,[],
		 function (error, window){
		     var document = window.document;
		     var old_doc = window.document;
		     console.log("old_doc: ", jsdom.serializeDocument(old_doc));
		     //safeAdd.call(this, args)
		     /*
		      As a temporary solution we assume that the document and 
		      window objects are explicitly passed to the call of the function
		      under test (FUT). This requires the signature of the FUT also 
		      accommodate this requirement.
		      */
		     safeAdd.call(window, jsFunArgs, document);
		     //jsFun.call(window, args) // works if jsFun referes to window and document only by this
		     console.log(eval(instrumenter.currentState.trackerVar).traceMap);
		     var new_doc = window.document;
		     console.log("new_doc: ", jsdom.serializeDocument(new_doc));
		 });


    });
    response.writeHeader(200, {"Content-Type": "text/plain"});
    response.write("0.99");
    response.end();
}).listen(8888);
sys.puts("Server Running on 8888");
