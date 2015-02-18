$(document).ready(function () {
    var loggable = function(obj, logger) {
	return Proxy.create({
	    get: function get(receiver, prop) {
		if (prop == 'getElementById') {
		    logger.info('Getting ' + prop);
		}
		return prop.call(obj, arguments);
	    }
	});
    };
    
    var doc = document;
    doc = loggable(doc, {
	info: function info(str) {
	    console.log(str);  
	}
    });
    
    // doc.getElementById(1);
    
    var environment = {tags:[], names:[], ids:[], classes:[], selector:[]}
    
    _getElementById = Document.prototype.getElementById;
    Document.prototype.getElementById = function() {
	console.log("id", arguments[0]);
	environment.ids = environment.ids.concat(arguments[0]);
	return _getElementById.apply(this, arguments);
    };
    
    _getElementsByClassName = Document.prototype.getElementsByClassName;
    Document.prototype.getElementsByClassName = function() {
	console.log("class", arguments[0]);
	environment.classes = environment.classes.concat(arguments[0].split(" "));
	return _getElementsByClassName.apply(this, arguments);
    };
    
    _getElementsByName =  HTMLDocument.prototype.getElementsByName;
    HTMLDocument.prototype.getElementsByName = function() {
	console.log("name", arguments[0]);
	environment.names = environment.names.concat(arguments[0]);
	return _getElementsByName.apply(this, arguments);
    };
    
    
    _getElementsByTagName = Document.prototype.getElementsByTagName;
    Document.prototype.getElementsByTagName = function() {
	console.log("tag", arguments[0]);
	environment.tags = environment.tags.concat(arguments[0]);
	return _getElementsByTagName.apply(this, arguments);
    };
    
    
    // the hook for getElementsByTagNameNS ignores information about namespace
    _getElementsByTagNameNS = Document.prototype.getElementsByTagNameNS;
    Document.prototype.getElementsByTagNameNS = function() {
	console.log("tag", arguments[1]);
	environment.tags = environment.tags.concat(arguments[1]);
	return _getElementsByTagNameNS.apply(this, arguments);
    };
    

    // console.log("getElementById", document.getElementById(1));
    var cn = "a" + " " + "b";
    // console.log("getElementsByClassName", document.getElementsByClassName(cn));
    // console.log("getElementsByName", document.getElementsByName("div1"));
    // console.log("getElementsByTagName", document.getElementsByTagName("div"));
    // console.log("getElementsByTagNameNS", document.getElementsByTagNameNS("", "div"));
    $("#a");
    console.log(environment);
});
