var http     = require('http');
var jsdom    = require("jsdom");
// var jquery   = require('jquery');
var testUtil = require("./Utils.js");
var contrLib = require("./JSContractsLib");

var $ = require('jquery'),
    XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest;


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
    res.on('data', function (data) {
	
	console.log("data", data.toString());
	
	var funJS, funSigJS, mfunJS, mposJS, fitScore;
	funJS    = JSON.parse(data).funJS;
	funSigJS = JSON.parse(data).funSigJS;
	mfunJS   = JSON.parse(data).mfunJS;
	mposJS   = JSON.parse(data).mposJS;

	console.log("funJS", funJS);
	console.log("funSigJS", funSigJS);
	console.log("mfunJS", mfunJS);
	console.log("mposJS", mposJS);

	console.log(eval(JSON.parse(data).funJS));

	// $.ajax({
	//     type: "POST",
	//     url: "http://localhost:8000/getstring",
	//     data: { name: "John", location: "Boston" }
	// }).done(function( msg ) {
	//     alert( "Data Saved: " + msg );
	// });


	var xmlhttp = new XMLHttpRequest();
	xmlhttp.open("GET","http://localhost:8000/rungenetic/?fitscore=yes", true);
	// xmlhttp.setRequestHeader("Content-type","text/plain");
	xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	// xmlhttp.setRequestHeader("Content-length", params.length);
	// xmlhttp.setRequestHeader("Connection", "close");
	xmlhttp.onreadystatechange=function(){
            if (xmlhttp.readyState==4 && xmlhttp.status==200){
		console.log("responseType", xmlhttp.responseType);
		console.log("ajax:", xmlhttp.responseText);
            }
	};
	xmlhttp.send();

	
	// $.support.cors = true;
	// $.ajaxSettings.xhr = function() {
	//     return new XMLHttpRequest();
	// };

	// $.open("GET","http://localhost:8000/getstring", true);
	// $.onreadystatechange=function(){
        //     if ($.readyState==4 && $.status==200){
	// 	string=$.responseText;
        //     }
	// };
	
	// $.send();
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

