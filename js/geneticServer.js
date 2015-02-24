var sys = require("sys"),
    http = require("http");

var func = "function(x){return 2*x;}"

http.createServer(function(request,response){
	//sys.puts("I got kicked");
        //console.log(request.data);
    request.on('data', function(chunk) {
	console.log("Received body data:");
	console.log(chunk.toString());
    });
	response.writeHeader(200, {"Content-Type": "text/plain"});
	response.write("0.99");
	response.end();
}).listen(8888);
sys.puts("Server Running on 8888");
