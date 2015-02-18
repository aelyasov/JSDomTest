var request = require("request");
 
request({
    uri: "localhost:8000",
    method: "POST",
    form: {
	name: "Bob"
    }
}, function(error, response, body) {
    console.log(body);
});
