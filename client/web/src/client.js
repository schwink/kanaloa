
readyStates = {};
readyStates[0] = "UNSENT";
readyStates[1] = "OPENED";
readyStates[2] = "HEADERS_RECEIVED";
readyStates[3] = "LOADING";
readyStates[4] = "DONE";

function post(address, body, logger) {
    var request = new XMLHttpRequest();
    
    request.open("POST", address);

    request.onreadystatechange = function() {
	var readyState = readyStates[request.readyState];
	logger("State changed to " + readyState);
	logger("responseText is \"" + request.responseText + "\"");
	var headers = request.getAllResponseHeaders();
	logger("AllResponseHeaders is \"" + headers + "\"");
    }
    
    // Setting headers causes an OPTIONS request to be sent first.
    //request.setRequestHeader("Content-Type", "application/json");
    var response = request.send(body);
    
    return request;
}
