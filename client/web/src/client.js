
const READYSTATE_UNSENT = 0;
const READYSTATE_OPENED = 1;
const READYSTATE_HEADERSRECEIVED = 2;
const READYSTATE_LOADING = 3;
const READYSTATE_DONE = 4;
readyStates = {};
readyStates[READYSTATE_UNSENT] = "UNSENT";
readyStates[READYSTATE_OPENED] = "OPENED";
readyStates[READYSTATE_HEADERSRECEIVED] = "HEADERS_RECEIVED";
readyStates[READYSTATE_LOADING] = "LOADING";
readyStates[READYSTATE_DONE] = "DONE";

function KanaloaConnection(server) {
    this._server = server;
    this._isConnected = false;

    this.OnDebugEvent = function(message) { };
}

KanaloaConnection.prototype._LogDebug = function(message) {
    if (this.OnDebugEvent) {
	this.OnDebugEvent(message);
    }
}

KanaloaConnection.prototype.Connect = function() {
    if (this._request) {
	if (this._request.readyState == READYSTATE_UNSENT) {
	    this._LogDebug("Connect: Using existing request.");
	    return false;
	}

	this._LogDebug("Connect: Aborting existing request.");
	this._request.abort();
	this._request = null;
    }
    
    this._LogDebug("Connect: Creating new request.");

    var connection = this;

    var request = new XMLHttpRequest();
    this._request = request;
    request.open("POST", this._server);
    
    request.onreadystatechange = function() {
	var readyState = readyStates[request.readyState];
	connection._LogDebug("State changed to " + readyState);
	
	connection._LogDebug("responseText is \"" + request.responseText + "\"");

	var headers = request.getAllResponseHeaders();
	connection._LogDebug("AllResponseHeaders is \"" + headers + "\"");
    }
    
    // Setting headers seems to cause an OPTIONS request to be sent first.
    //request.setRequestHeader("Content-Type", "application/json");
    
    return true;
}

KanaloaConnection.prototype.Send = function(data) {
    this.Connect();
    this._request.send(body);
}
