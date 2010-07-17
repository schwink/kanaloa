
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

/// Top-level user-facing abstraction of all Kanaloa client functionality.
function KanaloaConnection(server) {
    this._server = server;
    this.OnReceive = function(data) { }
    this.OnDebugEvent = function(message) { }
}

KanaloaConnection.prototype._ReportReceive = function(data) {
    if (this.OnReceive) {
	this.OnReceive(data);
    }
}

KanaloaConnection.prototype._LogDebug = function(message) {
    if (this.OnDebugEvent) {
	this.OnDebugEvent(message);
    }
}

KanaloaConnection.prototype.Send = function(data) {
    var connection = this;
    var post = new KanaloaHttpPost(this._server,
				   function(data) { connection._ReportReceive(data); },
				   function(message) { connection._LogDebug(message); }
				   );
    post.Send(data);
}

/// Wraps XmlHttpRequest to provide lowest-level send and receive functionality.
/// server -- The full URL to post to.
/// onReceiveChunk -- On stream-capable browsers, this is fired once per chunk. Otherwise, once per request.
/// onDebugEvent -- Reports interesting diagnostic information.
function KanaloaHttpPost(server, onReceiveChunk, onDebugEvent) {
    this._server = server;
    this._request = null;

    this._onReceiveChunk = onReceiveChunk;
    this._onDebugEvent = onDebugEvent;
}

KanaloaHttpPost.prototype._ReportChunk = function(data) {
    if (this._onReceiveChunk) {
	this._onReceiveChunk(data);
    }
}

KanaloaHttpPost.prototype._LogDebug = function(message) {
    if (this._onDebugEvent) {
	this._onDebugEvent(message);
    }
}

KanaloaHttpPost.prototype.Connect = function() {
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
    request.lenReceived = 0;
    request.open("POST", this._server);
    
    request.onreadystatechange = function() {
	var readyState = request.readyState;
	connection._LogDebug("State changed to " + readyStates[request.readyState]);
	
	connection._LogDebug("responseText is \"" + request.responseText + "\"");
	
	var headers = request.getAllResponseHeaders();
	connection._LogDebug("AllResponseHeaders is \"" + headers + "\"");
	
	if (readyState == READYSTATE_LOADING || readyState == READYSTATE_DONE) {
	    var allData = request.responseText;
	    var data = allData.substring(request.lenReceived);
	    request.lenReceived = allData.length;
	    if (data.length > 0) {
		connection._ReportChunk(data);
	    }
	}
    }
    
    // Setting headers seems to cause an OPTIONS request to be sent first.
    //request.setRequestHeader("Content-Type", "application/json");
    
    return true;
}

KanaloaHttpPost.prototype.Send = function(data) {
    this.Connect();
    this._request.send(body);
}
