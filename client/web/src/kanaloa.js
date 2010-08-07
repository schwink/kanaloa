
(function() {

var /*const*/ READYSTATE_UNSENT = 0;
var /*const*/ READYSTATE_OPENED = 1;
var /*const*/ READYSTATE_HEADERSRECEIVED = 2;
var /*const*/ READYSTATE_LOADING = 3;
var /*const*/ READYSTATE_DONE = 4;
var readyStates = {};
readyStates[READYSTATE_UNSENT] = "UNSENT";
readyStates[READYSTATE_OPENED] = "OPENED";
readyStates[READYSTATE_HEADERSRECEIVED] = "HEADERS_RECEIVED";
readyStates[READYSTATE_LOADING] = "LOADING";
readyStates[READYSTATE_DONE] = "DONE";

function stringTrim(str) {
    return str.replace(/^\s*/, "").replace(/\s*$/, "");
};

/// Top-level user-facing abstraction of all Kanaloa client functionality.
/// Parameters:
/// server -- The url of the Kanaloa service to connect to.
/// Methods:
/// send(data) -- Sends a JavaScript term to the server.
/// Properties:
/// onReceive -- Invoked when a message is received from the server. function(data)
/// onConnectionLost -- Invoked when an application-level timeout occurs. The server process that is handling this client has died. function()
this.KanaloaConnection = function(server) {
    this.settings = new _KanaloaHttpSettings();
    this.server = server;
    this.connectionId = null;
    
    this._receiver = null;
    this._sendBatcher = null;
    
    this.onReceive = function(data) { };
    this.onConnectionLost = function() { };
    this.onDebugEvent = function(message) { };
    
    this.connect();
};

KanaloaConnection.prototype._reportReceive = function(data) {
    if (this.onReceive) {
	this.onReceive(data);
    }
};

KanaloaConnection.prototype._reportConnectionLost = function() {
    if (this.onConnectionLost) {
	this.onConnectionLost();
    }
};

KanaloaConnection.prototype._logDebug = function(message) {
    if (this.onDebugEvent) {
	this.onDebugEvent("Connection: " + message);
    }
};

KanaloaConnection.prototype._bumpIncoming = function(statusCode) {
    if (statusCode == 410) {
	// GONE
	this.connectionId = null;
	this._reportConnectionLost();
    }
};

KanaloaConnection.prototype._bumpOutgoing = function(statusCode) {
    // Same thing.
    this._bumpIncoming(statusCode);
};

KanaloaConnection.prototype.connect = function() {
    var connection = this;
    connection._logDebug("Using stream mode? " + connection.settings.isStreamMode);
    
    function connectionOpened(receiverPost) {
	connection._logDebug("Opened");
	
	// Now that we have the ConnectionId, we can begin transmitting outgoing posts.
	connection.connectionId = receiverPost.connectionId;
	connection.send();
    }

    function connectionClosed(receiverPost, statusCode) {
	connection._logDebug("Closed with status: " + statusCode);
	
	connection._bumpIncoming(statusCode);
	connection.settings.bumpIncoming(statusCode);
	connection._logDebug("Waiting " + connection.settings.incomingWait + " ms before reconnect.");
	setTimeout(function() { connection.connect(); }, connection.settings.incomingWait);
    }
    
    var receiver = new _KanaloaHttpPost(this.server + "/" + this.settings.connectionSuffix,
				       this.connectionId,
				       this.settings.contentType,
				       this.settings.isStreamMode,
				       function() { connectionOpened(this); },
				       function(data) { connection._reportReceive(data); },
				       function(httpStatusCode) { connectionClosed(this, httpStatusCode); },
				       function(message) { connection._logDebug(message); }
				       );
    this._receiver = receiver;
    
    receiver.send("");
};

KanaloaConnection.prototype.send = function(data) {
    if (this._sendBatcher == null) {
	var connection = this;
	this._sendBatcher = new _KanaloaHttpSendBatcher(this,
						       function(message) { connection._logDebug(message); }
						       );
    }
    
    // Let's stringify the data now so it is immutable.
    var dataJson = JSON.stringify(data);
    this._sendBatcher.send(dataJson);
};

var /*const*/ KANALOA_WAIT_INCOMING_BASE = 10;
var /*const*/ KANALOA_WAIT_OUTGOING_BASE = 10;

/// Manages timeouts and other settings for the client.
function _KanaloaHttpSettings() {
    this.isStreamMode = this._isStreamMode();
    this.contentType = "application/json";

    if (this.isStreamMode) {
	this.connectionSuffix = "?t=stream";
    }
    else {
	this.connectionSuffix = "?t=longpoll";
    }
    
    this.reset();
}

_KanaloaHttpSettings.prototype._isStreamMode = function() {
    var userAgent = navigator.userAgent;
    return (userAgent.indexOf("MSIE") == -1);
};

_KanaloaHttpSettings.prototype.reset = function() {
    this.incomingWait = KANALOA_WAIT_INCOMING_BASE;
    this.outgoingWait = KANALOA_WAIT_OUTGOING_BASE;
};

_KanaloaHttpSettings.prototype.bumpIncoming = function(statusCode) {
    if (statusCode != 200) {
	if (this.incomingWait == KANALOA_WAIT_INCOMING_BASE) {
	    this.incomingWait = 1000;
	}
	else if (this.incomingWait < 100000) {
	    this.incomingWait *= 2;
	}
    }
};

_KanaloaHttpSettings.prototype.bumpOutgoing = function(statusCode) {
    if (statusCode != 200) {
	if (this.outgoingWait == KANALOA_WAIT_OUTGOING_BASE) {
	    this.outgoingWait = 1000;
	}
	else if (this.outgoingWait < 100000) {
	    this.outgoingWait *= 2;
	}
    }
};

/// Once a ConnectionId is established by the initial request, this class batches outgoing data.
function _KanaloaHttpSendBatcher(connection, onDebugEvent) {
    this._connection = connection;
    this._outgoing = [];
    this._post = null;

    this._onDebugEvent = onDebugEvent;
}

_KanaloaHttpSendBatcher.prototype._logDebug = function(message) {
    if (this._onDebugEvent) {
	this._onDebugEvent("HttpSendBatcher: " + message);
    }
};

_KanaloaHttpSendBatcher.prototype.send = function(data) {
    if (data) {
	this._logDebug("Adding data \"" + data + "\" to outbox.");
	this._outgoing.push(data);
    }
    
    this._sendPost();
};

_KanaloaHttpSendBatcher.prototype._sendPost = function() {
    if (this._outgoing.length == 0) {
	this._logDebug("No messages to send.");
	return;
    }
    
    if (this._connection.connectionId == null) {
	this._logDebug("ConnectionId not set yet.");
	return;
    }
    
    if (this._post && this._post.isActive()) {
	// If the post is still working, wait for it to complete.
	this._logDebug("A post is still active; waiting for it to complete.");
	return;
    }

    var batcher = this;
    var connection = batcher._connection;
    
    function postCompleted(post, statusCode) {
	// If successful, remove sent messages from outbox.
	if (statusCode == 200) {
	    for (var i = 0; i < post.sentCount; i++) {
		batcher._outgoing.shift();
	    }
	}

	// Loop to pick up accumulated messages.
	connection._bumpOutgoing(statusCode);
	connection.settings.bumpOutgoing(statusCode);
	connection._logDebug("Waiting " + connection.settings.outgoingWait + " ms before reconnect.");
	setTimeout(function() { batcher._sendPost(); }, connection.settings.outgoingWait);
    }
    
    this._logDebug("There is no active post; creating a new one.");
    // TODO: Reuse existing post or remove post reconnect logic.
    var post = new _KanaloaHttpPost(connection.server,
				   connection.connectionId,
				   connection.settings.contentType,
				   connection.settings.isStreamMode,
				   null,
				   null,
				   function(httpStatusCode) { postCompleted(this, httpStatusCode); },
				   function(message) { batcher._logDebug(message); }
				   );
    batcher._post = post;
    
    // TODO: Limit size of sent string to 1 MB to match server limit.
    var textOutgoing = "[";
    for (var i = 0; i < this._outgoing.length; i++) {
	if (i != 0) {
	    textOutgoing += ",";
	}
	var text = this._outgoing[i];
	textOutgoing += text;
    }
    textOutgoing += "]";

    this._logDebug("Sending batch \"" + textOutgoing + "\"");
    this._post.send(textOutgoing);
    this._post.sentCount = this._outgoing.length;
};

/// Wraps XmlHttpRequest to provide lowest-level send and receive functionality.
/// server -- The full URL to post to.
/// connectionId -- The value to set for the ConnectionId header.
/// httpContentType -- The value to set for the Content-Type header.
/// isStreamMode -- If false, will not try to read headers or responseText before request complete (IE compat == false).
/// onReceiveChunk -- On stream-capable browsers, this is fired once per chunk. Otherwise, once per request.
/// onClose -- Fired when the underlying request dies.
/// onDebugEvent -- Reports interesting diagnostic information.
function _KanaloaHttpPost(server, connectionId, httpContentType, isStreamMode, onOpen, onReceiveChunk, onClose, onDebugEvent) {
    this._server = server;
    this.connectionId = connectionId;
    this._contentType = httpContentType;
    this._isStreamMode = isStreamMode;
    this._request = null;

    this._onOpen = onOpen;
    this._onReceiveChunk = onReceiveChunk;
    this._onClose = onClose;
    this._onDebugEvent = onDebugEvent;
}

_KanaloaHttpPost.prototype._reportOpen = function() {
    if (this._onOpen) {
	this._onOpen();
    }
};

_KanaloaHttpPost.prototype._reportChunk = function(data) {
    if (this._onReceiveChunk) {
	this._onReceiveChunk(data);
    }
};

_KanaloaHttpPost.prototype._reportClose = function(httpStatusCode) {
    if (this._onClose) {
	this._onClose(httpStatusCode);
    }
};

_KanaloaHttpPost.prototype._logDebug = function(message) {
    if (this._onDebugEvent) {
	this._onDebugEvent("HttpPost: " + message);
    }
};

_KanaloaHttpPost.prototype.isActive = function() {
    if (this._request && (this._request.readyState != READYSTATE_UNSENT || this._request.readyState != READYSTATE_DONE)) {
	return false;
    }

    return true;
};

_KanaloaHttpPost.prototype.connect = function() {
    if (this._request) {
	if (this._request.readyState == READYSTATE_UNSENT) {
	    this._logDebug("Connect: Using existing request.");
	    return false;
	}

	this._logDebug("Connect: Aborting existing request.");
	this._request.abort();
	this._request = null;
    }
    
    this._logDebug("Connect: Creating new request.");

    var connection = this;

    var request = new XMLHttpRequest();
    this._request = request;
    request.lenReceived = 0;
    request.open("POST", this._server);
    
    request.onreadystatechange = function() {
	var readyState = request.readyState;
	connection._logDebug("State changed to " + readyStates[request.readyState]);
	
	//connection._logDebug("responseText is \"" + request.responseText + "\"");
	
	if ((connection._isStreamMode && readyState == READYSTATE_HEADERSRECEIVED) ||
	    (!connection._isStreamMode && readyState == READYSTATE_DONE)) {
	    var headers = request.getAllResponseHeaders();
	    connection._logDebug("AllResponseHeaders is \"" + headers + "\"");
	    
	    var newConnectionId = request.getResponseHeader("ConnectionId");
	    if (newConnectionId) {
		connection._logDebug("Set new ConnectionId \"" + newConnectionId + "\"");
		connection.connectionId = newConnectionId;
	    }
	    
	    connection._logDebug("status is \"" + request.status + "\"");

	    connection._reportOpen();
	}
	
	if ((connection._isStreamMode && readyState == READYSTATE_LOADING) ||
	    readyState == READYSTATE_DONE) {
	    var allData = request.responseText;
	    var data = allData.substring(request.lenReceived);
	    data = stringTrim(data);
	    request.lenReceived = allData.length;
	    if (data.length > 0) {
		connection._logDebug("Received additional responseText \"" + data + "\"");

		var responses = [];
		try {
		    // The response should always be a JSON array.
		    responses = JSON.parse(data);
		}
		catch (ex) {
		    connection._logDebug("Error parsing responseText \"" + data + "\"");
		}
		
		for (var i = 0; i < responses.length; i++) {
		    var response = responses[i];
		    connection._reportChunk(response);
		}
	    }
	}
	
	if (readyState == READYSTATE_DONE) {
	    connection._reportClose(request.status);
	}
    }
    
    if (this.connectionId) {
	connection._logDebug("Setting ConnectionId header to \"" + this.connectionId + "\"");
	request.setRequestHeader("ConnectionId", this.connectionId);
    }
    if (this._contentType) {
	connection._logDebug("Setting Content-Type header to \"" + this._contentType + "\"");
	request.setRequestHeader("Content-Type", this._contentType);
    }
    
    return true;
};

_KanaloaHttpPost.prototype.send = function(data) {
    this.connect();
    this._request.send(data);
};

})();
