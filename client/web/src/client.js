
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

String.prototype.trim = function () {
    return this.replace(/^\s*/, "").replace(/\s*$/, "");
}

/// Top-level user-facing abstraction of all Kanaloa client functionality.
function KanaloaConnection(server) {
    this._server = server;
    this._clientId = null;
    this._receiver = null;
    this._sendBatcher = null;

    this.OnReceive = function(data) { }
    this.OnDebugEvent = function(message) { }

    this.Connect();
}

KanaloaConnection.prototype._ReportReceive = function(data) {
    if (this.OnReceive) {
	this.OnReceive(data);
    }
}

KanaloaConnection.prototype._LogDebug = function(message) {
    if (this.OnDebugEvent) {
	this.OnDebugEvent("Connection: " + message);
    }
}

KanaloaConnection.prototype.Connect = function() {
    var connection = this;

    function ConnectionOpened(receiverPost) {
	connection._LogDebug("Opened");
	
	// Now that we have the ConnectionId, we can begin transmitting outgoing posts.
	connection._EnsureSendBatcher(); // Ensure that the batcher is created.
	connection._sendBatcher.SetConnectionId(receiverPost.ConnectionId);
    }

    var receiver = new KanaloaHttpPost(this._server,
				       null,
				       "application/json",
				       function() { ConnectionOpened(this); },
				       function(data) { connection._ReportReceive(data); },
				       function(httpStatusCode) { connection._LogDebug("Closed with status: " + httpStatusCode); },
				       function(message) { connection._LogDebug(message); }
				       );
    this._receiver = receiver;
    
    receiver.Send("");
}

KanaloaConnection.prototype._EnsureSendBatcher = function() {
    if (this._sendBatcher == null) {
	var connection = this;
	this._sendBatcher = new KanaloaHttpSendBatcher(this._server,
						       this._contentType,
						       function(message) { connection._LogDebug(message); }
						       );
    }
}

KanaloaConnection.prototype.Send = function(data) {
    this._EnsureSendBatcher();
    this._sendBatcher.Send(data);
}

/// Once a ConnectionId is established by the initial request, this class batches outgoing data.
function KanaloaHttpSendBatcher(server, httpContentType, onDebugEvent) {
    this._server = server;
    this._contentType = httpContentType;
    this._connectionId = null;
    this._outgoing = [];
    this._post = null;

    this._onDebugEvent = onDebugEvent;
}

KanaloaHttpSendBatcher.prototype._LogDebug = function(message) {
    if (this._onDebugEvent) {
	this._onDebugEvent("HttpSendBatcher: " + message);
    }
}

KanaloaHttpSendBatcher.prototype.SetConnectionId = function(connectionId) {
    this._LogDebug("Setting ConnectionId.");
    this._connectionId = connectionId;
    this._SendPost();
}

KanaloaHttpSendBatcher.prototype.Send = function(data) {
    this._LogDebug("Adding data \"" + data + "\" to outbox.");
    this._outgoing.push(data);
    this._SendPost();
}

KanaloaHttpSendBatcher.prototype._SendPost = function() {
    if (this._outgoing.length == 0) {
	this._LogDebug("No messages to send.");
	return;
    }
    
    if (this._connectionId == null) {
	this._LogDebug("ConnectionId not set yet.");
	return;
    }
    
    if (this._post && this._post.IsActive()) {
	// If the post is still working, wait for it to complete.
	this._LogDebug("A post is still active; waiting for it to complete.");
	return;
    }

    var batcher = this;

    function PostCompleted(post, statusCode) {
	// If successful, remove sent messages from outbox.
	if (statusCode == 200) {
	    for (var i = 0; i < post.sentCount; i++) {
		batcher._outgoing.shift();
	    }
	}

	// Loop to pick up accumulated messages.
	batcher._SendPost();
    }
    
    this._LogDebug("There is no active post; creating a new one.");
    // TODO: Reuse existing post or remove post reconnect logic.
    var post = new KanaloaHttpPost(this._server,
				   this._connectionId,
				   this._contentType,
				   null,
				   null,
				   function(httpStatusCode) { PostCompleted(this, httpStatusCode); },
				   function(message) { batcher._LogDebug(message); }
				   );
    batcher._post = post;
    
    // TODO: Limit size of sent string to 1 kB
    var textOutgoing = JSON.stringify(this._outgoing);
    this._LogDebug("Sending batch \"" + textOutgoing + "\"");
    this._post.Send(textOutgoing);
    this._post.sentCount = this._outgoing.length;
}

/// Wraps XmlHttpRequest to provide lowest-level send and receive functionality.
/// server -- The full URL to post to.
/// onReceiveChunk -- On stream-capable browsers, this is fired once per chunk. Otherwise, once per request.
/// onClose -- Fired when the underlying request dies.
/// onDebugEvent -- Reports interesting diagnostic information.
function KanaloaHttpPost(server, connectionId, httpContentType, onOpen, onReceiveChunk, onClose, onDebugEvent) {
    this._server = server;
    this.ConnectionId = connectionId;
    this._contentType = httpContentType;
    this._request = null;

    this._onOpen = onOpen;
    this._onReceiveChunk = onReceiveChunk;
    this._onClose = onClose;
    this._onDebugEvent = onDebugEvent;
}

KanaloaHttpPost.prototype._ReportOpen = function() {
    if (this._onOpen) {
	this._onOpen();
    }
}

KanaloaHttpPost.prototype._ReportChunk = function(data) {
    if (this._onReceiveChunk) {
	this._onReceiveChunk(data);
    }
}

KanaloaHttpPost.prototype._ReportClose = function(httpStatusCode) {
    if (this._onClose) {
	this._onClose(httpStatusCode);
    }
}

KanaloaHttpPost.prototype._LogDebug = function(message) {
    if (this._onDebugEvent) {
	this._onDebugEvent("HttpPost: " + message);
    }
}

KanaloaHttpPost.prototype.IsActive = function() {
    if (this._request && (this._request.readyState != READYSTATE_UNSENT || this._request.readyState != READYSTATE_DONE)) {
	return false;
    }

    return true;
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
	
	//connection._LogDebug("responseText is \"" + request.responseText + "\"");
	
	if (readyState == READYSTATE_HEADERSRECEIVED) {
	    var headers = request.getAllResponseHeaders();
	    connection._LogDebug("AllResponseHeaders is \"" + headers + "\"");
	    
	    var newConnectionId = request.getResponseHeader("ConnectionId");
	    if (newConnectionId) {
		connection._LogDebug("Set new ConnectionId \"" + newConnectionId + "\"");
		connection.ConnectionId = newConnectionId;
	    }
	    
	    connection._LogDebug("status is \"" + request.status + "\"");

	    connection._ReportOpen();
	}
	
	if (readyState == READYSTATE_LOADING || readyState == READYSTATE_DONE) {
	    var allData = request.responseText;
	    var data = allData.substring(request.lenReceived);
	    data = data.trim();
	    request.lenReceived = allData.length;
	    if (data.length > 0) {
		connection._LogDebug("Received additional responseText \"" + data + "\"");

		var responses = [];
		try {
		    // The response should always be a JSON array.
		    responses = JSON.parse(data);
		}
		catch (ex) {
		    connection._LogDebug("Error parsing responseText \"" + data + "\"");
		}
		
		for (var i = 0; i < responses.length; i++) {
		    var response = responses[i];
		    connection._ReportChunk(response);
		}
	    }
	}
	
	if (readyState == READYSTATE_DONE) {
	    connection._ReportClose(request.status);
	}
    }
    
    if (this.ConnectionId) {
	connection._LogDebug("Setting ConnectionId header to \"" + this.ConnectionId + "\"");
	request.setRequestHeader("ConnectionId", this.ConnectionId);
    }
    if (this._contentType) {
	connection._LogDebug("Setting Content-Type header to \"" + this._contentType + "\"");
	request.setRequestHeader("Content-Type", this._contentType);
    }
    
    return true;
}

KanaloaHttpPost.prototype.Send = function(data) {
    this.Connect();
    this._request.send(data);
}
