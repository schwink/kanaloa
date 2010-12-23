// Copyright 2010 Stephen Schwink. All Rights Reserved.

/**
   @fileoverview Include this file to define the KanaloaConnection class.
   Create instances of this class to communicate with the kanaloa server.
   @author kanaloa@schwink.net (Stephen Schwink)
*/

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

/**
 * Top-level user-facing abstraction of all Kanaloa client functionality.
 * @param {string} server The url of the Kanaloa service to connect to.
 * @constructor
 */
this.KanaloaConnection = function(/** String */ server) {
    this.settings = new _KanaloaHttpSettings(); 
    this.server = server;
    this.connectionId = null;
    
    this._receiver = null;
    this._sendBatcher = null;
    
    this.connect();
};

/**
 * Set this property to be notified when the connection opens.
 */
KanaloaConnection.prototype.onConnectionOpened = function() { };

/**
 * Set this property to handle messages when they are received from the server.
 * @param {Object} data The message received from the server.
 */
KanaloaConnection.prototype.onDataReceived = function(data) { };

/**
 * Set this property to be notified when the connection closes.
 * Invoked when the connection between the client and the server will never be reconnected.
 */
KanaloaConnection.prototype.onConnectionClosed = function() { };

/**
 * Set this property to handle local debug events.
 * @param {string} message A description of something going on in the client.
 */
KanaloaConnection.prototype.onDebugEvent = function(message) { };

/** @private */
KanaloaConnection.prototype._reportConnectionOpened = function() {
    if (this.onConnectionOpened) {
	this.onConnectionOpened();
    }
};

/** @private */
KanaloaConnection.prototype._reportDataReceived = function(data) {
    if (this.onDataReceived) {
	this.onDataReceived(data);
    }
};

/** @private */
KanaloaConnection.prototype._reportConnectionClosed = function() {
    if (this.onConnectionClosed) {
	this.onConnectionClosed();
    }
};

/** @private */
KanaloaConnection.prototype._logDebug = function(message) {
    if (this.onDebugEvent) {
	this.onDebugEvent("Connection: " + message);
    }
};

/** @private */
KanaloaConnection.prototype._bumpIncoming = function(statusCode) {
    if (statusCode == 410) {
	// GONE
	this.disconnect();
	return false;
    }
    
    this.settings.bumpIncoming(statusCode);
    return true;
};

/** @private */
KanaloaConnection.prototype._bumpOutgoing = function(statusCode) {
    if (statusCode == 410) {
	// GONE
	this.disconnect();
	return false;
    }
    
    this.settings.bumpOutgoing(statusCode);
    return true;
};

/**
 * Connect to the server. This is called automatically for you.
 */
KanaloaConnection.prototype.connect = function() {
    var connection = this;
    
    if (connection._receiver) {
	connection._logDebug("A receiver already exists, not trying to reconnect.");
	return;
    }
    
    connection._logDebug("Using stream mode? " + connection.settings.isStreamMode);
    
    function receiverOpened() {
	connection._logDebug("Opened");
	
	var receiverPost = connection._receiver;
	
	// Now that we have the ConnectionId, we can begin transmitting outgoing posts.
	var firstOpened = (connection.connectionId == null);
	if (receiverPost) {
	    connection.connectionId = receiverPost.connectionId;
	    if (firstOpened) {
		connection._reportConnectionOpened();
		connection.send();
	    }
	}
    }
    
    function receiverClosed(statusCode) {
	connection._logDebug("Closed with status: " + statusCode);
	
	var receiverPost = connection._receiver;
	
	if (connection._bumpIncoming(statusCode)) {
	    connection._logDebug("Waiting " + connection.settings.incomingWait + " ms before reconnect.");
	    
	    if (receiverPost) {
		receiverPost.reconnectTimeout = setTimeout(function() {
			connection._receiver = null;
			connection.connect();
		    }, connection.settings.incomingWait);
	    }
	}
    }
    
    var receiver = new _KanaloaHttpPost(this.server + "/" + this.settings.connectionSuffix,
					this.connectionId,
					this.settings.contentType,
					this.settings.isStreamMode,
					function() { receiverOpened(); },
					function(data) { connection._reportDataReceived(data); },
					function(httpStatusCode) { receiverClosed(httpStatusCode); },
					function(message) { connection._logDebug(message); }
					);
    this._receiver = receiver;
    
    receiver.send();
};

/**
 * Connect to the server. This is called automatically for you.
 */
KanaloaConnection.prototype.disconnect = function() {
    var connection = this;
    connection._logDebug("Closing the connection");
    
    if (connection._receiver) {
	connection._receiver.disconnect();
	clearTimeout(connection._receiver.reconnectTimeout);
	connection._receiver = null;
    }
    
    if (connection._sendBatcher) {
	connection._sendBatcher.close();
	connection._sendBatcher = null;
    }
    
    connection.connectionId = null;
    connection._reportConnectionClosed();
}

/**
 * Sends a JavaScript term to the server.
 * @param {object} data The item to send.
 */
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
    if (statusCode == 200) {
	this.incomingWait = KANALOA_WAIT_INCOMING_BASE;
    }
    else {
	if (this.incomingWait == KANALOA_WAIT_INCOMING_BASE) {
	    this.incomingWait = 1000;
	}
	else if (this.incomingWait < 100000) {
	    this.incomingWait *= 2;
	}
    }
};

_KanaloaHttpSettings.prototype.bumpOutgoing = function(statusCode) {
    if (statusCode == 200) {
	this.outgoingWait = KANALOA_WAIT_OUTGOING_BASE;
    }
    else {
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

_KanaloaHttpSendBatcher.prototype.close = function() {
    if (this._post) {
	this._post.disconnect();
	clearTimeout(this._post.reconnectTimeout);
	this._post = null;
    }
}

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
    
    if (this._post) {
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
	    post.sentCount = 0;
	}
	
	batcher._post = null;

	// Loop to pick up accumulated messages.
	if (connection._bumpOutgoing(statusCode)) {
	    connection._logDebug("Waiting " + connection.settings.outgoingWait + " ms before reconnect.");
	    post.reconnectTimeout = setTimeout(function() {
		    batcher._sendPost();
		}, connection.settings.outgoingWait);
	}
    }
    
    this._logDebug("There is no active post; creating a new one.");
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
	this._logDebug("Connect: Aborting existing request.");
	this.disconnect();
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
	
	// How much of the request has completed?
	var statusStatusKnown = (connection._isStreamMode && readyState >= READYSTATE_HEADERSRECEIVED) || readyState == READYSTATE_DONE;
	var statusReceivedHeaders = (connection._isStreamMode && readyState == READYSTATE_HEADERSRECEIVED) || (!connection._isStreamMode && readyState == READYSTATE_DONE);
	var statusReceivedBody = (connection._isStreamMode && readyState == READYSTATE_LOADING) || readyState == READYSTATE_DONE;
	var statusClosed = readyState == READYSTATE_DONE;
	
	// Read the request before any callbacks, in case the request is aborted and its properties are unset.
	var requestStatus;
	if (statusStatusKnown) {
	    requestStatus = request.status;
	}
	var headers = {};
	if (statusReceivedHeaders) {
	    headers["ConnectionId"] = request.getResponseHeader("ConnectionId");
	}
	var responseText = "";
	if (statusReceivedBody) {
	    responseText = request.responseText;
	}
	
	// Go ahead and act on the data that was gathered.
	
	if (statusReceivedHeaders) {
	    var newConnectionId = headers["ConnectionId"];
	    if (newConnectionId) {
		connection._logDebug("Set new ConnectionId \"" + newConnectionId + "\"");
		connection.connectionId = newConnectionId;
	    }
	    
	    connection._logDebug("status is \"" + request.status + "\"");
	    
	    if (requestStatus == 200) {
		connection._reportOpen();
	    }
	}
	
	if (statusReceivedBody) {
	    var data = responseText.substring(request.lenReceived);
	    request.lenReceived = responseText.length;
	    
	    data = stringTrim(data);
	    if (data.length > 0) {
		connection._logDebug("Received additional responseText \"" + data + "\"");

		var responses = JSON.parse(data);
		for (var i = 0; i < responses.length; i++) {
		    var response = responses[i];
		    connection._reportChunk(response);
		}
	    }
	}
	
	if (statusClosed) {
	    connection._reportClose(requestStatus);
	}
    };
    
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

_KanaloaHttpPost.prototype.disconnect = function() {
    if (this._request) {
	this._logDebug("disconnect aborting existing request");
	this._request.abort();
	this._request = null;
    }
};

_KanaloaHttpPost.prototype.send = function(data) {
    this.connect();
    this._request.send(data);
};

})();
