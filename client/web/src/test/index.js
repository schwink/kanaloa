Element.prototype.removeAllChildren = function() {
    if (this.hasChildNodes()) {
	while (this.childNodes.length >= 1) {
	    this.removeChild( this.firstChild );       
	}
    }
}

Element.prototype.addChildElement = function(elementType) {
    var element = document.createElement(elementType);
    return this.appendChild(element);
}

var requests = [];
function updateRequestMessages() {
    var requestMessagesElement = document.getElementById("requestMessages");
    if (requestMessagesElement) {
	requestMessagesElement.removeAllChildren();
	
	for (var requestId = 0; requestId < requests.length; requestId++) {
	    var requestTable = requestMessagesElement.addChildElement("table");
	    requestTable.border = 1;
	    var request = requests[requestId];
	    
	    for (var messageId in request.statusMessages) {
		var message = request.statusMessages[messageId];
		var messageRow = requestTable.addChildElement("tr");
		messageRow.addChildElement("td").innerHTML = requestId.toString();
		messageRow.addChildElement("td").addChildElement("pre").innerHTML = message;
	    }
	}
    }
}

function submit() {
    if (requests.length == 0) {
	var server = document.getElementById("server").value;
	var connection = new KanaloaConnection(server);
	requests.push(connection);
	
	connection.statusMessages = [];
	connection.OnReceive = function(data) {
	    this.statusMessages.push("Receive: \"" + data + "\"");
	    updateRequestMessages();	
	}
	connection.OnDebugEvent = function(message) {
	    this.statusMessages.push("Debug: " + message);
	    updateRequestMessages();	
	}
    }

    var bodyElement = document.getElementById("body");
    var bodyText = bodyElement.value;
    
    var connection = requests[0];
    connection.Send(bodyText);

    return false;
}
