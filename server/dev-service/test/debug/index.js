
function removeAllChildren(element) {
    if (element.hasChildNodes()) {
	while (element.childNodes.length >= 1) {
	    element.removeChild( element.firstChild );       
	}
    }
}

function addChildElement(element, elementType) {
    var child = document.createElement(elementType);
    return element.appendChild(child);
}

var requests = [];
function updateRequestMessages() {
    var requestMessagesElement = document.getElementById("requestMessages");
    if (requestMessagesElement) {
	removeAllChildren(requestMessagesElement);
	
	for (var requestId = 0; requestId < requests.length; requestId++) {
	    var requestTable = addChildElement(requestMessagesElement, "table");
	    requestTable.border = 1;
	    var request = requests[requestId];
	    
	    for (var messageId in request.statusMessages) {
		var message = request.statusMessages[messageId];
		var messageRow = addChildElement(requestTable, "tr");
		addChildElement(messageRow, "td").innerHTML = requestId.toString();
		var messageCol = addChildElement(messageRow, "td");
		addChildElement(messageCol, "pre").innerHTML = message;
	    }
	}
    }
}

var id = 0;
function submit() {
    if (requests.length == 0) {
	var server = document.getElementById("server").value;
	var connection = new KanaloaConnection(server);
	requests.push(connection);
	
	connection.statusMessages = [];
	connection.onReceive = function(data) {
	    this.statusMessages.push("Receive: \"" + data + "\"");
	    updateRequestMessages();	
	}
	connection.onDebugEvent = function(message) {
	    this.statusMessages.push("Debug: " + message);
	    updateRequestMessages();	
	}
    }

    var bodyElement = document.getElementById("body");
    var bodyText = bodyElement.value;
    
    var connection = requests[0];
    connection.send(bodyText + (id++));

    return false;
}
