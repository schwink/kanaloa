$(document).ready(function(){
	
	var server = "/kanaloa/testsvc/";
	
	asyncTest("basic connect and disconnect", 4, function() {
		var connection = new KanaloaConnection(server);
		
		connection.onConnectionOpened = function() {
		    ok( true, "onConnectionOpened callback received" );
		    ok( (connection.connectionId), "connectionId has been set" );
		    
		    connection.disconnect();
		};
		
		connection.onConnectionClosed = function() {
		    ok( true, "onConnectionClosed callback received" );
		    equals( connection.connectionId, null, "connectionId has been unset" );
		    
		    start();
		};
		
		connection.connect();
	    });
	
	asyncTest("basic message send and receive", 1, function() {
		var message = "test message " + Date.UTC();

		var connection = new KanaloaConnection(server);

		connection.onDataReceived = function(data) {
		    equals(message, data.message, "received valid message");
		    
		    start();
		};
		
		connection.send(message);
	    });
    });
