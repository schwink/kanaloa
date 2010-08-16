$(document).ready(function(){
	
	var server = "/kanaloa/testsvc/";
	
	module("Basics");
	
	asyncTest("basic connect and onConnectionOpened disconnect", 4, function() {
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
	
	asyncTest("basic message send and receive with onDataReceived disconnect", 1, function() {
		var message = "test message " + (new Date()).getTime();
		
		var connection = new KanaloaConnection(server);

		connection.onDataReceived = function(data) {
		    equals(message, data.message, "received valid message");

		    connection.disconnect();
		    start();
		};
		
		connection.send(message);
	    });

	asyncTest("server disconnect handling", 1, function() {
		var connection = new KanaloaConnection(server);
		
		connection.onConnectionOpened = function() {
		    connection.send("kill_owner");
		};
		
		connection.onConnectionClosed = function() {
		    ok( true, "onConnectionClosed callback received" );
		    start();
		};
	    });
	
	module("Stress");
	
	var repetitions = 20;
	asyncTest("message order preservation", repetitions + 1, function() {
		var receivedCount = 0;
		
		var connection = new KanaloaConnection(server);
		
		connection.onDataReceived = function(data) {
		    equals(receivedCount, data.message, "received message " + receivedCount + " in order");
		    receivedCount++;
		    
		    if (receivedCount == repetitions) {
			connection.disconnect();
			ok(true, "received all messages");
			start();
		    }
		}
		
		for (var s = 0; s < repetitions; s++) {
		    connection.send(s);
		}
	    });
    });
