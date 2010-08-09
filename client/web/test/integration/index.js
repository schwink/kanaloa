$(document).ready(function(){
	
	asyncTest("basic connect and disconnect", 4, function() {
		var connection = new KanaloaConnection("/kanaloa/testsvc/");
		
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
    });
