$(document).ready(function(){
	
	var server = "/kanaloa/testsvc/";
	
	module("Chunk splitter unit tests");
	
	test("empty chunk", function() {
		var input = "[]";
		var output = ["[]", ""];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
	test("double quoted chunk", function() {
		var input = '["]"]';
		var output = ['["]"]', ""];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
	test("single quoted chunk", function() {
		var input = "[']']";
		var output = ["[']']", ""];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
	test("nested quoted chunk", function() {
		var input = "['\"]\"']";
		var output = ["['\"]\"']", ""];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
	test("incomplete chunk", function() {
		var input = '["Lorem ipsum dolor sit amet, consectetur adipiscing elit.';
		var output = ['["Lorem ipsum dolor sit amet, consectetur adipiscing elit.'];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
	test("two chunks", function() {
		var input = '["Lorem ipsum dolor sit amet, consectetur adipiscing elit."]["foo bar"]';
		var output = ['["Lorem ipsum dolor sit amet, consectetur adipiscing elit."]', '["foo bar"]', ""];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
	test("one plus incomplete chunk", function() {
		var input = '["Lorem ipsum dolor sit amet, consectetur adipiscing elit.';
		var output = ['["Lorem ipsum dolor sit amet, consectetur adipiscing elit.'];
		
		var chunks = KanaloaConnection.prototype._splitChunk(input);
		same(chunks, output);
	    });
	
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

	asyncTest("chained message send and receive (onMessageReceived send)", 3, function() {
		var connection = new KanaloaConnection(server);
		var counter = 0;
		
		connection.onDataReceived = function(data) {
		    counter++;
		    
		    if (counter == 1) {
			equals("first", data.message, "received first message");
			connection.send("second");
		    }
		    else {
			connection.disconnect();
			equals("second", data.message, "received second message");
			equals(2, counter, "received both messages");
			start();
		    }
		};
		
		connection.send("first");
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

	asyncTest("message series with large total size (greater than typical chunk size)", 5, function() {
		var messages = [
				"14eb8a40-0f17-11e0-ac64-0800200c9a66 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet",
				"20334410-0f17-11e0-ac64-0800200c9a66 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet",
				"2c1b3580-0f17-11e0-ac64-0800200c9a66 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet",
				"3724d4e0-0f17-11e0-ac64-0800200c9a66 Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet"
				];
		var receivedCount = 0;
		
		var connection = new KanaloaConnection(server);
		
		connection.onDataReceived = function(data) {
		    var expectedMessage = messages[receivedCount];
		    receivedCount++;
		    equals(expectedMessage, data.message, "received expected message in its entirety.");
		    
		    if (receivedCount == messages.length) {
			connection.disconnect();
			ok(true, "received all messages");
			start();
		    }
		}
		
		for (var i = 0; i < messages.length; i++) {
		    connection.send(messages[i]);
		}
	    });
	
	asyncTest("message flood 32", 33, function() {
		var connection = new KanaloaConnection(server);
		
		var counter = 0;
		
		connection.onDataReceived = function(data) {
		    equals(counter, data.count, "received message " + counter + " in order");
		    counter++;
		    
		    if (counter == 32) {
			connection.disconnect();
			ok(true, "received all messages");
			start();
		    }
		}
		
		connection.send("flood32");
	    });
    });
