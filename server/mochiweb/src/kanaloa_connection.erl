%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Wraps a mochiweb_response to represent a connection to a specific client.

-module(kanaloa_connection, [MochiResp, Self, CometMethod]).
-author('Stephen Schwink <kanaloa@schwink.net>').

-export([open/1, send/1, close/0]).

-define(BATCH_INTERVAL, 200). % Milliseconds between batches are transmitted.
-define(BATCH_CHECK_INTERVAL, 100). % Milliseconds between checking the interval.
-define(BATCH_COUNT, 1024). % Number of batch intervals to keep the batch open.

-record(batch, {messages=[], owner=none, interval=?BATCH_INTERVAL, check_interval=?BATCH_CHECK_INTERVAL, count=?BATCH_COUNT, timeout=none}).

%% @spec open(Owner::pid()) -> void()
%% @doc Opens the connection. Control does not return from this call.
open(Owner) when is_pid(Owner) ->
    process_flag(trap_exit, true),
    link(Owner),
    
    % Initialize the batch (see loop/1 below).
    {CheckInterval, Count} = case CometMethod of
				 longpoll ->
				     % Wait indefinitely for the first message, then only send one batch (after this batch is created).
				     {infinity, 2};
				 _ ->
				     {?BATCH_CHECK_INTERVAL, ?BATCH_COUNT}
			     end,
    Batch = #batch {
      owner=Owner,
      check_interval = CheckInterval,
      count = Count
     },
    
    {ok, NewBatch} = new_batch_state(Batch),
    loop(NewBatch).

%% @spec send(Data::iolist()) -> ok
%% @doc Sends a message to the client.
send(Message) ->
    io:format("Someone called send\n", []),
    Self ! {send, Message},
    ok.

%% @spec close() -> ok
%% @doc Closes the connection. The process will exit; no further data can be sent.
close() ->
    io:format("Someone called close\n", []),
    Self ! close,
    ok.

%% Internal API

%% @doc Call this recursively to receive the next message in the connection inbox.
loop(Batch) ->
    % Check batch timeout
    Now = now_ms(),
    if
	Now > Batch#batch.timeout ->
	    % Send the batch
	    case catch send_batch(Batch#batch.messages) of
		ok ->
		    ok;
		SendError ->
		    io:format("Error sending a batch: ~w\n", [SendError]),
		    exit(closed_remote)
	    end,
	    case new_batch_state(Batch) of
		done ->
		    exit(count);
		{ok, NewBatch} ->
		    loop(NewBatch)
	    end;
	true ->
	    io:format("Listening for the next message, with timeout ~w\n", [Batch#batch.check_interval]),
	    Owner = Batch#batch.owner,
	    NewBatch = receive
			   {send, Message} ->
			       io:format("Connection received a send\n", []),
			       Batch#batch{
				 messages = [Message | Batch#batch.messages]
				};
			   {'EXIT', Owner, _Reason} ->
			       io:format("Connection received an owner exit\n", []),
			       exit(owner_exit);
			   close ->
			       io:format("Connection received a close\n", []),
			       exit(close_local)
		       after Batch#batch.check_interval ->
			       io:format("listen timeout\n", []),
			       Batch
		       end,
	    loop(NewBatch)
    end.

%% @spec new_batch_state(OldBatch::batch()) -> {ok, batch()} | done
%% @doc Updates the batch with a new count and timeout.
new_batch_state(OldBatch) ->
    Count = OldBatch#batch.count - 1,
    if
	Count > 0 ->
	    Timeout = now_ms() + ?BATCH_INTERVAL,
	    NewBatch = OldBatch#batch{
			 messages = [],
			 timeout = Timeout,
			 count = Count,
			 check_interval = ?BATCH_CHECK_INTERVAL
			},
	    {ok, NewBatch};
	true ->
	    done
    end.

%% @spec now_ms() -> integer()
%% @doc Returns local system time in milliseconds.
now_ms() ->
    {MegaS, S, MicroS} = erlang:now(),
    Seconds = MegaS * 1000000 + S,
    MilliS = Seconds * 1000 + trunc(MicroS / 1000),
    MilliS.

%% @spec send_batch([message()]) -> ok
%% @doc Sends the batch of messages over the wire as a HTTP chunk, encoded as a JSON array.
send_batch([]) ->
    io:format("Sending empty batch. Keepalive?\n", []),
    MochiResp:write_chunk(<<"\n">>), % Send a batch to detect if the connection has closed.
    ok;
send_batch(Messages) when is_list(Messages) ->
    io:format("Sending batch of ~w messages\n", [length(Messages)]),
    Data = mochijson2:encode(Messages),
    io:format("Datch is JSON encoded as ~s messages\n", [Data]),
    MochiResp:write_chunk([<<"\n">>, Data, <<"\n">>]).
