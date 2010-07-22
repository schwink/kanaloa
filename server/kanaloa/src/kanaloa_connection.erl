%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Wraps a mochiweb_response to represent a connection to a specific client.

-module(kanaloa_connection, [MochiResp, Self, ConnectionId, CometMethod]).
-author('Stephen Schwink <kanaloa@schwink.net>').

-export([open/1, send/1, close/0]).
-export([log/2]).

-define(BATCH_INTERVAL, 1000). % Milliseconds between batches are transmitted. Controls how quickly we detect a disconnected client.
-define(BATCH_CHECK_INTERVAL, 600). % Milliseconds between checking the interval. This should scale with BATCH_INTERVAL.
-define(BATCH_COUNT, 32). % Number of batch intervals to keep the batch open. Maximum connection time = BATCH_INTERVAL * BATCH_COUNT

-record(batch_settings, {owner=none, interval=?BATCH_INTERVAL, check_interval=?BATCH_CHECK_INTERVAL, count=?BATCH_COUNT}).

%% @spec open(Owner::pid()) -> void()
%% @doc Opens the connection. Control does not return from this call.
open(Owner) when is_pid(Owner) ->
    process_flag(trap_exit, true),
    link(Owner),
    BatchSettings = #batch_settings { owner = Owner },
    loop(BatchSettings, BatchSettings#batch_settings.count).

%% @spec send(Data::iolist()) -> ok
%% @doc Sends a message to the client.
send(Message) ->
    Self ! {send, Message},
    ok.

%% @spec close() -> ok
%% @doc Closes the connection. The process will exit; no further data can be sent.
close() ->
    Self ! close,
    ok.

%% Internal API

%% @spec log(MessageTemplate::string(), MessageParameters::[term()]) -> ok
%% @doc Submits a connection-specific log message. Use like io:format/2.
log(Template, Parameters) when is_list(Template) andalso is_list(Parameters) ->
    Message = io_lib:format(Template, Parameters),
    io:format("~s : ~s\n", [ConnectionId, Message]),
    ok.

%% @doc Loops to send batches.
%% If no outgoing messages are accumulated, we still send empty data to detect if the client is connected.
loop(BatchSettings, Count) ->
    % To prevent memory leaks on the client, we do not stream forever, but rather limit the count of batches that we send.
    % The client will reconnect when this connection is terminated.
    if
	Count < 1 ->
	    log("Connection reached batch count.", []),
	    exit(count);
	true ->
	    ok
    end,
    
    % Accumulate messages for the batch interval.
    Timeout = now_ms() + BatchSettings#batch_settings.interval,
    {Status, Messages} = loop_accumulate(BatchSettings, [], Timeout),
    
    % Send the batch.
    case catch send_batch(Messages) of
	ok ->
	    ok;
	SendError ->
	    log("Error sending a batch: ~w", [SendError]),
	    exit(closed_remote)
    end,
    
    NewCount = case CometMethod of
		   longpoll ->
		       case Messages of
			   [] -> Count - 1; % Carry on
			   _ when Count > 1 -> 1 % Listen for one more batch, then close the connection.
		       end;
		   _ ->
		       Count - 1
	       end,
    
    % Loop if everything is good, otherwise exit.
    case Status of
	ok ->
	    loop(BatchSettings, NewCount);
	_ ->
	    exit(Status)
    end.

%% @spec loop_accumulate(BatchSettings::batch_settings(), Messages::list(), Timeout::integer()) -> {Status::ok | owner_exit, Messages::list()}
%% @doc Loops to accumulate messages, for one timeout period.
loop_accumulate(BatchSettings, Messages, Timeout) when is_list(Messages) andalso is_integer(Timeout)->
    Now = now_ms(),
    if
	Timeout < Now ->  % Check batch timeout
	    {ok, Messages};
	
	true ->
	    Owner = BatchSettings#batch_settings.owner,
	    receive
		{send, Message} ->
		    loop_accumulate(BatchSettings, [Message | Messages], Timeout);
		
		{'EXIT', Owner, _Reason} ->
		    {owner_exit, Messages};
		
		close ->
		    {close, Messages}
	    
	    after BatchSettings#batch_settings.check_interval ->
		    loop_accumulate(BatchSettings, Messages, Timeout)
	    end
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
    log("Sending empty batch to test that the client is still receiving.", []),
    MochiResp:write_chunk(<<"\n">>), % Send a batch to detect if the connection has closed.
    ok;
send_batch(Messages) when is_list(Messages) ->
    log("Sending batch of ~w messages", [length(Messages)]),
    Data = mochijson2:encode(Messages),
    MochiResp:write_chunk(Data).
