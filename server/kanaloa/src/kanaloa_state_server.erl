%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc This gen_server module maintains a unique mapping between binary GUIDs and PIDs,
%% along with some transient per-owner metadata.
%% The GUIDs are assigned as connection IDs that reference an "Owner" process on the server.

-module(kanaloa_state_server).
-author('Stephen Schwink <kanaloa@schwink.net>').

-behavior(gen_server).

%% @headerfile "../include/kanaloa.hrl"
-include("../include/kanaloa.hrl").

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([new_guid/0]).
-export([set_state/1, get_state/1]).

-define(GUID_SERVER, whereis(?MODULE)).

-record(state, {id_to_process, process_to_id}).

%% @spec new_guid() -> binary()
%% @doc Returns a globally unique identifier.
new_guid() ->
    Out = os:cmd("uuid"),
    % Strip the trailing "\n"
    Uuid = string:strip(Out, right, $\n),
    list_to_binary(Uuid).

%% @spec set_state(kanaloa_connection_state()) -> ok
%% @doc Sets a connection's state in the index.
set_state(Entry) when is_record(Entry, kanaloa_connection_state)
                      andalso is_pid(Entry#kanaloa_connection_state.owner)
                      andalso is_binary(Entry#kanaloa_connection_state.id) ->
    gen_server:call(?GUID_SERVER, {set, Entry}).

%% @spec get_state(Id::binary()) -> {ok, kanaloa_connection_state()} | no_id
%% @doc Retreives a connection's state from the index.
get_state(Id) when is_binary(Id) ->
    gen_server:call(?GUID_SERVER, {get, Id}).

%% @spec start_link() -> ok
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
init([]) ->
    process_flag(trap_exit, true), % So we can catch death of registered processes.

    State = #state{
      id_to_process = gb_trees:empty(),
      process_to_id = gb_trees:empty()
     },
    
    {ok, State}.

%% @spec stop() -> ok
stop() ->
    gen_server:cast(?GUID_SERVER, stop).

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
handle_call({set, Entry}, _From, State) ->
    Id = Entry#kanaloa_connection_state.id,
    Process = Entry#kanaloa_connection_state.owner,
    log("registering Id ~s Pid ~w", [Id, Process]),
    
    link(Process),
    NewId = gb_trees:enter(Id, Entry, State#state.id_to_process),
    NewProcess = gb_trees:enter(Process, Entry, State#state.process_to_id),
    NewState = #state{
      id_to_process = NewId,
      process_to_id = NewProcess
     },
    {reply, ok, NewState};

handle_call({get, Id}, _From, State) ->
    case gb_trees:lookup(Id, State#state.id_to_process) of
	none ->
	    log("couldn't find Id ~s", [Id]),
	    {reply, no_id, State};
	{value, Entry} ->
	    log("found Id ~s", [Id]),
	    {reply, {ok, Entry}, State}
    end.

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
handle_info(Info, State) ->
    NewState = case Info of
		   {'EXIT', Process, Why} ->
		       log("Process ~w exited with reason ~w", [Process, Why]),
		       % Remove the process.
		       case gb_trees:lookup(Process, State#state.process_to_id) of
			   none ->
			       log("Dead process ~w was not found in the index", [Process]),
			       State;
			   {value, Entry} ->
			       Id = Entry#kanaloa_connection_state.id,
			       log("Dead process ~w was found with Id ~s in the index", [Process, Id]),
			       NewId = gb_trees:delete_any(Id, State#state.id_to_process),
			       NewProcess = gb_trees:delete_any(Process, State#state.process_to_id),
			       #state{ id_to_process = NewId,
				       process_to_id = NewProcess
				      }
		       end;
		   Wtf ->
		       log("Caught unhandled message: ~w", [Wtf]),
		       State
	       end,
    {noreply, NewState}.

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
terminate(_Reason, _State) ->
    ok.

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @spec log(MessageTemplate::string(), MessageParameters::[term()]) -> ok
%% @doc Submits a log message. Use like io:format/2.
log(Template, Parameters) when is_list(Template) andalso is_list(Parameters) ->
    Message = io_lib:format(Template, Parameters),
    io:format("kanaloa_state_server : ~s\n", [Message]),
    ok.
