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
-export([new_state/2, get_owner/1, pop_pending/1, add_pending/2]).

-define(GUID_SERVER, whereis(?MODULE)).

-record(state, {id_to_process, process_to_id}).
%% @type kanaloa_connection_state() = {Owner::pid(), Guid::binary(), Pending::[iolist()]}
-record(kanaloa_connection_state, {owner, id, pending=[]}).

%% @spec new_guid() -> binary()
%% @doc Returns a globally unique identifier.
new_guid() ->
    Out = os:cmd("uuid"),
    % Strip the trailing "\n"
    Uuid = string:strip(Out, right, $\n),
    list_to_binary(Uuid).

%% @spec new_state(Id::binary(), Owner::pid()) -> ok
%% @doc Inserts a new connection's initial state in the index.
new_state(Id, Owner) when is_pid(Owner)
                     andalso is_binary(Id) ->
    Entry = #kanaloa_connection_state{ id = Id, owner = Owner },
    gen_server:cast(?GUID_SERVER, {new, Entry}).

%% @spec get_owner(Id::binary()) -> (pid() | no_id)
%% @doc Looks up a connection's owner process.
get_owner(Id) when is_binary(Id) ->
    gen_server:call(?GUID_SERVER, {get_owner, Id}).

%% @spec pop_pending(Id::binary()) -> ([iolist()] | no_id)
%% @doc Atomically retrieves and resets a connection's pending message list, returning the old list.
pop_pending(Id) when is_binary(Id) ->
    gen_server:call(?GUID_SERVER, {pop_pending, Id}).

%% @spec add_pending(Id::binary(), Pending::[iolist()]) -> ok
%% @doc Adds the specified messages to the end of the connection's pending message list.
add_pending(Id, Pending) when is_binary(Id) andalso is_list(Pending) ->
    gen_server:cast(?GUID_SERVER, {add_pending, Id, Pending}).

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
handle_call({get_owner, Id}, _From, State) ->
    Result = case gb_trees:lookup(Id, State#state.id_to_process) of
		 none ->
		     log("couldn't find Id ~s", [Id]),
		     no_id;
		 {value, Entry} ->
		     log("found Id ~s", [Id]),
		     Entry#kanaloa_connection_state.owner
	     end,
    {reply, Result, State};

handle_call({pop_pending, Id}, _From, State) ->
    case gb_trees:lookup(Id, State#state.id_to_process) of
	none ->
	    log("couldn't find Id ~s", [Id]),
	    {reply, no_id, State};
	
	{value, Entry} ->
	    log("found Id ~s", [Id]),
	    
	    NewEntry = Entry#kanaloa_connection_state{ pending = [] },
	    NewState = set_entry(State, NewEntry),
	    
	    Pending = Entry#kanaloa_connection_state.pending,
	    {reply, Pending, NewState}
    end.

%% @hidden
%% @doc Internal gen_server callback. Do not call directly.
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({new, Entry}, State) ->
    link(Entry#kanaloa_connection_state.owner),
    NewState = set_entry(State, Entry),
    {noreply, NewState};

handle_cast({add_pending, Id, Pending}, State) ->
    NewState = case gb_trees:lookup(Id, State#state.id_to_process) of
		   none ->
		       log("couldn't find Id ~s", [Id]),
		       State;
		   
		   {value, Entry} ->
		       log("found Id ~s", [Id]),
		       OldPending = Entry#kanaloa_connection_state.pending,
		       NewPending = lists:append(OldPending, Pending),
		       io:format("adding ~w, resulting in ~w\n", [Pending, NewPending]),
		       NewEntry = Entry#kanaloa_connection_state{ pending = NewPending },
		       set_entry(State, NewEntry)
	       end,
    {noreply, NewState}.

set_entry(State, Entry) ->
    Id = Entry#kanaloa_connection_state.id,
    Process = Entry#kanaloa_connection_state.owner,
    log("setting Id ~s Pid ~w", [Id, Process]),

    NewIdIndex = gb_trees:enter(Id, Entry, State#state.id_to_process),
    NewProcessIndex = gb_trees:enter(Process, Entry, State#state.process_to_id),
    NewState = #state{
      id_to_process = NewIdIndex,
      process_to_id = NewProcessIndex
     },
    NewState.

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
