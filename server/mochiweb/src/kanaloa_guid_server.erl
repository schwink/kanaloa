%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc This gen_server module maintains a unique mapping between binary GUIDs and PIDs.

-module(kanaloa_guid_server).
-author('Stephen Schwink <kanaloa@schwink.net>').

-behavior(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([new_guid/0]).
-export([register_new/2, find/1]).

-define(GUID_SERVER, whereis(?MODULE)).

-record(state, {id_to_process, process_to_id}).

%% @spec new_guid() -> binary()
%% @doc Returns a globally unique identifier.
new_guid() ->
    Out = os:cmd("uuid"),
    % Strip the trailing "\n"
    Uuid = string:strip(Out, right, $\n),
    list_to_binary(Uuid).

%% @spec register_new(Process::pid(), Id::binary()) -> ok | duplicate_process | duplicate_id
%% @doc Adds a process identifier to the index.
register_new(Process, Id) when is_pid(Process) andalso is_binary(Id) ->
    gen_server:call(?GUID_SERVER, {register, Id, Process}).

%% @spec find(Id::binary()) -> {ok, Process::pid()} | no_id
%% @doc Retreives a process identifier from the index.
find(Id) when is_binary(Id) ->
    gen_server:call(?GUID_SERVER, {find, Id}).

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
handle_call({register, Id, Process}, _From, State) ->
    case gb_trees:lookup(Id, State#state.id_to_process) of
	none ->
	    case gb_trees:lookup(Process, State#state.process_to_id) of
		none ->
		    log("registering Id ~s Pid ~w", [Id, Process]),
		    
		    link(Process),
		    NewId = gb_trees:insert(Id, Process, State#state.id_to_process),
		    NewProcess = gb_trees:insert(Process, Id, State#state.process_to_id),
		    NewState = #state{
		      id_to_process = NewId,
		      process_to_id = NewProcess
		     },
		    %log("New state is ~w", [NewState]),
		    {reply, ok, NewState};
		_ ->
		    log("not registering duplicate Id ~s for process ~w", [Id, Process]),
		    {reply, duplicate_process, State}
	    end;
	_ ->
	    log("not registering Id ~s for duplicate process ~w", [Id, Process]),
	    {reply, duplicate_id, State}
    end;

handle_call({find, Id}, _From, State) ->
    case gb_trees:lookup(Id, State#state.id_to_process) of
	none ->
	    log("couldn't find Id ~s", [Id]),
	    {reply, no_id, State};
	{value, Process} when is_pid(Process) ->
	    log("found Id ~s Process ~w", [Id, Process]),
	    {reply, {ok, Process}, State}
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
		   {'EXIT', Pid, Why} ->
		       log("Process ~w exited with reason ~w", [Pid, Why]),
		       % Remove the process.
		       case gb_trees:lookup(Pid, State#state.process_to_id) of
			   none ->
			       log("Dead process ~w was not found in the index", [Pid]),
			       State;
			   {value, Id} when is_binary(Id) ->
			       log("Dead process ~w was found with Id ~s in the index", [Pid, Id]),
			       log("Old state is ~w", [State]),
			       NewId = gb_trees:delete_any(Id, State#state.id_to_process),
			       NewProcess = gb_trees:delete_any(Pid, State#state.process_to_id),
			       #state{ id_to_process = NewId,
				       process_to_id = NewProcess
				      }
		       end;
		   Wtf ->
		       log("Caught unhandled message: ~w", [Wtf]),
		       State
	       end,
    log("New state is ~w", [NewState]),
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
    io:format("kanaloa_guid_server : ~s\n", [Message]),
    ok.
