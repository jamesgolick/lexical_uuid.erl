-module(lexical_uuid).

-compile({no_auto_import,[get/0]}).

-behaviour(gen_server).

-export([start_link/0,
	 state/0,
	 stop/0,
         get/0,
         get_string/0,
         get_binary_string/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {
    last = 0  :: integer(),
    worker_id :: binary()
  }).

%% Public API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

state() ->
  gen_server:call(?MODULE, state).

get() ->
  gen_server:call(?MODULE, get).

get_string() ->
  lexical_uuid_util:to_string(?MODULE:get()).

get_binary_string() ->
  list_to_binary(get_string()).

%% Server implementation, a.k.a.: callbacks

init([]) ->
  {ok, #state{worker_id=fqdn:get()}}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call(get, _From, State=#state{last=LastTime,worker_id=WorkerID}) ->
  NextTime = lexical_uuid_clock:get_next_timestamp(LastTime),
  UUID = lexical_uuid_util:make(NextTime, WorkerID),
  {reply, UUID, State#state{last=NextTime}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
