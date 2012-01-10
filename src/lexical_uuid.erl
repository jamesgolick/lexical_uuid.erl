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
  lager:debug("init", []),
  {ok, #state{worker_id=lexical_uuid_util:get_fqdn()}}.

handle_call(stop, _From, State) ->
  lager:debug("stopping by ~p, state was ~p.", [_From, State]),
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  lager:debug("~p is asking for the state.", [_From]),
  {reply, State, State};

handle_call(get, _From, State=#state{last=LastTime,worker_id=WorkerID}) ->
  lager:debug("~p is asking for a uuid.", [_From]),
  NextTime = lexical_uuid_clock:get_next_timestamp(LastTime),
  UUID = lexical_uuid_util:make(NextTime, WorkerID),
  {reply, UUID, State#state{last=NextTime}};

handle_call(_Request, _From, State) ->
  lager:debug("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  lager:debug("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.

handle_info(_Info, State) ->
  lager:debug("info ~p, ~p.", [_Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  lager:debug("terminate ~p, ~p", [_Reason, _State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  lager:debug("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.
