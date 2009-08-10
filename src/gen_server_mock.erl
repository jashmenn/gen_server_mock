%%%-------------------------------------------------------------------
%%% File    : gen_server_mock.erl
%%% Author  : nmurray@attinteractive.com
%%% Description : Mocking for gen_server. Expectations are ordered, every
%%%    message required and no messages more than are expected are allowed.
%%%
%%% Expectations get the same input as the handle_(whatever) gen_server methods. They should return -> ok | {ok, NewState}
%%% Created     : 2009-08-05
%%% Inspired by: http://erlang.org/pipermail/erlang-questions/2008-April/034140.html
%%%-------------------------------------------------------------------

-module(gen_server_mock).
-behaviour(gen_server).

% API
-export([new/0, 
        expect/3, expect_call/2, expect_info/2, expect_cast/2,
        assert_expectations/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-define(DEFAULT_CONFIG, {}).

-record(state, {
        expectations
}).

-record(expectation, {
        type,
        lambda
}).

% steal assert from eunit
-define(assert(BoolExpr),
    ((fun () ->
        case (BoolExpr) of
        true -> ok;
        __V -> .erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expression, (??BoolExpr)},
                       {expected, true},
                       {value, case __V of false -> __V;
                           _ -> {not_a_boolean,__V}
                           end}]})
        end
      end)())).

-define(raise(ErrorName),
    erlang:error({ErrorName,
                  [{module, ?MODULE},
                      {line, ?LINE}]})).

-define(raise_info(ErrorName, Info),
    erlang:error({ErrorName,
                  [{module, ?MODULE},
                      {line, ?LINE},
                      {info, Info}
                  ]})).


-define (DEBUG, true).
-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Alias for start_link
%%--------------------------------------------------------------------
start() ->
    start_link([]). 

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []). % start a nameless server

%%--------------------------------------------------------------------
%% Function: new() -> {ok, Mock} | {error, Error}
%% Description: 
%%--------------------------------------------------------------------
new() ->
    case start() of
        {ok, Pid} ->
            {ok, Pid};
        {error, Error} ->
            {error, Error};
        Other ->
            {error, Other}
    end.

expect(Mock, Type, Callback) ->
    Exp = #expectation{type=Type, lambda=Callback},
    added = gen_server:call(Mock, {expect, Exp}),
    ok.

expect_call(Mock, Callback) ->
    expect(Mock, call, Callback).

expect_info(Mock, Callback) ->
    expect(Mock, info, Callback).

expect_cast(Mock, Callback) ->
    expect(Mock, cast, Callback).

assert_expectations(Mock) ->
    gen_server:call(Mock, assert_expectations).

stop([H|T]) ->
    gen_server:cast(H, {'$gen_server_mock', stop}),
    stop(T);
stop([]) ->
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

init(_Args) -> 
    InitialState = #state{expectations=[]},
    {ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

% return the state
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};

handle_call({expect, Expectation}, _From, State) ->
    {ok, NewState} = store_expectation(Expectation, State),
    {reply, added, NewState};

handle_call(assert_expectations, _From, State) ->
    {ok, NewState} = handle_assert_expectations(State),
    {reply, ok, NewState};

handle_call(Request, From, State) -> 
    {Reply, NewState} = reply_with_next_expectation(call, Request, From, undef, undef, State),
    {reply, Reply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({'$gen_server_mock', stop}, State) -> 
    {stop, normal, State};
handle_cast(Msg, State) -> 
    {_Reply, NewState} = reply_with_next_expectation(cast, undef, undef, Msg, undef, State),
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    {_Reply, NewState} = reply_with_next_expectation(info, undef, undef, undef, Info, State),
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%
%% private functions
%%
store_expectation(Expectation, State) -> % {ok, NewState}
    NewExpectations = [Expectation|State#state.expectations],  
    NewState = State#state{expectations = NewExpectations},
    {ok, NewState}.

pop(L) -> % {Result, NewList} | {undef, []}
    case L of
        [] -> {undef, []};
        List -> {lists:last(List), lists:sublist(List, 1, length(List) - 1)}
    end.

pop_expectation(State) -> % {ok, Expectation, NewState}
    {Expectation, RestExpectations} = case pop(State#state.expectations) of
        {undef, []} -> ?raise(no_gen_server_mock_expectation);
        {Head, Rest} -> {Head, Rest}
    end,
    NewState = State#state{expectations = RestExpectations},
    {ok, Expectation, NewState}.

handle_assert_expectations(State) -> % {ok, State}
    ExpLeft = State#state.expectations,
    case length(ExpLeft) > 0 of
        true -> ?raise_info(unmet_gen_server_expectation, ExpLeft);
        false -> ok
    end,
    {ok, State}.

reply_with_next_expectation(Type, Request, From, Msg, Info, State) -> % -> {Reply, NewState}
    {ok, Expectation, NewState} = pop_expectation(State),
    ?assert(Type =:= Expectation#expectation.type),

    {ok, NewState2} = try call_expectation_lambda(Expectation, Type, Request, From, Msg, Info, NewState) of
        {ok, State2} -> {ok, State2}
    catch
        error:function_clause ->
            ?raise_info(unexpected_request_made, {Expectation, Type, Request, From, Msg, Info, NewState})
    end,

    % {ok, NewState2} = call_expectation_lambda(Expectation, Type, Request, From, Msg, Info, NewState),  
    {ok, NewState2}.

call_expectation_lambda(Expectation, Type, Request, From, Msg, Info, State) -> % {ok, NewState}
    L = Expectation#expectation.lambda,
    Response = case Type of 
       call -> L(Request, From, State);
       cast -> L(Msg, State);
       info -> L(Info, State);
          _ -> L(Request, From, Msg, Info, State)
    end,
    case Response of
        {ok, NewState} -> {ok, NewState};
        ok             -> {ok, State}
    end.
