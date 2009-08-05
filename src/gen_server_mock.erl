%%%-------------------------------------------------------------------
%%% File    : gen_server_mock.erl
%%% Author  : nmurray@attinteractive.com
%%% Description : Mocking for gen_server. Expectations are ordered, every
%%%    message required and no messages more than are expected are allowed.
%%%
%%% Created     : 2009-08-05
%%% Inspired by: http://erlang.org/pipermail/erlang-questions/2008-April/034140.html
%%%-------------------------------------------------------------------

-module(gen_server_mock).
-behaviour(gen_server).

% API
-export([new/0, 
        expect/3, expect_call/2, expect_info/2, expect_cast/2,
        assertExpectations/1]).

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

assertExpectations(Mock) ->
    gen_server:call(Mock, {assertExpectations}).

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

init(Args) -> 
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

handle_call(Request, From, State) -> 
    {Reply, NewState} = reply_with_next_expectation(call, Request, From, undef, undef, State),
    {reply, Reply, NewState}

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) -> 
    {_Reply, NewState} = reply_with_next_expectation(cast, undef, undef, Msg, undef, State).
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    {_Reply, NewState} = reply_with_next_expectation(info, undef, undef, undef, Info, State).
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

reply_with_next_expectation(Type, Request, From, Msg, Info, State) -> % -> {Reply, NewState}
    % check the type of the next expectation, if it doesn't match raise an error
    % 
    
