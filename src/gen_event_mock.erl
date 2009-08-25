%%%-------------------------------------------------------------------
%%% File          : gen_event_mock.erl
%%% Author        : Micah Warren
%%% Organization  : SpiceCSM
%%% Project       : gen_server_mock
%%% Description   : 
%%%
%%% Created       :  8/25/09
%%%-------------------------------------------------------------------

%% @doc  Gen_event_mock has to bend around gen_event a bit to work properly.
%% While it is possible to start a gen event as a mock, it is generally better
%% to have a gen_event already running, but to swap the appropriate handler.
%% for example:
%%
%%	{ok, Logger} = gen_event:start({local, logger}),
%%	gen_event:add_handler(logger, log_module, []),
%%		% now supplant with the mock module
%%	gen_event_mock:supplant(logger, {log_module, []})
%%		% you can now properly check that calls occur, as well as notifyies.

-module(gen_event_mock).
-behaviour(gen_event).

% API
-export([
	start/0,
	start/1,
	supplant/2,
	stop/1,
	expect/3,
	expect_call/2,
	expect_event/2,
	expect_info/2,
	assert_expectations/1
]).

-export([
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
        expectations,
		unexpected = []
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


start() ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, ?MODULE, []),
	{ok, Pid}.

start(Nom) ->
	{ok, Pid} = gen_event:start_link({local, Nom}),
	gen_event:add_handler(Pid, ?MODULE, []),
	{ok, Pid}.

%% @doc See gen_event:swap_Handler
supplant(EventRef, OldHandler) ->
	gen_event:swap_handler(EventRef, OldHandler, {?MODULE, []}).
	
expect(Mock, Type, Callback) ->
    Exp = #expectation{type=Type, lambda=Callback},
    added = gen_event:call(Mock, ?MODULE, {expect, Exp}),
    ok.

expect_call(Mock, Callback) ->
    expect(Mock, call, Callback).

expect_event(Mock, Callback) ->
	expect(Mock, event, Callback).

expect_info(Mock, Callback) ->
	expect(Mock, info, Callback).

assert_expectations(Mock) when is_pid(Mock) ->
    assert_expectations([Mock]);
assert_expectations([H|T]) ->
    case gen_event:call(H, ?MODULE, assert_expectations) of
		ok ->
			assert_expectations(T);
		{Errname, Info} ->
			?raise_info(Errname, Info),
			ok
	end;
assert_expectations([]) ->
    ok.

stop(H) when is_pid(H) ->
    stop([H]);
stop([H|T]) ->
    gen_event:stop(H),
    stop(T);
stop([]) ->
    ok.

%%====================================================================
%% gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%--------------------------------------------------------------------

init(_Args) -> 
    InitialState = #state{expectations=[]},
    {ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: %% handle_event
%%--------------------------------------------------------------------

handle_event(Msg, State) ->
    {ok, _Reply, NewState} = reply_with_next_expectation(event, Msg, State),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%--------------------------------------------------------------------

% return the state
handle_call(state, State) ->
    {ok, {ok, State}, State};

handle_call({expect, Expectation}, State) ->
    {ok, NewState} = store_expectation(Expectation, State),
    {ok, added, NewState};

handle_call(assert_expectations, State) ->
    Reply = handle_assert_expectations(State),
    {ok, Reply, State};

handle_call(Request, State) -> 
    {ok, Reply, NewState} = reply_with_next_expectation(call, Request, State),
    {ok, Reply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    {ok, _Reply, NewState} = reply_with_next_expectation(info, Info, State),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
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
        {undef, []} -> ?raise(no_gen_event_mock_expectation);
        {Head, Rest} -> {Head, Rest}
    end,
    NewState = State#state{expectations = RestExpectations},
    {ok, Expectation, NewState}.

handle_assert_expectations(State) -> % {ok, State}
    ExpLeft = State#state.expectations,
    case length(ExpLeft) > 0 of
        true -> {unmet_gen_event_expectation, ExpLeft};
        false -> 
			case length(State#state.unexpected) of
				0 ->
					ok;
				_Else ->
					[H | _] = State#state.unexpected,
					{unexpected_request_made, H}
			end
    end.

reply_with_next_expectation(Type, Request, State) -> % -> {ok, Reply, NewState}
    {ok, Expectation, NewState} = pop_expectation(State),
	{ok, Reply, NewState2} = case Expectation#expectation.type of
		 Type ->
			try call_expectation_lambda(Expectation, Type, Request, NewState) of
				{ok, R, State2} -> {ok, R, State2}
			catch
				error:function_clause ->
					{ok, ok, NewState#state{unexpected = [{unexpected_request_made, Type, Request} | NewState#state.unexpected]}}
			end;
		_Else ->
			{ok, ok, NewState#state{unexpected = [{unexpected_request_made, Type, Request} | NewState#state.unexpected]}}
	end,
	{ok, Reply, NewState2}.

% hmm what if we want better response. 
call_expectation_lambda(Expectation, Type, Request, State) -> % {ok, NewState}
    L = Expectation#expectation.lambda,
    Response = case Type of
		call -> L(Request, State);
		event -> L(Request, State);
		info -> L(Request, State);
		_ -> L(Request, State)
	end,
    case Response of % hmmm
        ok                       -> {ok, ok,       State};
        {ok, NewState}           -> {ok, ok,       NewState};
        {ok, ResponseValue, NewState} -> {ok, ResponseValue, NewState};
        Other -> Other
    end.