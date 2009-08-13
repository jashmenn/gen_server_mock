%%%-------------------------------------------------------------------
%%% File          : gen_leader_mock.erl
%%% Author        : Micah Warren
%%% Organization  : SpiceCSM
%%% Project       : gen_server_mock
%%% Description   : 
%%%
%%% Created       :  8/13/09
%%%-------------------------------------------------------------------
-module(gen_leader_mock).

% gen_leader callbacks
-export([init/1,
		elected/3,
		surrendered/3,
		handle_DOWN/3,
		handle_leader_call/4,
		handle_leader_cast/3,
		from_leader/3,
		handle_call/4,
		handle_cast/3,
		handle_info/2,
		terminate/2,
		code_change/4]).

% API
-export([
	start/1,
	start/2,
	start_link/1,
	start_link/2,
	expect/3,
	expect_leader_call/2,
	expect_leader_cast/2,
	expect_call/2,
	expect_cast/2,
	expect_info/2,
	assert_expectations/1,
	stop/1
]).

-record(state, {
	expectations = []
}).

-type(expectation_type() :: 'leader_call' | 'leader_cast' | 'call' | 'cast' | 'info').

-record(expectation, {
	type :: expectation_type(),
	lambda :: fun()
}).

% steal assert from gen_server_mock, which was stolen from eunit
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

% =====
% API
% =====

%% @doc Start a mock gen_leader with the given name and default options.
%-spec(start/1 :: (Name :: atom()) -> {'ok', pid()}).
start(Name) ->
	start(Name, []).

-type(response() :: 'ok' | {'ok', any()} | {'ok', any(), any()}).
-type(leader_call() :: {'leader_call', fun((any(), {any(), pid()}, any(), any()) -> response())}).
-type(leader_cast() :: {'leader_cast', fun((any(), any(), any()) -> response())}).
-type(call() :: {'cast', fun((any(), {any(), pid()}, any(), any()) -> response())}).
-type(cast() :: {'cast', fun((any(), any(), any()) -> response())}).
-type(info() :: {'info', fun((any(), any()) -> response())}).
-type(option() :: leader_call() | leader_cast() | call() | cast() | info()).
-type(options() :: [option()]).

%% @doc Start a mock gen_leader with the given name and options.
%-spec(start/2 :: (Name :: atom(), Opts :: gen_leader_mock_opts()) -> {'ok', pid()}).
start(Name, Opts) ->
	gen_leader:start(Name, [node()], [], ?MODULE, [Opts], []).

%% @doc Start a mock gen_leader with the given name and default options, and 
%% link to calling process.
%-spec(start_link/1 :: (Name :: atom()) -> {'ok', pid()}).
start_link(Name) ->
	start_link(Name, []).

%% @doc Start a mock gen_leader with the given name and options, and link to 
%% calling process.
%-spec(start_link/2 :: (Name :: atom(), Opts :: gen_leader_mock_opts()) -> {'ok', pid()}).
start_link(Name, Opts) ->
	gen_leader:start_link(Name, [node()], [], ?MODULE, [Opts], []).

%% @doc add an expectation to the messages list
%-spec(expect/3 :: (Mock :: atom(), Type :: expectation_type(), Fun :: fun((...) - response())) -> 'ok').
expect(Mock, Type, Fun) ->
	Expect = #expectation{type = Type, lambda = Fun},
	gen_leader:leader_call(Mock, {expect, Expect}).

%% @doc Add a leader_call expect to the expectations list.
%-spec(expect_leader_call/2 :: (Mock :: atom(), Fun :: fun((...) -> response())) -> 'ok').
expect_leader_call(Mock, Fun) ->
	expect(Mock, leader_call, Fun).

%% @doc Add a leader_cast expectation.
%-spec(expect_leader_cast/2 :: (Mock :: atom(), Fun :: fun((...) -> response())) -> 'ok').
expect_leader_cast(Mock, Fun) ->
	expect(Mock, leader_cast, Fun).

%% @doc Add a call expectation
%-spec(expect_call/2 :: (Mock :: atom(), Fun :: fun((...) -> response())) -> 'ok').
expect_call(Mock, Fun) ->
	expect(Mock, call, Fun).

%% @doc Add a cast expectation
%-spec(expect_cast/2 :: (Mock :: atom(), Fun :: fun((...) -> response())) -> 'ok').
expect_cast(Mock, Fun) ->
	expect(Mock, cast, Fun).

%% @doc Add an info expectation
%-spec(expect_info/2 :: (Mock :: atom(), Fun :: fun((...) -> response())) -> 'ok').
expect_info(Mock, Fun) ->
	expect(Mock, info, Fun).

%% @doc Ensure expectations where met.
%-spec(assert_expectations/1 :: (Mock :: atom() | [atom()]) -> 'ok').
assert_expectations(Mock) when is_atom(Mock); is_pid(Mock) ->
	assert_expectations([Mock]);
assert_expectations([]) ->
	ok;
assert_expectations([Mock | Tail]) when is_atom(Mock); is_pid(Mock) ->
	gen_leader:leader_call(Mock, assert_expectations),
	assert_expectations(Tail).

%% @doc Stop the mock(s).
%-spec(stop/1 :: (Mock :: atom() | [atom()]) -> 'ok').
stop(Mock) when is_atom(Mock); is_pid(Mock) ->
	stop([Mock]);
stop([]) -> 
	ok;
stop([Mock | Tail]) when is_atom(Mock); is_pid(Mock) ->
	gen_leader:leader_cast(Mock, {'$gen_leader_mock', stop}),
	stop(Tail).

% =====
% Init
% =====

%% @hidden
init([Opts]) ->
	Initstate = #state{},
	F = fun({Type, Lambda}, {ok, Accstate}) ->
		Elem = #expectation{type = Type, lambda = Lambda},
		store_expectation(Elem, Accstate)
	end,
	lists:foldl(F, {ok, Initstate}, Opts).

% =====
% handle_leader_call
% =====

%% @hidden
handle_leader_call(state, _From, State, _Election) ->
	{reply, {ok, State}, State};

handle_leader_call({expect, Expectation}, _From, State, _Elec) ->
	{ok, NewState} = store_expectation(Expectation, State),
	{reply, ok, NewState};

handle_leader_call(assert_expectations, _From, State, _Elec) ->
	{ok, NewState} = handle_assert_expectations(State),
	{reply, ok, NewState};

handle_leader_call(Request, From, State, Elec) ->
	{ok, Reply, NewState} = reply_with_next_expectation(leader_call, Request, From, undef, undef, State, Elec),
	{reply, Reply, NewState}.
	
% =====
% handle_leader_cast
% =====

%% @hidden
handle_leader_cast({'$gen_leader_mock', stop}, State, _Elec) ->
	{stop, normal, State};

handle_leader_cast(Msg, State, Elec) ->
	{ok, _Reply, NewState} = reply_with_next_expectation(leader_cast, undef, undef, Msg, undef, State, Elec),
	{noreply, NewState}.

% =====
% handle_call
% =====

%% @hidden
handle_call(Request, From, State, Elec) ->
	{ok, Reply, NewState} = reply_with_next_expectation(call, Request, From, undef, undef, State, Elec),
	{reply, Reply, NewState}.

% =====
% handle_cast
% =====

%% @hidden
handle_cast(Msg, State, Elec) ->
	{ok, _Reply, NewState} = reply_with_next_expectation(cast, undef, undef, Msg, undef, State, Elec),
	{noreply, NewState}.

% =====
% handle_info
% =====

%% @hidden
handle_info(Info, State) ->
	{ok, _Reply, NewState} = reply_with_next_expectation(info, undef, undef, undef, Info, State, undef),
	{noreply, NewState}.

% =====
% stubs (terminate, code change, handle down, etc)
% =====

%% @hidden
terminate(_, _) ->
	ok.
	
%% @hidden
code_change(_OldVsn, State, _Elec, _Extra) ->
	{ok, State}.

%% @hidden
elected(State, _Election, _Node) -> 
	{ok, ok, State}.
	
%% @hidden
surrendered(State, _LeaderState, _Election) -> 
	{ok, State}.
	
%% @hidden
handle_DOWN(_Node, State, _Election) -> 
	{ok, State}.

%% @hidden
from_leader(_Msg, State, _Elec) ->
	{ok, State}.

% =====
% internal functions
% =====

%% @private
%-spec(store_expectation/1 :: (Expectation :: #expectation, State :: #state{}) -> {'ok', #state{}}).
store_expectation(Expectation, State) ->
	NewExpectations = [Expectation | State#state.expectations],
	Newstate = State#state{expectations = NewExpectations},
	{ok, Newstate}.

%% @private
%-spec(pop/1 :: (List :: [any()]) -> {any(), [any()]}).
pop([]) ->
	{undef, []};
pop(List) ->
	{lists:last(List), lists:sublist(List, 1, length(List) - 1)}.

%% @private
%-spec(pop_expectation/1 :: (State :: #state{}) -> {'ok', #expectation{}, #state{}}).
pop_expectation(State) -> % {ok, Expectation, NewState}
    {Expectation, RestExpectations} = case pop(State#state.expectations) of
        {undef, []} -> ?raise(no_gen_leader_mock_expectation);
        {Head, Rest} -> {Head, Rest}
    end,
    NewState = State#state{expectations = RestExpectations},
    {ok, Expectation, NewState}.

%% @hidden
handle_assert_expectations(State) ->
	case length(State#state.expectations) of
		0 -> 
			ok;
		_Number ->
			?raise_info(unmet_gen_leader_expectation, State#state.expectations)
	end,
	{ok, State}.

%% @hidden
reply_with_next_expectation(Type, Request, From, Msg, Info, State, Elec) -> % -> {ok, Reply, NewState}
    {ok, Expectation, NewState} = pop_expectation(State),
    ?assert(Type =:= Expectation#expectation.type), % todo, have a useful error message, "expected this got that" 

    {ok, Reply, NewState2} = try call_expectation_lambda(Expectation, Type, Request, From, Msg, Info, NewState, Elec) of
        {ok, R, State2} -> {ok, R, State2}
    catch
        error:function_clause ->
            ?raise_info(unexpected_request_made, {Expectation, Type, Request, From, Msg, Info, NewState})
    end,
    {ok, Reply, NewState2}.

%% @hidden
call_expectation_lambda(Expectation, Type, Request, From, Msg, Info, State, Elec) -> % {ok, NewState}
    L = Expectation#expectation.lambda,
    Response = case Type of 
		leader_call -> L(Request, From, State, Elec);
		leader_cast -> L(Msg, State, Elec);
       call -> L(Request, From, State, Elec);
       cast -> L(Msg, State, Elec);
       info -> L(Info, State);
          _ -> L(Request, From, Msg, Info, State)
    end,
    case Response of % hmmm
        ok                       -> {ok, ok,       State};
        {ok, NewState}           -> {ok, ok,       NewState};
        {ok, ResponseValue, NewState} -> {ok, ResponseValue, NewState};
        Other -> Other
    end.