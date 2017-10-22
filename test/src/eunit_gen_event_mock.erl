%%%-------------------------------------------------------------------
%%% File          : eunit_gen_event_mock.erl
%%% Author        : Micah Warren
%%% Organization  : OpenACD
%%% Project       : gen_server_mock
%%% Description   : 
%%%
%%% Created       :  8/25/09
%%%-------------------------------------------------------------------
-module(eunit_gen_event_mock).
-author(micahw).

-include_lib("eunit/include/eunit.hrl").

-define (DEBUG, true).
-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).

-define(exit_error_name(Exception),
    ((fun () ->
    {ErrorName, _Info } = Exception,
    ErrorName
    end)())).

setup() ->
    ok.

teardown(_Arg) ->
    ok.

all_test_() ->
	{setup,
	fun setup/0,
	fun teardown/1,
	[{"everything's okay",
	fun() ->
		{ok, Mock} = gen_event_mock:start(mock1),
		gen_event_mock:expect(Mock, call, fun({foo, hi}, _State) -> ok end),
		gen_event_mock:expect_call(Mock, fun({bar, bye}, _State) -> ok end),
		
		ok = gen_event:call(Mock, gen_event_mock, {foo, hi}),
		ok = gen_event:call(Mock, gen_event_mock, {bar, bye}),
		
		ok = gen_event_mock:assert_expectations(Mock),
		
		gen_event_mock:stop(Mock),
		
		{ok}
	end},
	{"missing expectations",
	fun() ->
		{ok, Mock} = gen_event_mock:start(mock2),
		gen_event_mock:expect(Mock, call, fun({foo, hi}, _State) -> ok end),
		gen_event_mock:expect_call(Mock, fun({bar, bye}, _State) -> ok end),
		
		ok = gen_event:call(Mock, gen_event_mock, {foo, hi}),
		
		?assertError({unmet_gen_event_expectation, _}, gen_event_mock:assert_expectations(Mock)),
%		Result = try gen_event_mock:assert_expectations(Mock)
%		catch
%			exit:Exceptions -> Exceptions
%		end,
%		?debugFmt("Result:  ~p", [Result]),
%		ErrorName = ?exit_error_name(Result),
%		?assertEqual(unmet_gen_leader_expectation, ErrorName),
		
		{ok}
	end},
	{"unexpected_messages",
	fun() ->
		{ok, Mock} = gen_event_mock:start(mock3),
		gen_event_mock:expect_call(Mock, fun({bar, bye}, _State) -> ok end),
		gen_event:call(Mock, gen_event_mock, {foo, hi}),

		?assertError({unexpected_request_made, _}, gen_event_mock:assert_expectations(Mock)),
		
		{ok}
	end},
	{"special return values",
	fun() ->
		{ok, Mock} = gen_event_mock:start(mock4),
		
		gen_event_mock:expect_call(Mock, fun(call_one, _State) -> ok end),
		gen_event_mock:expect_call(Mock, fun(call_two, State) -> {ok, State} end),
		gen_event_mock:expect_call(Mock, fun(call_three, State) -> {ok, good, State} end),
		gen_event_mock:expect_event(Mock, fun(event_one, _State) -> ok end),
		gen_event_mock:expect_event(Mock, fun(event_two, _State) -> ok end),
		gen_event_mock:expect_info(Mock, fun(info_one, _State) -> ok end),
		
		?assertEqual(ok, gen_event:call(Mock, gen_event_mock, call_one)),
		?assertEqual(ok, gen_event:call(Mock, gen_event_mock, call_two)),
		?assertEqual(good, gen_event:call(Mock, gen_event_mock, call_three)),
		?assertEqual(ok, gen_event:notify(Mock, event_one)),
		?assertEqual(ok, gen_event:sync_notify(Mock, event_two)),
		Mock ! info_one,
		
		?assertEqual(ok, gen_event_mock:assert_expectations(Mock)),
		
		gen_event_mock:stop(Mock),
		
		{ok}
	end}]}.