-module(eunit_gen_leader_mock).

-include_lib("eunit/include/eunit.hrl").

-define (DEBUG, true).
-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).

-define(exit_error_name(Exception),
    ((fun () ->
    {{{ErrorName, _Info }, _Trace }, _MoreInfo} = Exception,
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
	[{"everything_working_normally", fun () ->
         {ok, Mock} = gen_leader_mock:start(mock1),
		 gen_leader_mock:expect(Mock, call, fun({foo, hi}, _From, _State, _Elec) -> ok end),
		 gen_leader_mock:expect_call(Mock, fun({bar, bye}, _From, _State, _Elec) -> ok end),
		 
		 ok = gen_leader:call(Mock, {foo, hi}),
		 ok = gen_leader:call(Mock, {bar, bye}),
		 
		 ok = gen_leader_mock:assert_expectations(Mock),
		 
		 gen_leader_mock:stop(Mock),
		 
		 {ok}
	end},
	{"missing_expectations", fun () ->
		{ok, Mock} = gen_leader_mock:start(mock2),
		gen_leader_mock:expect(Mock, call, fun({foo, hi}, _From, _State, _Elec) -> ok end),
		gen_leader_mock:expect_call(Mock, fun({bar, bye}, _From, _State, _Elec) -> ok end),

		ok = gen_leader:call(Mock, {foo, hi}),  

		%% TODO - hide the gen_server termination ERROR REPORT
		Result = try gen_leader_mock:assert_expectations(Mock)
		catch
			exit:Exception -> Exception
		end,
		ErrorName = ?exit_error_name(Result),
		?assertEqual(unmet_gen_leader_expectation, ErrorName),
		
		%gen_leader_mock:stop(Mock),
		
		{ok}
      end},
	{"unexpected_messages", fun () ->
		{ok, Mock} = gen_leader_mock:start(mock3),
		gen_leader_mock:expect_call(Mock, fun({bar, bye}, _From, _State, _Elec) -> ok end),

		%% TODO - hide the gen_server termination ERROR REPORT
		Result = try gen_leader:call(Mock, {foo, hi})
		catch
			exit:Exception -> Exception
		end,
		ErrorName = ?exit_error_name(Result),
		?assertEqual(unexpected_request_made, ErrorName),

		%gen_leader_mock:stop(Mock),
		
         {ok}
      end},
	{"special_return_values", fun () ->
		{ok, Mock} = gen_leader_mock:start(mock4),

		gen_leader_mock:expect_leader_call(Mock, fun(lead_call_one, _From, _State, _Elec)	-> ok end),
		gen_leader_mock:expect_leader_call(Mock, fun(lead_call_two, _From, State, _Elec)	-> {ok, State} end),
		gen_leader_mock:expect_leader_call(Mock, fun(lead_call_three, _From, State, _Elec)	-> {ok, good, State} end),
		gen_leader_mock:expect_leader_cast(Mock, fun(lead_cast, State, _Elec) -> {ok, State} end),
		gen_leader_mock:expect_call(Mock, fun(one,  _From, _State, _Elec)            -> ok end),
		gen_leader_mock:expect_call(Mock, fun(two,  _From,  State, _Elec)            -> {ok, State} end),
		gen_leader_mock:expect_call(Mock, fun(three, _From,  State, _Elec)           -> {ok, good, State} end),
		gen_leader_mock:expect_call(Mock, fun({echo, Response}, _From, State, _Elec) -> {ok, Response, State} end),
		gen_leader_mock:expect_cast(Mock, fun(fish, State, _Elec) -> {ok, State} end),
		gen_leader_mock:expect_info(Mock, fun(cat,  State) -> {ok, State} end),

		?assertEqual(ok, gen_leader:leader_call(Mock, lead_call_one)),
		?assertEqual(ok, gen_leader:leader_call(Mock, lead_call_two)),
		?assertEqual(good, gen_leader:leader_call(Mock, lead_call_three)),
		?assertEqual(ok, gen_leader:leader_cast(Mock, lead_cast)),
		?assertEqual(ok, gen_leader:call(Mock, one)),
		?assertEqual(ok, gen_leader:call(Mock, two)),
		?assertEqual(good, gen_leader:call(Mock, three)),
		?assertEqual(tree, gen_leader:call(Mock, {echo, tree})),
		?assertEqual(ok, gen_leader:cast(Mock, fish)),
		Mock ! cat,

		gen_leader_mock:assert_expectations(Mock),
		
		gen_leader_mock:stop(Mock),
		
		{ok}
      end}
	]}.
