-module(eunit_gen_server_mock).

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

everything_working_normally_test_not_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, Mock} = gen_server_mock:new(),
         gen_server_mock:expect(Mock, call, fun({foo, hi}, _From, _State) -> ok end),
         gen_server_mock:expect_call(Mock, fun({bar, bye}, _From, _State) -> ok end),

         ok = gen_server:call(Mock, {foo, hi}),  
         ok = gen_server:call(Mock, {bar, bye}),  

         ok = gen_server_mock:assert_expectations(Mock),
         {ok}
      end
  }.

names_work_too_test_() ->
	{
		setup, fun setup/0, fun teardown/1,
		fun () ->
			{ok, Mock} = gen_server_mock:named({local, testname}),
			?assertEqual(Mock, whereis(testname))
		end
	}.

missing_expectations_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, Mock} = gen_server_mock:new(),
         gen_server_mock:expect(Mock, call, fun({foo, hi}, _From, _State) -> ok end),
         gen_server_mock:expect_call(Mock, fun({bar, bye}, _From, _State) -> ok end),

         ok = gen_server:call(Mock, {foo, hi}),  

         %% TODO - hide the gen_server termination ERROR REPORT
         Result = try gen_server_mock:assert_expectations(Mock)
         catch
             exit:Exception -> Exception
         end,
         ErrorName = ?exit_error_name(Result),
         ?assertEqual(unmet_gen_server_expectation, ErrorName),
         {ok}
      end
  }.

unexpected_messages_test_not_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, Mock} = gen_server_mock:new(),
         gen_server_mock:expect_call(Mock, fun({bar, bye}, _From, _State) -> ok end),

         %% TODO - hide the gen_server termination ERROR REPORT
         Result = try gen_server:call(Mock, {foo, hi})
         catch
             exit:Exception -> Exception
         end,
         ErrorName = ?exit_error_name(Result),
         ?assertEqual(unexpected_request_made, ErrorName),

         {ok}
      end
  }.

special_return_values_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, Mock} = gen_server_mock:new(),

         gen_server_mock:expect_call(Mock, fun(one,  _From, _State)            -> ok end),
         gen_server_mock:expect_call(Mock, fun(two,  _From,  State)            -> {ok, State} end),
         gen_server_mock:expect_call(Mock, fun(three, _From,  State)           -> {ok, good, State} end),
         gen_server_mock:expect_call(Mock, fun({echo, Response}, _From, State) -> {ok, Response, State} end),
         gen_server_mock:expect_cast(Mock, fun(fish, State) -> {ok, State} end),
         gen_server_mock:expect_info(Mock, fun(cat,  State) -> {ok, State} end),

         ok = gen_server:call(Mock, one),
         ok = gen_server:call(Mock, two),
         good = gen_server:call(Mock, three),
         tree = gen_server:call(Mock, {echo, tree}),
         ok = gen_server:cast(Mock, fish),
         Mock ! cat,

         gen_server_mock:assert_expectations(Mock),
         {ok}
      end
  }.
