-module (test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
    {module, eunit_gen_server_mock},
	{module, eunit_gen_leader_mock},
	{module, eunit_gen_event_mock}
  ].