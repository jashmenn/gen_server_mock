-module(eunit_gen_server_mock).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    % ?TRACE("seed servers", self()),
    % {ok, Node1Pid} = stoplight_srv:start_named(node1, {seed, undefined}),
    % {ok, _Node2Pid} = stoplight_srv:start_named(node2, {seed, Node1Pid}),
    % {ok, _Node3Pid} = stoplight_srv:start_named(node3, {seed, Node1Pid}),
    % [node1, node2, node3].
    ok.

teardown(Servers) ->
    % io:format(user, "teardown: ~p ~p ~n", [Servers, global:registered_names()]),
    % lists:map(fun(Pname) -> 
    %     Pid = whereis(Pname),
    %     % io:format(user, "takedown: ~p ~p ~n", [Pname, Pid]),
    %     gen_cluster:cast(Pid, stop), 
    %     unregister(Pname)
    %  end, Servers),

    % lists:map(fun(Pname) -> 
    %     Pid = global:whereis_name(Pname),
    %     % io:format(user, "takedown: ~p ~p ~n", [Pname, Pid]),
    %     gen_cluster:cast(Pid, stop), 
    %     global:unregister_name(Pname)
    % end, global:registered_names()),
    % ok.
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         ?assert(true =:= true),
         % {ok, State1} = gen_cluster:call(node1, {state}),
         % ?assert(is_record(State1, srv_state) =:= true),

         % {ok, Plist} = gen_cluster:call(node1, {'$gen_cluster', plist}),
         % ?assertEqual(3, length(Plist)),
         {ok}
      end
  }.
