gen_server_mock.erl - gen_server mocking
===================

## Summary
`gen_server` mocking. This project is functioning but still young.

## Example

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
         ok   = gen_server:cast(Mock, fish),
         Mock ! cat,
         
         gen_server_mock:assert_expectations(Mock),
         {ok}.

## More Examples 

  See `test/src/eunit_gen_server_mock.erl`

## Usage

Currently three expectations are supported: `expect_call`, `expect_cast`, `expect_info`.

The `fun` argument takes the same arguments as the respective `handle_`
`gen_server` call. However, unlike the `handle_` calls, the response should be
one of `ok`, `{ok, NewState}`, `{ok, Response, NewState}`.  Anything else will
be considered a failure.

For example:

* `expect_call(Mock, fun(Request, From, State) -> ok end)`
* `expect_cast(Mock, fun(Msg, State) -> {ok, State} end)` 
* `expect_info(Mock, fun(Info, State) -> {error, State} end)` (will raise an error, return `{ok, State}` for success)

## Status

Alpha. API may change depending on feedback. Hosted on Github - patches readily accepted.

## Dependencies

* `skelerl`, if you want to run tests
* tested with eunit, should work with other libraries in theory

## Bugs / Problems

I suspect that one could write a more general process mocking library relatively easily.

## References

* [Initial blog post](http://www.xcombinator.com/2009/08/11/testing-erlang-gen_server-with-gen_server_mock/)
* Work inspired by [this post](http://erlang.org/pipermail/erlang-questions/2008-April/034140.html)
* [Github Repo](http://github.com/jashmenn/gen_server_mock) (Patches readily accepted)
* [Mocks Aren't Stubs](http://martinfowler.com/articles/mocksArentStubs.html)

## Authors

* Nate Murray \<nmurray [AT] attinteractive.com\> [github](http://github.com/jashmenn)
