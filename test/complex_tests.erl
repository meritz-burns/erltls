-module(complex_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

foo_test_() ->
  ?setup(fun(_) ->
             [?_assertEqual(complex:foo(5), 6)]
          end).

bar_test_() ->
  ?setup(fun(_) ->
             [?_assertEqual(complex:bar(5), 10)]
          end).

john_and_bob_test_() ->
  ?setup(fun(_) ->
             {ok, GreenRef} = complex:bob(),
             [?_assertEqual(complex:john(GreenRef), ok)]
          end).

start() ->
  Pid = complex:start(),
  timer:sleep(1),
  ?assertEqual(Pid, whereis(complex)).

stop(_) ->
  complex:stop().
