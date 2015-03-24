-module(complex_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

tls_config_new_and_free_test_() ->
  ?setup(fun(_) ->
             {ok, ConfigRefNo} = complex:tls_config_new(),
             [?_assertEqual(complex:tls_config_free(ConfigRefNo), ok)]
          end).


start() ->
  Pid = complex:start(),
  timer:sleep(1),
  Pid = whereis(complex),
  ok = complex:tls_init().

stop(_) ->
  complex:stop().
