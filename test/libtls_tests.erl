-module(libtls_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

tls_config_new_and_free_test_() ->
  ?setup(fun(_) ->
             {ok, ConfigRefNo} = libtls:tls_config_new(),
             [?_assertEqual(libtls:tls_config_free(ConfigRefNo), ok)]
          end).


bin_path() ->
  filename:join([filename:absname(".."), "csrc", "erltls"]).

start() ->
  Bin = bin_path(),
  Pid = libtls:start(Bin),
  timer:sleep(1),
  Pid = whereis(libtls),
  ok = libtls:tls_init().

stop(_) ->
  libtls:stop().
