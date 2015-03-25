-module(libtls_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

tls_config_test_() ->
  ?setup(fun(_) ->
             {ok, ConfigRefNo} = libtls:tls_config_new(),
             [
               ?_assertEqual(libtls:tls_config_set_ca_file(
                               ConfigRefNo, "/fake/ca.crt"), ok)
             , ?_assertEqual(libtls:tls_config_set_ca_path(
                               ConfigRefNo, "/fake-path/"), ok)
             , ?_assertEqual(libtls:tls_config_free(ConfigRefNo), ok)
             ]
          end).


start() ->
  Pid = libtls:start(),
  timer:sleep(1),
  Pid = whereis(libtls),
  ok = libtls:tls_init().

stop(_) ->
  libtls:stop().
