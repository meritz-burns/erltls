-module(libtls_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

tls_config_test_() ->
  ?setup(fun(_) ->
             {ok, ConfigRefNo} = libtls:tls_config_new(),
             {ok, ProtocolFlagsNo} = libtls:tls_config_parse_protocols("tlsv1.2"),
             [
               ?_assertEqual(8, ProtocolFlagsNo)
             , ?_assertEqual(ok, libtls:tls_config_set_ca_file(
                               ConfigRefNo, "/fake/ca.crt"))
             , ?_assertEqual(ok, libtls:tls_config_set_ca_path(
                               ConfigRefNo, "/fake-path/"))
             , ?_assertEqual(ok, libtls:tls_config_set_cert_file(
                               ConfigRefNo, "/fake/cert.crt"))
             , ?_assertEqual(ok, libtls:tls_config_set_key_file(
                               ConfigRefNo, "/fake/key"))
             , ?_assertEqual(ok, libtls:tls_config_free(ConfigRefNo))
             ]
          end).


start() ->
  Pid = libtls:start(),
  timer:sleep(1),
  Pid = whereis(libtls),
  ok = libtls:tls_init().

stop(_) ->
  libtls:stop().
