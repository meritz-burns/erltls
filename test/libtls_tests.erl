-module(libtls_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

tls_config_test_() ->
  ?setup(fun(ConfigRefNo) ->
             [
               ?_assertEqual(ok, libtls:tls_config_set_ca_file(
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

tls_protocol_test_() ->
  ?setup(fun(_ConfigRefNo) ->
             {ok, TLSV10} = libtls:tls_config_parse_protocols("tlsv1.0"),
             {ok, TLSV11} = libtls:tls_config_parse_protocols("tlsv1.1"),
             {ok, TLSV12} = libtls:tls_config_parse_protocols("tlsv1.2"),
             {ok, TLSV1211} = libtls:tls_config_parse_protocols(
                                "tlsv1.2, tlsv1.1"),
             {ok, TLSV1210} = libtls:tls_config_parse_protocols(
                                "tlsv1.2, tlsv1.0"),
             error = libtls:tls_config_parse_protocols("not a valid protocal"),
             [
               ?_assertEqual(2, TLSV10)
             , ?_assertEqual(4, TLSV11)
             , ?_assertEqual(8, TLSV12)
             , ?_assertEqual(8 bor 2, TLSV1210)
             , ?_assertEqual(8 bor 4, TLSV1211)
             ]
          end).



start() ->
  Pid = libtls:start(),
  timer:sleep(1),
  Pid = whereis(libtls),
  ok = libtls:tls_init(),
  {ok, ConfigRefNo} = libtls:tls_config_new(),
  ConfigRefNo.

stop(ConfigRefNo) ->
  ok = libtls:tls_config_free(ConfigRefNo),
  libtls:stop().
