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
             , ?_assertEqual(ok, libtls:tls_config_insecure_noverifycert(
                                   ConfigRefNo))
             , ?_assertEqual(ok, libtls:tls_config_insecure_noverifyname(
                               ConfigRefNo))
             , ?_assertEqual(ok, libtls:tls_config_verify(ConfigRefNo))
             , ?_assertEqual(ok, libtls:tls_config_set_ciphers(
                                   ConfigRefNo, "secure"))
             , ?_assertEqual(ok, libtls:tls_config_set_dheparams(
                                   ConfigRefNo, "legacy"))
             , ?_assertEqual(error, libtls:tls_config_set_dheparams(
                                   ConfigRefNo, "crap"))
             , ?_assertEqual(ok, libtls:tls_config_set_ecdhecurve(
                                   ConfigRefNo, "auto"))
             , ?_assertEqual(error, libtls:tls_config_set_ecdhecurve(
                                   ConfigRefNo, "crap"))
             ]
          end).

tls_protocols_test_() ->
  ?setup(fun(ConfigRefNo) ->
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
             , ?_assertEqual(ok, libtls:tls_config_set_protocols(ConfigRefNo,
                                                                 TLSV1211))
             ]
          end).

tls_client_test_() ->
  ?setup(fun(ConfigRefNo) ->
             TLS_ALL = 2 bor 4 bor 8,
             Ciphers = "DES-CBC3-SHA",
             Get = "GET / HTTP/1.1\r\nUser-Agent: erltls/0.1\r\nHost: mike-burns.com\r\n\r\n",
             {ok, TLSRefNo} = libtls:tls_client(),
             ok = libtls:tls_config_set_ciphers(ConfigRefNo, Ciphers),
             ok = libtls:tls_config_set_protocols(ConfigRefNo, TLS_ALL),
             ok = libtls:tls_config_set_ca_file(
                 ConfigRefNo, "/etc/ssl/certs/ca-certificates.crt"),
             ok = libtls:tls_configure(TLSRefNo, ConfigRefNo),
             ok = libtls:tls_connect(TLSRefNo, "mike-burns.com", "443"),
             ok = libtls:tls_write(TLSRefNo, Get),
             {ok, Content} =  libtls:tls_read(TLSRefNo),
             [ ?_assertEqual("HTTP/1.1 200 OK", lists:sublist(Content, 15))
             , ?_assertEqual(ok, libtls:tls_close(TLSRefNo))
             , ?_assertEqual(ok, libtls:tls_free(TLSRefNo))
             ]
         end).

tls_server_test_() ->
  ?setup(fun(ConfigRefNo) ->
             {ok, TLSRefNo} = libtls:tls_server(),
             [
               ?_assertEqual({error, "private/public key mismatch"},
                             libtls:tls_configure(TLSRefNo, ConfigRefNo))
             , ?_assertEqual({error, "not a client context"},
                             libtls:tls_connect(TLSRefNo, "localhost", "3333"))
             , ?_assertEqual(ok, libtls:tls_close(TLSRefNo))
             , ?_assertEqual(ok, libtls:tls_free(TLSRefNo))
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
