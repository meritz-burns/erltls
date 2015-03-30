% Copyright (c) 2015 Mike Burns, Rebecca Meritz <rebecca@meritz.com>
%
% Permission to use, copy, modify, and distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(libtls).
-export([start/0, start/1, stop/0, init/1]).
-export([
         tls_init/0
        ,tls_config_new/0
        ,tls_config_free/1
        ,tls_config_set_ca_file/2
        ,tls_config_set_ca_path/2
        ,tls_config_set_cert_file/2
        ,tls_config_set_key_file/2
        ,tls_config_parse_protocols/1
        ,tls_config_set_protocols/2
        ,tls_config_insecure_noverifycert/1
        ,tls_config_insecure_noverifyname/1
        ,tls_config_verify/1
        ,tls_config_clear_keys/1
        ,tls_config_set_ciphers/2
        ,tls_config_set_dheparams/2
        ,tls_config_set_ecdhecurve/2
        ,tls_client/0
        ,tls_server/0
        ,tls_configure/2
        ,tls_free/1
        ,tls_connect/3
        ]).

start() ->
  Bin = filename:join(["c_src", "erltls"]),
  start(Bin).

start(C_BIN) ->
    spawn(?MODULE, init, [C_BIN]).

stop() ->
    ?MODULE ! stop.

tls_init() ->
    call_port({tls_init}).

tls_config_new() ->
    call_port({tls_config_new}).

tls_config_free(ConfigRefNo) ->
    call_port({tls_config_free, ConfigRefNo}).

tls_config_set_ca_file(ConfigRefNo, CAFile) ->
    call_port({tls_config_set_ca_file, ConfigRefNo, CAFile}).

tls_config_set_ca_path(ConfigRefNo, CAPath) ->
    call_port({tls_config_set_ca_path, ConfigRefNo, CAPath}).

tls_config_set_cert_file(ConfigRefNo, CertFile) ->
    call_port({tls_config_set_cert_file, ConfigRefNo, CertFile}).

tls_config_set_key_file(ConfigRefNo, CAPath) ->
    call_port({tls_config_set_key_file, ConfigRefNo, CAPath}).

tls_config_parse_protocols(Protostring) ->
    call_port({tls_config_parse_protocols, Protostring}).

tls_config_set_protocols(ConfigRefNo, ProtocolsNo) ->
    call_port({tls_config_set_protocols, ConfigRefNo, ProtocolsNo}).

tls_config_insecure_noverifycert(ConfigRefNo) ->
    call_port({tls_config_insecure_noverifycert, ConfigRefNo}).

tls_config_insecure_noverifyname(ConfigRefNo) ->
    call_port({tls_config_insecure_noverifyname, ConfigRefNo}).

tls_config_verify(ConfigRefNo) ->
    call_port({tls_config_verify, ConfigRefNo}).

tls_config_clear_keys(ConfigRefNo) ->
    call_port({tls_config_clear_keys, ConfigRefNo}).

tls_config_set_ciphers(ConfigRefNo, Ciphers) ->
    call_port({tls_config_set_ciphers, ConfigRefNo, Ciphers}).

tls_config_set_dheparams(ConfigRefNo, Params) ->
    call_port({tls_config_set_dheparams, ConfigRefNo, Params}).

tls_config_set_ecdhecurve(ConfigRefNo, Name) ->
    call_port({tls_config_set_ecdhecurve, ConfigRefNo, Name}).

tls_client() ->
    call_port({tls_client}).

tls_server() ->
    call_port({tls_server}).

tls_configure(TLSRefNo, ConfigRefNo) ->
    call_port({tls_configure, TLSRefNo, ConfigRefNo}).

tls_free(TLSRefNo) ->
    call_port({tls_free, TLSRefNo}).

tls_connect(TLSRefNo, Hostname, Port) ->
    call_port({tls_connect, TLSRefNo, Hostname, Port}).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller} ->
	    Port ! {self(), {command}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, binary_to_term(Data)}
	    end,
	    loop(Port);
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, binary_to_term(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.
