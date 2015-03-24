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

-module(complex).
-export([start/0, start/1, stop/0, init/1]).
-export([foo/1, bar/1, bob/0, john/1]).
-export([
         tls_init/0
        ]).

start() ->
  start("/home/rebecca/erltls/c_src/erltls").

start(C_BIN) ->
    spawn(?MODULE, init, [C_BIN]).

stop() ->
    ?MODULE ! stop.

% config_free
john(GreenRef) ->
  call_port({john, GreenRef}).

% config_new
bob() ->
  call_port({bob}).

foo(X) ->
    call_port({foo, X}).

bar(Y) ->
    call_port({bar, Y}).

tls_init() ->
    call_port({tls_init}).

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
