-module(complex).
-export([start/1, start/0, stop/0, init/1]).
-export([foo/1, bar/1, bob/0]).

-record(green, {x, y}).

start() ->
  start("/home/rebecca/libtls-erlang/src/erltls").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

bob() ->
  call_port({bob}).

foo(X) ->
    call_port({foo, X}).

bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller} ->
	    Port ! {self(), {command}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, binary_to_term(Data)}
	    end,
	    loop(Port);
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, binary_to_term(Data)}
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
