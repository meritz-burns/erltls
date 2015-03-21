```
http://erlang.org/doc/man/ei.html#ei_x_encode_double


gcc -Werror -I/usr/lib/erlang/lib/erl_interface-3.7.20/include -L/usr/lib/erlang/lib/erl_interface-3.7.20/lib -o foobar ei.c erl_comm.c complex.c -lerl_interface -lei -lpthread

sudo apt-get install erlang-dev

http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man3/tls_accept_socket.3?query=tls_init&sec=3


http://erlang.org/doc/tutorial/erl_interface.html#id62544

1> c(complex).
{ok,complex}
2> complex:start("/home/rebecca/libtls-erlang/foobar").
<0.48.0>
3> complex:foo(2).

```
