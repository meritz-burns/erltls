libtls bindings for Erlang
==========================

These are our notes as we go along:

Debian installation
-------------------

```sh
apt-get install build-essential erlang-dev
```

OpenBSD installation
--------------------

```sh
pkg_add erlang
```

Build from repo
---------------

First time:

```sh
./autogen.sh && ./configure
```

After changing C code:

```sh
make
```

Try it from erl
---------------

```erlang
c(complex).
complex:start("/home/rebecca/libtls-erlang/erltls").
complex:foo(2).
```

External documentation
----------------------

* [ei documentation][].
* [erl interface tutorial][].
* [libtls man page][].

[ei documentation]: http://erlang.org/doc/man/ei.html
[erl interface tutorial]: http://erlang.org/doc/tutorial/erl_interface.html
[libtls man page]: http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man3/tls_accept_socket.3

C style guide
-------------

*OpenBSD KNF*. See style(9) for details, or [find it online][openbsd-knf]. This
is a way of thinking: explictness in an effort to understand what the software
does, but not too much explictness so that the idea is not muddled by the
syntax. Use these vim settings for the indentation format:

    set sw=0
    set ts=8
    set noet
    set cinoptions=:0,t0,+4,(4

[openbsd-knf]: http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man9/style.9

Authors
-------

* [Mike Burns](https://mike-burns.com)
* [Rebecca Meritz](http://rebecca.meritz.com/)

[Donate to the OpenBSD Foundation](http://www.openbsdfoundation.org/donations.html).
