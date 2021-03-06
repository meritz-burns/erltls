libtls bindings for Erlang
==========================

These are our notes as we go along:

Debian installation
-------------------

```sh
apt-get install build-essential erlang-dev
```

Install libressl-portable. To install it into `/opt`, understand and do this:

```sh
curl -LO http://...libressl-portable.tar.gz
tar -zxf libressl-portable*tar.gz
cd libressl
./configure --prefix=/opt
make check
sudo make install
sudo sh -c 'echo /opt/libressl/lib > /etc/ld.so.conf.d/libressl'
sudo ldconfig
echo export PKG_CONFIG_PATH=/opt/libressl/lib/pkgconfig > ~/.profile
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

After changing code:

```sh
./rebar3 compile
```

Cleaning
--------

```sh
./rebar3 clean
```

Testing
-------

```sh
./rebar3 eunit
```

When things go crazy
--------------------

See `rebar.config`: as part of cleaning the Erlang, it also runs `make clean`;
as part of compiling the Erlang, it also runs `make`.

If you just want to try to get things working again without much thought, try
this:

```sh
make distclean ; ./autogen.sh && ./configure
./rebar3 clean
./rebar3 compile
```

Try it from erl
---------------

```sh
./rebar3 shell
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

Released under the [ISC license][LICENSE].

[LICENSE]: LICENSE
