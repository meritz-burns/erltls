bin_PROGRAMS = erltls
dist_erltls_SOURCES = main.c erl_comm.c erl_comm.h
erltls_CPPFLAGS = -I$(ERLANG_ROOT_DIR)/lib/erl_interface-$(ERLANG_LIB_VER_erl_interface)/include/ $(TLS_CFLAGS) $(SSL_CFLAGS)
erltls_LDFLAGS = -L$(ERLANG_ROOT_DIR)/lib/erl_interface-$(ERLANG_LIB_VER_erl_interface)/lib $(TLS_LIBS) $(SSL_LIBS)
erltls_LDADD = -lerl_interface -lei
erltls_CFLAGS = -Wall -Wextra -pedantic-errors -Werror -Wno-unused-parameter
