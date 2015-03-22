bin_PROGRAMS = erltls
dist_erltls_SOURCES = ei.c erl_comm.c erl_comm.h complex.c complex.h
erltls_CPPFLAGS = -I$(ERLANG_ROOT_DIR)/lib/erl_interface-$(ERLANG_LIB_VER_erl_interface)/include/
erltls_LDFLAGS = -L$(ERLANG_ROOT_DIR)/lib/erl_interface-$(ERLANG_LIB_VER_erl_interface)/lib
erltls_LDADD = -lerl_interface -lei 
erltls_CFLAGS = -Wall -Wextra -pedantic-errors -Werror
