#include <err.h>
#include <stdlib.h>
#include <string.h>

#include <erl_interface.h>
#include <ei.h>

#include "complex.h"
#include "erl_comm.h"

typedef unsigned char byte;

int
main()
{
	ETERM *tuplep, *intp;
	ETERM *fnp, *argp;
	int res;
	byte *buf;
	long allocated, freed;

        if ((buf = calloc(100, sizeof(byte))) == NULL)
          err(1, "calloc");

	erl_init(NULL, 0);

	while (read_cmd(buf) > 0) {
		tuplep = erl_decode(buf);
		fnp = erl_element(1, tuplep);
		argp = erl_element(2, tuplep);

		if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
			res = foo(ERL_INT_VALUE(argp));
		} else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 17) == 0) {
			res = bar(ERL_INT_VALUE(argp));
		}

		intp = erl_mk_int(res);
		erl_encode(intp, buf);
		write_cmd(buf, erl_term_len(intp));

		erl_free_compound(tuplep);
		erl_free_term(fnp);
		erl_free_term(argp);
		erl_free_term(intp);
	}

        free(buf);
}
