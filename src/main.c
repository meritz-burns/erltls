/*
 * Copyright (c) 2015 Mike Burns, Rebecca Meritz <rebecca@meritz.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <err.h>
#include <stdlib.h>
#include <string.h>

#include <erl_interface.h>
#include <ei.h>

#include "complex.h"
#include "erl_comm.h"

int
main()
{
	int i, j, arity, res;
	long res_struct_index, idx;
	char funp[MAXATOMLEN], buf[100], out_buf[100];
	long longp;
	struct green *res_struct;
	struct green *res_structs[100];

	res = 0;
	res_struct_index = 0;

	while (read_cmd(buf) > 0) {
		i = 0;
		j = 0;
		if (ei_decode_version(buf, &i, NULL) != 0)
			errx(1, "ei_decode_version");
		if (ei_decode_tuple_header(buf, &i, &arity) != 0)
			errx(1, "ei_decode_tuple_header");
		if (ei_decode_atom(buf, &i, funp) != 0)
			errx(1, "ei_decode_atom");

		if (strncmp(funp, "foo", MAXATOMLEN) == 0) {
			if (ei_decode_long(buf, &i, &longp) != 0)
				errx(1, "ei_decode_ei_long");
			res = foo((int)longp);
			if (ei_encode_version(out_buf, &j) != 0)
				errx(1, "ei_encode_version");
			if (ei_encode_long(out_buf, &j, (long)res) != 0)
				errx(1, "ei_encode_long");
		} else if (strncmp(funp, "bar", MAXATOMLEN) == 0) {
			if (ei_decode_long(buf, &i, &longp) != 0)
				errx(1, "ei_decode_ei_long");
			res = bar((int)longp);
			if (ei_encode_version(out_buf, &j) != 0)
				errx(1, "ei_encode_version");
			if (ei_encode_long(out_buf, &j, (long)res) != 0)
				errx(1, "ei_encode_long");
		} else if (strncmp(funp, "bob", MAXATOMLEN) == 0) {
			res_struct = bob();
			if (ei_encode_version(out_buf, &j) != 0)
				errx(1, "ei_encode_version");
			if (ei_encode_tuple_header(out_buf, &j, 2))
				errx(1, "ei_encode_tuple_header");
			if (ei_encode_atom(out_buf, &j, "ok"))
				errx(1, "ei_encode_atom");
			if (ei_encode_long(out_buf, &j, res_struct_index) != 0)
				errx(1, "ei_encode_long");
			res_structs[res_struct_index++] = res_struct;
		} else if (strncmp(funp, "john", MAXATOMLEN) == 0) {
			if (ei_decode_long(buf, &i, &idx) != 0)
				errx(1, "ei_decode_ei_long");
			john(res_structs[idx]);
			if (ei_encode_version(out_buf, &j) != 0)
				errx(1, "ei_encode_version");
			if (ei_encode_atom(out_buf, &j, "ok"))
				errx(1, "ei_encode_atom");
		}

		write_cmd(out_buf, j);
	}

	return 0;
}
