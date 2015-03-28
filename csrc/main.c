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
#include <stdint.h>

#include "tls.h"

#include <erl_interface.h>
#include <ei.h>

#include "erl_comm.h"

typedef int (*CONFIG_STRING_SETTER)(struct tls_config *, const char *);

static void encode_ok(char *, int *);
static void encode_error(char *, int *);
static void encode_ok_tuple_header(char *, int *);
static void decode_function_call(char *, int *, char *);
static void config_set_string(char *, int *, char *, int *, CONFIG_STRING_SETTER);

static void handle_tls_init(char *, int *, char *, int *);
static void handle_tls_config_new(char *, int *, char *, int *);
static void handle_tls_config_free(char *, int *, char *, int *);
static void handle_tls_config_set_ca_file(char *, int *, char *, int *);
static void handle_tls_config_set_ca_path(char *, int *, char *, int *);
static void handle_tls_config_set_cert_file(char *, int *, char *, int *);
static void handle_tls_config_set_key_file(char *, int *, char *, int *);
static void handle_tls_config_parse_protocols(char *, int *, char *, int *);
static void handle_tls_config_set_protocols(char *, int *, char *, int *);
static void handle_tls_config_insecure_noverifyname(char *, int *, char *, int *);
static void handle_tls_config_insecure_noverifycert(char *, int *, char *, int *);

struct handle {
	char name[MAXATOMLEN];
	void (*handler)(char *, int *, char *, int *);
};

struct tls_config *configs[100];
long config_idx = 0;

struct handle handles[] = {
	{"tls_init", handle_tls_init},
	{"tls_config_new", handle_tls_config_new},
	{"tls_config_free", handle_tls_config_free},
	{"tls_config_set_ca_file", handle_tls_config_set_ca_file},
	{"tls_config_set_ca_path", handle_tls_config_set_ca_path},
	{"tls_config_set_cert_file", handle_tls_config_set_cert_file},
	{"tls_config_set_key_file", handle_tls_config_set_key_file},
	{"tls_config_parse_protocols", handle_tls_config_parse_protocols},
	{"tls_config_set_protocols", handle_tls_config_set_protocols},
	{"tls_config_insecure_noverifyname", handle_tls_config_insecure_noverifyname},
	{"tls_config_insecure_noverifycert", handle_tls_config_insecure_noverifycert},

};

int
main()
{
	int i, j, k;
	char funp[MAXATOMLEN], buf[100], out_buf[100];

	while (read_cmd(buf) > 0) {
		i = j = 0;

		decode_function_call(buf, &i, funp);

		for (k = 0; k < 11; k++)
			if (strncmp(funp, handles[k].name, MAXATOMLEN) == 0)
				(handles[k].handler)(buf, &i, out_buf, &j);


		write_cmd(out_buf, j);
	}

	return 0;
}

void
handle_tls_config_insecure_noverifycert(char *buf, int *i, char *out_buf, int *j)
{
	long idx;

	if (ei_decode_long(buf, i, &idx) != 0)
		errx(1, "ei_decode_ei_long");
	tls_config_insecure_noverifycert(configs[idx]);
	encode_ok(out_buf, j);
}

void
handle_tls_config_insecure_noverifyname(char *buf, int *i, char *out_buf, int *j)
{
	long idx;

	if (ei_decode_long(buf, i, &idx) != 0)
		errx(1, "ei_decode_ei_long");
	tls_config_insecure_noverifyname(configs[idx]);
	encode_ok(out_buf, j);
}

void
handle_tls_config_parse_protocols(char *buf, int *i, char *out_buf, int *j)
{
	uint32_t protocols;
	char protostr[100];

	if (ei_decode_string(buf, i, protostr) != 0)
		errx(1, "ei_decode_string");
	if (tls_config_parse_protocols(&protocols, protostr) == 0) {
		encode_ok_tuple_header(out_buf, j);
		if (ei_encode_long(out_buf, j, (long)protocols) != 0)
			errx(1, "ei_encode_long");
	} else {
		encode_error(out_buf, j);
	}
}

void
handle_tls_config_set_protocols(char *buf, int *i, char *out_buf, int *j)
{
	long idx, protocols;

	if (ei_decode_long(buf, i, &idx) != 0)
		errx(1, "ei_decode_ei_long");
	if (ei_decode_long(buf, i, &protocols) != 0)
		errx(1, "ei_decode_ei_long");

	tls_config_set_protocols(configs[idx], (uint32_t)protocols);
	encode_ok(out_buf, j);
}

void
handle_tls_init(char *buf, int *i, char *out_buf, int *j)
{
	if (tls_init() == 0) {
		encode_ok(out_buf, j);
	} else {
		encode_error(out_buf, j);
	}
}

void
handle_tls_config_new(char *buf, int *i, char *out_buf, int *j)
{
	struct tls_config *config;

	if ((config = tls_config_new()) == NULL) {
		encode_error(out_buf, j);
	} else {
		encode_ok_tuple_header(out_buf, j);
		if (ei_encode_long(out_buf, j, config_idx) != 0)
			errx(1, "ei_encode_long");
		configs[config_idx++] = config;
	}
}

void
handle_tls_config_free(char *buf, int *i, char *out_buf, int *j)
{
	long idx;

	if (ei_decode_long(buf, i, &idx) != 0)
		errx(1, "ei_decode_ei_long");
	tls_config_free(configs[idx]);
	configs[idx] = NULL;
	encode_ok(out_buf, j);
}

void
handle_tls_config_set_ca_file(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_ca_file);
}

void
handle_tls_config_set_ca_path(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_ca_path);
}

void
handle_tls_config_set_cert_file(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_cert_file);
}

void
handle_tls_config_set_key_file(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_key_file);

}

void
config_set_string(char *buf, int *i, char *out_buf, int *j, CONFIG_STRING_SETTER tls_config_set)
{
	long idx;
	char string[100];

	if (ei_decode_long(buf, i, &idx) != 0)
		errx(1, "ei_decode_ei_long");
	if (ei_decode_string(buf, i, string) != 0)
		errx(1, "ei_decode_string");

	if (tls_config_set(configs[idx], string) == 0) {
		encode_ok(out_buf, j);
	} else {
		encode_error(out_buf, j);
	}
}

void
decode_function_call(char *buf, int *i, char *funp)
{
	int arity;

	if (ei_decode_version(buf, i, NULL) != 0)
		errx(1, "ei_decode_version");
	if (ei_decode_tuple_header(buf, i, &arity) != 0)
		errx(1, "ei_decode_tuple_header");
	if (ei_decode_atom(buf, i, funp) != 0)
		errx(1, "ei_decode_atom");
}

void
encode_ok(char *out_buf, int *j)
{
	if (ei_encode_version(out_buf, j) != 0)
		errx(1, "ei_encode_version");
	if (ei_encode_atom(out_buf, j, "ok"))
		errx(1, "ei_encode_atom");
}

void
encode_error(char *out_buf, int *j)
{
	if (ei_encode_version(out_buf, j) != 0)
		errx(1, "ei_encode_version");
	if (ei_encode_atom(out_buf, j, "error"))
		errx(1, "ei_encode_atom");
}

void
encode_ok_tuple_header(char *out_buf, int *j)
{
	if (ei_encode_version(out_buf, j) != 0)
		errx(1, "ei_encode_version");
	if (ei_encode_tuple_header(out_buf, j, 2))
		errx(1, "ei_encode_tuple_header");
	if (ei_encode_atom(out_buf, j, "ok"))
		errx(1, "ei_encode_atom");
}
