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
typedef void (*CONFIG_DO)(struct tls_config *);

static void encode_ok(char *, int *);
static void encode_error(char *, int *);
static void encode_ok_tuple_header(char *, int *);
static void decode_function_call(char *, int *, char *);
static void config_set_string(char *, int *, char *, int *, CONFIG_STRING_SETTER);
static void config_run(char *, int *, char *, int *, CONFIG_DO);

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
static void handle_tls_config_verify(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_config_clear_keys(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_config_set_ciphers(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_config_set_dheparams(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_config_set_ecdhecurve(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_client(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_configure(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_free(char *buf, int *i, char *out_buf, int *j);
static void handle_tls_connect(char *buf, int *i, char *out_buf, int *j);

struct handle {
	char name[MAXATOMLEN];
	void (*handler)(char *, int *, char *, int *);
};

struct tls_config *configs[100];
struct tls *ctxs[100];
long config_idx = 0;
long ctx_idx = 0;

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
	{"tls_config_verify", handle_tls_config_verify},
	{"tls_config_clear_keys", handle_tls_config_clear_keys},
	{"tls_config_set_ciphers", handle_tls_config_set_ciphers},
	{"tls_config_set_dheparams", handle_tls_config_set_dheparams},
	{"tls_config_set_ecdhecurve", handle_tls_config_set_ecdhecurve},
	{"tls_client", handle_tls_client},
	{"tls_configure", handle_tls_configure},
	{"tls_free", handle_tls_free},
	{"tls_connect", handle_tls_connect},
};

int
main()
{
	int i, j, k;
	char funp[MAXATOMLEN], buf[100], out_buf[100];

	while (read_cmd(buf) > 0) {
		i = j = 0;

		decode_function_call(buf, &i, funp);

		for (k = 0; k < 20; k++)
			if (strncmp(funp, handles[k].name, MAXATOMLEN) == 0)
				(handles[k].handler)(buf, &i, out_buf, &j);


		write_cmd(out_buf, j);
	}

	return 0;
}

void
handle_tls_client(char *buf, int *i, char *out_buf, int *j)
{
	struct tls *ctx;

	if ((ctx = tls_client()) == NULL) {
		encode_error(out_buf, j);
	} else {
		encode_ok_tuple_header(out_buf, j);
		if (ei_encode_long(out_buf, j, ctx_idx) != 0)
			errx(1, "ei_encode_long");
		ctxs[ctx_idx++] = ctx;
	}
}

void
handle_tls_configure(char *buf, int *i, char *out_buf, int *j)
{
	long current_config_idx, current_ctx_idx;

	if (ei_decode_long(buf, i, &current_ctx_idx) != 0)
		errx(1, "ei_decode_ei_long");
	if (ei_decode_long(buf, i, &current_config_idx) != 0)
		errx(1, "ei_decode_ei_long");
	if (tls_configure(ctxs[current_config_idx], configs[current_config_idx]) == 0) {
		encode_ok(out_buf, j);
	} else {
		encode_error(out_buf, j);
	}
}

void
handle_tls_connect(char *buf, int *i, char *out_buf, int *j)
{
	long current_ctx_idx;
	char hostname[100];
	char port[10];
	if (ei_decode_long(buf, i, &current_ctx_idx) != 0)
		errx(1, "ei_decode_ei_long");
	if (ei_decode_string(buf, i, hostname) != 0)
		errx(1, "ei_decode_string");
	if (ei_decode_string(buf, i, port) != 0)
		errx(1, "ei_decode_string");
	if (tls_connect(ctxs[current_ctx_idx], hostname, port) == 0) {
		encode_ok(out_buf, j);
	} else {
		encode_error(out_buf, j);
	}
}

void
handle_tls_free(char *buf, int *i, char *out_buf, int *j)
{
	long current_ctx_idx;

	if (ei_decode_long(buf, i, &current_ctx_idx) != 0)
		errx(1, "ei_decode_ei_long");
	tls_free(ctxs[current_ctx_idx]);
	ctxs[current_ctx_idx] = NULL;
	encode_ok(out_buf, j);
}

void
handle_tls_config_clear_keys(char *buf, int *i, char *out_buf, int *j)
{
	config_run(buf, i, out_buf, j, tls_config_clear_keys);
}

void
handle_tls_config_verify(char *buf, int *i, char *out_buf, int *j)
{
	config_run(buf, i, out_buf, j, tls_config_verify);
}

void
handle_tls_config_insecure_noverifycert(char *buf, int *i, char *out_buf, int *j)
{
	config_run(buf, i, out_buf, j, tls_config_insecure_noverifycert);
}

void
handle_tls_config_insecure_noverifyname(char *buf, int *i, char *out_buf, int *j)
{
	config_run(buf, i, out_buf, j, tls_config_insecure_noverifyname);
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
handle_tls_config_set_dheparams(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_dheparams);
}

void
handle_tls_config_set_ecdhecurve(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_ecdhecurve);
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
handle_tls_config_set_ciphers(char *buf, int *i, char *out_buf, int *j)
{
	config_set_string(buf, i, out_buf, j, tls_config_set_ciphers);
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
config_run(char *buf, int *i, char *out_buf, int *j, CONFIG_DO config_do)
{
	long idx;

	if (ei_decode_long(buf, i, &idx) != 0)
		errx(1, "ei_decode_ei_long");
	config_do(configs[idx]);
	encode_ok(out_buf, j);
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
