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

#include <unistd.h>
#include "erl_comm.h"

static int read_exact(char *, int);
static int write_exact(char *, int);

int
read_cmd(char *buf)
{
	int len;

	if (read_exact(buf, 2) != 2)
		return(-1);

	len = (buf[0] << 8) | buf[1];
	return read_exact(buf, len);
}

int
write_cmd(char *buf, int len)
{
	char li;

	li = (len >> 8) & 0xff;
	write_exact(&li, 1);

	li = len & 0xff;
	write_exact(&li, 1);

	return write_exact(buf, len);
}

int
read_exact(char *buf, int len)
{
	int i, got = 0;

	do {
		if ((i = read(0, buf+got, len-got)) <= 0)
			return(i);
		got += i;
	} while (got<len);

	return(len);
}

int
write_exact(char *buf, int len)
{
	int i, wrote = 0;

	do {
		if ((i = write(1, buf+wrote, len-wrote)) <= 0)
			return (i);
		wrote += i;
	} while (wrote<len);

	return (len);
}
