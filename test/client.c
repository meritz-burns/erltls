/*
 * OpenBSD:
 *   gcc -g -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libssl --cflags --libs) -ltls client.c -o client
 *
 *   ./client
 *
 * Debian:
 *   gcc -g -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libtls libssl --cflags --libs) client.c -o client
 *
 *   ./client /etc/ssl/certs/ca-certificates.crt
 *
 */

#define TLS_INT(f, mesg) do { \
		if (f == -1) { \
			warnx(mesg": %s", tls_error(ctx)); rv=1; goto done; \
		} \
        } while(0);

#define LEN 1024

#include <stdint.h>
#include <unistd.h>
#include <err.h>
#include <tls.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int	to_server(struct tls *, char *);
static int	from_server(struct tls *, char *, size_t, size_t *);

/*
 * Connect to mike-burns.com via HTTPS and print the home page to stdout.
 */
int
main(int argc, char *argv[])
{
	struct tls_config	*config;
	struct tls		*ctx;
	size_t			 outlen;
	char			*buf;
	const char		*ciphers = "DES-CBC3-SHA";
	int			 rv = 0;

	if ((buf = calloc(LEN, sizeof(char))) == NULL)
		err(1, "calloc");

	if (tls_init() == -1)
		errx(1, "tls_init failed");

	if ((config = tls_config_new()) == NULL)
		errx(1, "tls_config_new");

	if ((ctx = tls_client()) == NULL)
		errx(1, "tls_client");

	TLS_INT(tls_config_set_ciphers(config, ciphers),
	    "tls_config_set_ciphers");
	tls_config_set_protocols(config, TLS_PROTOCOLS_ALL);

	if (argc > 1)
		TLS_INT(tls_config_set_ca_file(
			    config,
			    argv[1]),
		    "tls_config_set_ca_file");

	TLS_INT(tls_configure(ctx, config), "tls_configure");
	TLS_INT(tls_connect(ctx, "mike-burns.com", "443"), "tls_connect");

	TLS_INT(to_server(ctx, "GET / HTTP/1.1\r\nUser-Agent: erltls/0.1\r\nHost: mike-burns.com\r\n\r\n"), "tls_write");

	do {
		TLS_INT(from_server(ctx, buf, LEN, &outlen), "tls_read");
		write(1, buf, outlen);
	} while (LEN <= outlen);

done:

	tls_close(ctx);

	free(buf);
	tls_free(ctx);
	tls_config_free(config);

	return rv;
}

/*
 * Send a HTTP command to the host.
 */
int
to_server(struct tls *ctx, char *mesg)
{
	int	 ret, rv;
	size_t	 outlen;

	rv = 0;

	ret = tls_write(ctx, mesg, strlen(mesg), &outlen);
	switch (ret) {
	case -1:
		rv = -1;
		break;
	case TLS_WRITE_AGAIN:
		rv = to_server(ctx, mesg);
		break;
	case 0:
		rv = 0;
		break;
	}

	return rv;
}

/*
 * Read from the host.
 */
int
from_server(struct tls *ctx, char *buf, size_t len, size_t *outlen)
{
	int	 ret, rv = 0;

	ret = tls_read(ctx, buf, len, outlen);
	switch (ret) {
	case -1:
		rv = -1;
		break;
	case TLS_READ_AGAIN:
		rv = from_server(ctx, buf, len, outlen);
		break;
	case 0:
		rv = 0;
		break;
	}

	return rv;
}
