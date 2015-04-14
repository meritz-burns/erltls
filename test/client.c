/*
 * OpenBSD:
 *   gcc -g -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libssl --cflags --libs) -ltls client.c -o client
 *
 * Debian:
 *   gcc -g -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libtls libssl --cflags --libs) client.c -o client
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
main()
{
	struct tls_config	*config;
	struct tls		*ctx;
	size_t			 outlen;
	char			*buf;
	const char		*ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:SRP-DSS-AES-256-CBC-SHA:SRP-RSA-AES-256-CBC-SHA:SRP-AES-256-CBC-SHA:DHE-DSS-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA256:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:DHE-RSA-CAMELLIA256-SHA:DHE-DSS-CAMELLIA256-SHA:ECDH-RSA-AES256-GCM-SHA384:ECDH-ECDSA-AES256-GCM-SHA384:ECDH-RSA-AES256-SHA384:ECDH-ECDSA-AES256-SHA384:ECDH-RSA-AES256-SHA:ECDH-ECDSA-AES256-SHA:AES256-GCM-SHA384:AES256-SHA256:AES256-SHA:CAMELLIA256-SHA:PSK-AES256-CBC-SHA:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:SRP-DSS-AES-128-CBC-SHA:SRP-RSA-AES-128-CBC-SHA:SRP-AES-128-CBC-SHA:DHE-DSS-AES128-GCM-SHA256:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES128-SHA256:DHE-DSS-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA:DHE-RSA-SEED-SHA:DHE-DSS-SEED-SHA:DHE-RSA-CAMELLIA128-SHA:DHE-DSS-CAMELLIA128-SHA:ECDH-RSA-AES128-GCM-SHA256:ECDH-ECDSA-AES128-GCM-SHA256:ECDH-RSA-AES128-SHA256:ECDH-ECDSA-AES128-SHA256:ECDH-RSA-AES128-SHA:ECDH-ECDSA-AES128-SHA:AES128-GCM-SHA256:AES128-SHA256:AES128-SHA:SEED-SHA:CAMELLIA128-SHA:PSK-AES128-CBC-SHA:ECDHE-RSA-RC4-SHA:ECDHE-ECDSA-RC4-SHA:ECDH-RSA-RC4-SHA:ECDH-ECDSA-RC4-SHA:RC4-SHA:RC4-MD5:PSK-RC4-SHA:ECDHE-RSA-DES-CBC3-SHA:ECDHE-ECDSA-DES-CBC3-SHA:SRP-DSS-3DES-EDE-CBC-SHA:SRP-RSA-3DES-EDE-CBC-SHA:SRP-3DES-EDE-CBC-SHA:EDH-RSA-DES-CBC3-SHA:EDH-DSS-DES-CBC3-SHA:ECDH-RSA-DES-CBC3-SHA:ECDH-ECDSA-DES-CBC3-SHA:DES-CBC3-SHA:PSK-3DES-EDE-CBC-SHA:EDH-RSA-DES-CBC-SHA:EDH-DSS-DES-CBC-SHA:DES-CBC-SHA:EXP-EDH-RSA-DES-CBC-SHA:EXP-EDH-DSS-DES-CBC-SHA:EXP-DES-CBC-SHA:EXP-RC2-CBC-MD5:EXP-RC4-MD5";
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
        TLS_INT(tls_config_set_ca_file(config, "/etc/ssl/certs/ca-certificates.crt"), "tls_config_set_ca_file");

	TLS_INT(tls_configure(ctx, config), "tls_configure");
	TLS_INT(tls_connect(ctx, "mike-burns.com", "443"), "tls_connect");

	TLS_INT(to_server(ctx, "GET / HTTP/1.1"), "tls_write");
	TLS_INT(to_server(ctx, "User-Agent: erltls/0.1"), "tls_write");
	TLS_INT(to_server(ctx, "Host: mike-burns.com"), "tls_write");
	TLS_INT(to_server(ctx, ""), "tls_write");

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
	char	*srv_mesg;
	int	 ret, rv;
	size_t	 len, outlen;

	rv = 0;

	len = strlen(mesg) + 2;
	if ((srv_mesg = calloc(len + 1, sizeof(char))) == NULL)
		err(1, "calloc");

	snprintf(srv_mesg, len + 1, "%s\r\n", mesg);

	ret = tls_write(ctx, srv_mesg, len, &outlen);
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

	free(srv_mesg);

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
