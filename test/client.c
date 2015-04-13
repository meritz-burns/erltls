/*
 * OpenBSD:
 *   gcc -g -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libssl --cflags --libs) -ltls client.c -o client
 */
#define TLS_INT(f, mesg) do { \
		if (f == -1) { \
			warnx(mesg": %s", tls_error(ctx)); rv=1; goto done; \
		} \
        } while(0);

#include <unistd.h>
#include <err.h>
#include <tls.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int	to_server(struct tls *, char *);
static char *	from_server(struct tls *, size_t, size_t *);

int
main()
{
	struct tls_config	*config;
	struct tls		*ctx;
	int			 rv = 0;
	size_t			 len, outlen;
	char			*buf;
	const char		*ciphers = "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-CHACHA20-POLY1305:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-DSS-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA256:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:GOST2012256-GOST89-GOST89:DHE-RSA-CAMELLIA256-SHA256:DHE-DSS-CAMELLIA256-SHA256:DHE-RSA-CAMELLIA256-SHA:DHE-DSS-CAMELLIA256-SHA:GOST2001-GOST89-GOST89:ECDH-RSA-AES256-GCM-SHA384:ECDH-ECDSA-AES256-GCM-SHA384:ECDH-RSA-AES256-SHA384:ECDH-ECDSA-AES256-SHA384:ECDH-RSA-AES256-SHA:ECDH-ECDSA-AES256-SHA:AES256-GCM-SHA384:AES256-SHA256:AES256-SHA:CAMELLIA256-SHA256:CAMELLIA256-SHA:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:DHE-DSS-AES128-GCM-SHA256:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES128-SHA256:DHE-DSS-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA:DHE-RSA-CAMELLIA128-SHA256:DHE-DSS-CAMELLIA128-SHA256:DHE-RSA-CAMELLIA128-SHA:DHE-DSS-CAMELLIA128-SHA:ECDH-RSA-AES128-GCM-SHA256:ECDH-ECDSA-AES128-GCM-SHA256:ECDH-RSA-AES128-SHA256:ECDH-ECDSA-AES128-SHA256:ECDH-RSA-AES128-SHA:ECDH-ECDSA-AES128-SHA:AES128-GCM-SHA256:AES128-SHA256:AES128-SHA:CAMELLIA128-SHA256:CAMELLIA128-SHA:IDEA-CBC-SHA:ECDHE-RSA-RC4-SHA:ECDHE-ECDSA-RC4-SHA:ECDH-RSA-RC4-SHA:ECDH-ECDSA-RC4-SHA:RC4-SHA:RC4-MD5:ECDHE-RSA-DES-CBC3-SHA:ECDHE-ECDSA-DES-CBC3-SHA:EDH-RSA-DES-CBC3-SHA:EDH-DSS-DES-CBC3-SHA:ECDH-RSA-DES-CBC3-SHA:ECDH-ECDSA-DES-CBC3-SHA:DES-CBC3-SHA:EDH-RSA-DES-CBC-SHA:EDH-DSS-DES-CBC-SHA:DES-CBC-SHA";

	len = 1024;

	if (tls_init() == -1)
		errx(1, "tls_init failed");

	if ((config = tls_config_new()) == NULL)
		errx(1, "tls_config_new");

	if ((ctx = tls_client()) == NULL)
		errx(1, "tls_server");

	TLS_INT(tls_config_set_ciphers(config, ciphers),
	    "tls_config_set_ciphers");
	tls_config_set_protocols(config, TLS_PROTOCOLS_ALL);

	TLS_INT(tls_configure(ctx, config), "tls_configure");
	TLS_INT(tls_connect(ctx, "mike-burns.com", "443"), "tls_connect");

	TLS_INT(to_server(ctx, "GET / HTTP/1.1"), "tls_write");
	TLS_INT(to_server(ctx, "User-Agent: erltls/0.1"), "tls_write");
	TLS_INT(to_server(ctx, "Host: mike-burns.com"), "tls_write");
	TLS_INT(to_server(ctx, "Accept: */*"), "tls_write");
	TLS_INT(to_server(ctx, ""), "tls_write");

	do {
		if ((buf = from_server(ctx, len, &outlen)) == NULL) {
			warnx("tls_read: %s", tls_error(ctx));
			rv = 1;
			goto done;
		}
		write(1, buf, outlen);
	} while (len <= outlen);

done:

	tls_close(ctx);

	free(buf);
	tls_free(ctx);
	tls_config_free(config);

	return rv;
}

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

char *
from_server(struct tls *ctx, size_t len, size_t *outlen)
{
	int	 ret;
	char	*buf;

	if ((buf = calloc(len, sizeof(char))) == NULL)
		err(1, "calloc");

	ret = tls_read(ctx, buf, len, outlen);
	switch (ret) {
	case -1:
		buf = NULL;
		break;
	case TLS_READ_AGAIN:
		buf = from_server(ctx, len, outlen);
		break;
	}

	return buf;
}
