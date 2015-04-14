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

#define ASSERT_INT(f, mesg) do { \
		if ((f) == -1) { \
			warnx(mesg": %s", tls_error(ctx)); rv=1; goto done; \
		} \
        } while(0);

#define ASSERT_CCTX_INT(f, mesg) do { \
		if ((f) == -1) { \
			warnx(mesg": %s", tls_error(cctx)); rv=1; goto done; \
		} \
        } while(0);

#define LEN 6

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <err.h>
#include <errno.h>
#include <netdb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <unistd.h>

#include <bsd/string.h>

#include <tls.h>

int to_client(struct tls *, char *);
int from_client(struct tls *, char *, size_t, size_t *);

extern int errno;

/*
 * client: hello
 * server: 4
 */
int
main()
{
	struct tls_config	*config;
	struct tls		*ctx, *cctx;
	size_t			 outlen;
	char			*buf;
	/* const char		*ciphers = "DES-CBC-SHA"; */
	int			 sock, rv = 0;
	struct sockaddr_un	 addr;

	cctx = NULL;

	if ((buf = calloc(LEN, sizeof(char))) == NULL)
		err(1, "calloc");

	if (tls_init() == -1)
		errx(1, "tls_init failed");

	if ((config = tls_config_new()) == NULL)
		errx(1, "tls_config_new");

	if ((ctx = tls_server()) == NULL)
		errx(1, "tls_server");

        ASSERT_INT(tls_config_set_ca_file(
		    config, "/etc/ssl/certs/ca-certificates.crt"),
	    "tls_config_set_ca_file");
	ASSERT_INT(tls_config_set_key_file(
		    config,
		    "/home/rebecca/erltls/test/thekey.key"),
	    "tls_config_set_key_file");
	ASSERT_INT(tls_config_set_cert_file(
		    config,
		    "/home/rebecca/erltls/test/thecert.crt"),
	    "tls_config_set_cert_file");

	ASSERT_INT(tls_configure(ctx, config), "tls_configure");

	ASSERT_INT((sock = socket(AF_UNIX, SOCK_STREAM, 0)), "socket");

	memset(&addr, 0, sizeof(struct sockaddr_un));
	addr.sun_family = AF_UNIX;
	strlcpy(addr.sun_path, "/tmp/foo", 9);
	if (bind(sock, (struct sockaddr *)&addr, sizeof(struct sockaddr_un)) == -1) {
		rv = 1;
		warn("bind");
		goto done;
	}

	if (listen(sock, 50) == -1) {
		rv = 1;
		warn("listen");
		goto done;
	}

	ASSERT_CCTX_INT(tls_accept_socket(ctx, &cctx, sock),
	    "tls_accept_socket");

	do {
		ASSERT_CCTX_INT(from_client(cctx, buf, LEN, &outlen), "tls_read");
	} while (LEN <= outlen);

	to_client(cctx, "4");

done:

	tls_close(ctx);
	close(sock);

	tls_free(ctx);
	tls_config_free(config);

	return rv;
}

/*
 * Send a HTTP command to the host.
 */
int
to_client(struct tls *ctx, char *mesg)
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
		rv = to_client(ctx, mesg);
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
from_client(struct tls *ctx, char *buf, size_t len, size_t *outlen)
{
	int	 ret, rv = 0;

	ret = tls_read(ctx, buf, len, outlen);
	switch (ret) {
	case -1:
		rv = -1;
		break;
	case TLS_READ_AGAIN:
		rv = from_client(ctx, buf, len, outlen);
		break;
	case 0:
		rv = 0;
		break;
	}

	return rv;
}
