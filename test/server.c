/*
 * OpenBSD:
 *   gcc -g -O0 -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libssl --cflags --libs) -ltls server.c -o server
 *
 * Debian:
 *   gcc -g -O0 -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libtls libssl --cflags --libs) -lbsd server.c -o server
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

#include <err.h>
#include <errno.h>
#include <netdb.h>
#include <poll.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <unistd.h>

#ifndef __BSD_VISIBLE
#include <bsd/string.h>
#endif

#include <tls.h>

#define NFDS 1

int to_client(struct tls *, char *);
int from_client(struct tls *, char *, size_t, size_t *);

extern int errno;

/*
 * client: hello
 * server: 4
 */
int
main(int argc, char *argv[])
{
	struct tls_config	*config;
	struct tls		*ctx, *cctx;
	size_t			 outlen;
	char			*buf;
	/* const char		*ciphers = "DES-CBC-SHA"; */
	int			 sock, ret, rv = 0;
	struct sockaddr_in	 addr;
	struct pollfd		 pfds[NFDS];

	cctx = NULL;

	if ((buf = calloc(LEN, sizeof(char))) == NULL)
		err(1, "calloc");

	if (tls_init() == -1)
		errx(1, "tls_init failed");

	if ((config = tls_config_new()) == NULL)
		errx(1, "tls_config_new");

	if ((ctx = tls_server()) == NULL)
		errx(1, "tls_server");

	if (argc > 1)
		ASSERT_INT(tls_config_set_ca_file(config, argv[1]),
		    "tls_config_set_ca_file");
	ASSERT_INT(tls_config_set_key_file(
		    config,
		    "thekey.key"),
	    "tls_config_set_key_file");
	ASSERT_INT(tls_config_set_cert_file(
		    config,
		    "thecert.crt"),
	    "tls_config_set_cert_file");
	tls_config_set_protocols(config, TLS_PROTOCOL_TLSv1_2);

	ASSERT_INT(tls_configure(ctx, config), "tls_configure");

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		rv = 1;
		warn("socket");
		goto done;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(3333);
	addr.sin_len = sizeof(addr);

	if (bind(sock, (struct sockaddr *)&addr, sizeof(struct sockaddr)) == -1) {
		rv = 1;
		warn("bind");
		goto done;
	}

	if (listen(sock, 1) == -1) {
		rv = 1;
		warn("listen");
		goto done;
	}

	ret = tls_accept_socket(ctx, &cctx, sock);
	switch (ret) {
	case -1:
		rv = 1;
		warn("bind");
		goto done;
		break;
	case TLS_READ_AGAIN:
	case TLS_WRITE_AGAIN:
	case 0:
		break;
	}

	for (;;) {
main_loop:
		pfds[0].fd = sock;
		pfds[0].events = POLLIN | POLLOUT;
		ret = poll(pfds, NFDS, 500);
		switch (ret) {
		case -1:
			rv = 1;
			warn("poll");
			goto done;
			break;
		case 0:
			goto main_loop;
			break;
		}

		if (pfds[0].revents & POLLERR) {
			rv = 1;
			warn("POLLERR");
			goto done;
		}

		if (pfds[0].revents & POLLHUP) {
			rv = 1;
			warn("POLLHUP");
			goto done;
		}

		if (pfds[0].revents & POLLNVAL) {
			rv = 1;
			warn("POLLNVAL");
			goto done;
		}

		if (!(pfds[0].revents & POLLIN)) {
			warn("POLLIN");
			goto main_loop;
		}

		if (!(pfds[0].revents & POLLOUT)) {
			warn("POLLOUT");
			goto main_loop;
		}

fprintf(stderr, "about to read from client\n");
		do {
			if (from_client(cctx, buf, LEN, &outlen) == -1) {
fprintf(stderr, "setting rv=1\n");
				rv = 1;
fprintf(stderr, "printing the warning\n");
				warnx("tls_read: %s", tls_error(cctx));
fprintf(stderr, "cleaning up\n");
				goto done;
			}
		} while (LEN <= outlen);

		to_client(cctx, "4");
	}

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
fprintf(stderr, "tls_read finished\n");
	switch (ret) {
	case -1:
		rv = -1;
		break;
	case TLS_READ_AGAIN:
fprintf(stderr, "got TLS_READ_AGAIN\n");
		rv = from_client(ctx, buf, len, outlen);
		break;
	case 0:
		rv = 0;
		break;
	}

	return rv;
}
