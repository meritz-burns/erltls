/*
 * OpenBSD:
 *   gcc -g -O0 -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libssl --cflags --libs) -ltls server.c -o server
 *
 *   ./server
 *   openssl s_client -connect localhost:3333
 *
 * Debian:
 *   gcc -g -O0 -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libtls libssl --cflags --libs) -lbsd server.c -o server
 *
 *   ./server /etc/ssl/certs/ca-certificates.crt
 *   openssl s_client -connect localhost:3333
 *
 * Code modified from Nick Mathewson's libevent book. BSD 3-clause license,
 * copyright 2009 Nick Mathewson.
 *
 * http://www.wangafu.net/~nickm/libevent-book/01_intro.html
 */

#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/select.h>

#include <assert.h>
#include <err.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <tls.h>

#define MAX_LINE 16384

#define ASSERT_INT(f, mesg) do { \
		if ((f) == -1) { \
			warnx(mesg": %s", tls_error(ctx)); rv=1; goto done; \
		} \
        } while(0);

struct fd_state {
	char buffer[MAX_LINE];
	size_t buffer_used;

	size_t n_written;
	size_t write_upto;
};

struct fd_state	*fd_state_init();
int		 do_read(struct tls *, struct fd_state *);
int		 do_write(struct tls *, struct fd_state *);

/*
 * An echo server over TLS.
 */
int
main(int argc, char *argv[])
{
	int			 sock, i, maxfd, ret, fd, rv = 0;
	struct fd_state		*state[FD_SETSIZE];
	struct sockaddr_in	 sin;
	fd_set			 readset, writeset;
	struct sockaddr_storage	 ss;
	socklen_t		 slen = sizeof(ss);
	struct tls_config	*config;
	struct tls		*ctx, *cctx;

	setvbuf(stdout, NULL, _IONBF, 0);

	cctx = NULL;

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = 0;
	sin.sin_port = htons(3333);

	for (i = 0; i < FD_SETSIZE; ++i)
		state[i] = NULL;

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

	ASSERT_INT(tls_configure(ctx, config), "tls_configure");


	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		warn("socket");
		rv = 1;
		goto done;
	}
	fcntl(sock, F_SETFL, O_NONBLOCK);

	if (bind(sock, (struct sockaddr*)&sin, sizeof(sin)) < 0) {
		warn("bind");
		rv = 1;
		goto done;
	}

	if (listen(sock, 1) < 0) {
		warn("listen");
		rv = 1;
		goto done;
	}

	for (;;) {
		maxfd = sock;

		FD_ZERO(&readset);
		FD_ZERO(&writeset);

		FD_SET(sock, &readset);

		for (i = 0; i < FD_SETSIZE; i++)
			if (state[i]) {
				if (i > maxfd)
					maxfd = i;

				FD_SET(i, &readset);
				FD_SET(i, &writeset);
			}

		if (select(maxfd + 1, &readset, &writeset, NULL, NULL) < 0) {
			warn("select");
			rv = 1;
			goto done;
		}

		if (FD_ISSET(sock, &readset)) {
			fd = accept(sock, (struct sockaddr*)&ss, &slen);

			if (fd < 0)
				warn("accept");
			else if (fd > FD_SETSIZE)
				close(fd);
			else {
				fcntl(fd, F_SETFL, O_NONBLOCK);

accept:
				ret = tls_accept_socket(ctx, &cctx, fd);

				if (ret == -1) {
					warnx("tls_acept_socket: %s", tls_error(ctx));
					break;
				} else if (ret == TLS_READ_AGAIN)
					goto accept;
				else if (ret == TLS_WRITE_AGAIN)
					goto accept;

				if ((state[fd] = fd_state_init()) == NULL) {
					warn("fd_state_init");
					rv = 1;
					goto done;
				}
			}
		}

		for (i = 0; i < maxfd+1; i++) {
			ret = 0;

			if (i == sock)
				continue;

			if (FD_ISSET(i, &readset))
				ret = do_read(cctx, state[i]);

			if (ret == 0 && FD_ISSET(i, &writeset))
				ret = do_write(cctx, state[i]);

			if (ret) {
				free(state[i]);
				state[i] = NULL;
				close(i);
			}
		}
	}

done:
	tls_close(ctx);
	close(sock);

	tls_free(ctx);
	tls_config_free(config);

	return rv;
}

struct fd_state *
fd_state_init(void)
{
	struct fd_state	*state;
	
	if ((state = malloc(sizeof(struct fd_state))) == NULL)
		return NULL;

	state->buffer_used = state->n_written = state->write_upto = 0;

	return state;
}

int
do_read(struct tls *ctx, struct fd_state *state)
{
	char		buf[1024];
	unsigned int	i;
	ssize_t		ret;
	size_t		outl = 0;

	for (;;) {
		if ((ret = tls_read(ctx, buf, sizeof(buf), &outl)) < 0)
			break;

		for (i = 0; i < outl; i++)  {
			if (state->buffer_used < sizeof(state->buffer))
				state->buffer[state->buffer_used++] = buf[i];
			if (buf[i] == '\n')
				state->write_upto = state->buffer_used;
		}
	}

	if (ret == -1)
		return -1;
	if (outl == 0)
		return 0;
	if (ret == TLS_READ_AGAIN)
		return do_read(ctx, state);
	return 0;
}

int
do_write(struct tls *ctx, struct fd_state *state)
{
	ssize_t	ret;
	size_t	outl;

	while (state->n_written < state->write_upto) {
		ret = tls_write(ctx, state->buffer + state->n_written,
		    state->write_upto - state->n_written, &outl);

		switch (ret) {
		case -1:
			return -1;
		case TLS_WRITE_AGAIN:
			return 0;
		}

		assert(outl != 0);

		state->n_written += outl;
	}

	if (state->n_written == state->buffer_used)
		state->n_written = state->write_upto = state->buffer_used = 0;

	return 0;
}
