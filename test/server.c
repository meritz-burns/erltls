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

int		 tls_accept(struct tls *, struct tls **, int);
struct fd_state	*fd_state_init();
int		 do_read(struct tls *, struct fd_state *);
int		 do_write(struct tls *, struct fd_state *);

/*
 * An echo server over TLS.
 */
int
main(int argc, char *argv[])
{
	int			 sock, ret, fd, rv = 0;
	struct fd_state		*state;
	struct sockaddr_in	 sin;
	struct sockaddr_storage	 ss;
	socklen_t		 slen = sizeof(ss);
	struct tls_config	*config;
	struct tls		*ctx, *cctx;

	setvbuf(stdout, NULL, _IONBF, 0);

	cctx = NULL;

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = 0;
	sin.sin_port = htons(3333);

	state = NULL;

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

	if ((fd = accept(sock, (struct sockaddr*)&ss, &slen)) < 0) {
		warn("accept");
		goto done;
	}

	if (tls_accept(ctx, &cctx, fd) == -1) {
		warnx("tls_acept_socket: %s", tls_error(ctx));
		goto done;
	}

	if ((state = fd_state_init()) == NULL) {
		warn("fd_state_init");
		rv = 1;
		goto done;
	}

	ret = do_read(cctx, state);

	if (ret == 0)
		ret = do_write(cctx, state);
done:
	free(state);
	tls_close(ctx);
	close(sock);

	tls_free(ctx);
	tls_config_free(config);

	return rv;
}

int
tls_accept(struct tls *ctx, struct tls **cctx, int fd)
{
	int	ret;

	ret = tls_accept_socket(ctx, cctx, fd);

	if (ret == TLS_READ_AGAIN || ret == TLS_WRITE_AGAIN)
		return tls_accept(ctx, cctx, fd);

	return ret;
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

	if ((ret = tls_read(ctx, buf, sizeof(buf), &outl)) < 0)
		return -1;

	for (i = 0; i < outl; i++)  {
		if (state->buffer_used < sizeof(state->buffer))
			state->buffer[state->buffer_used++] = buf[i];
		if (buf[i] == '\n')
			state->write_upto = state->buffer_used;
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
