/*
 * OpenBSD:
 *   gcc -g -O0 -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libssl --cflags --libs) -ltls server.c -o server
 *
 * Debian:
 *   gcc -g -O0 -std=c99 -Wall -Wextra -pedantic-errors -Werror \
 *     $(pkg-config libtls libssl --cflags --libs) -lbsd server.c -o server
 *
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
int		 do_read(int, struct fd_state *);
int		 do_write(int, struct fd_state *);

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
			fprintf(stderr, "handling %i\n", i);

			if (FD_ISSET(i, &readset))
				ret = do_read(i, state[i]);

			if (ret == 0 && FD_ISSET(i, &writeset))
				ret = do_write(i, state[i]);

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
do_read(int fd, struct fd_state *state)
{
	char	buf[1024];
	int	i;
	ssize_t	ret;

	for (;;) {
		if ((ret = recv(fd, buf, sizeof(buf), 0)) <= 0)
			break;

		for (i = 0; i < ret; i++)  {
			if (state->buffer_used < sizeof(state->buffer))
				state->buffer[state->buffer_used++] = buf[i];
			if (buf[i] == '\n')
				state->write_upto = state->buffer_used;
		}
	}

	if (ret == 0)
		return 1;
	else if (ret < 0) {
		if (errno == EAGAIN)
			return 0;
		return -1;
	}

	return 0;
}

int
do_write(int fd, struct fd_state *state)
{
	ssize_t	ret;

	while (state->n_written < state->write_upto) {
		ret = send(fd, state->buffer + state->n_written,
		    state->write_upto - state->n_written, 0);

		if (ret < 0) {
			if (errno == EAGAIN)
				return 0;
			return -1;
		}

		assert(ret != 0);

		state->n_written += ret;
	}

	if (state->n_written == state->buffer_used)
		state->n_written = state->write_upto = state->buffer_used = 0;

	return 0;
}

#if 0
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
#include <fcntl.h>
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
	/*size_t			 outlen;*/
	char			*buf;
	/* const char		*ciphers = "DES-CBC-SHA"; */
	int			 sock, ret, rv = 0;
	struct sockaddr_in	 addr;
	/* struct pollfd		 pfds[NFDS]; */
	fd_set			 readfds;
	int			 read_fd;
	socklen_t		 sock_len;

	int maxfd, i;
	fd_set writefds, exfds;

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
	/* tls_config_set_protocols(config, TLS_PROTOCOL_TLSv1_2); */

	ASSERT_INT(tls_configure(ctx, config), "tls_configure");

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		rv = 1;
		warn("socket");
		goto done;
	}

	fcntl(sock, F_SETFL, O_NONBLOCK);

if ((ret = fcntl(sock, F_GETFL)) == -1)
	err(1, "fnctl");
fprintf(stderr, "F_GETFL = %i\n", ret);
fprintf(stderr, "O_NONBLOCK = %i\n", O_NONBLOCK);

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
fprintf(stderr, "bind: done\n");

	if (listen(sock, 1) == -1) {
		rv = 1;
		warn("listen");
		goto done;
	}
fprintf(stderr, "listen: done\n");

#if 0
		len = sizeof(struct sockaddr);
		read_fd = accept(sock, (struct sockaddr *)&addr, &len);
		if (read_fd < 0) {
			rv = 1;
			warn("accept");
			goto done;
		}

ret = read(read_fd, buf, LEN);
if (ret <= 0)
	fprintf(stderr, "dude, where's my data?\n");
else
	fprintf(stderr, "read something: %s\n", buf);
#endif

	for (;;) {
		maxfd = sock;

		FD_ZERO(&readfds);
		FD_ZERO(&writefds);
		FD_ZERO(&exfds);

		FD_SET(sock, &readfds);

		for (i = 0; i < FD_SETSIZE; i++) {
			if (i > maxfd)
				maxfd = i;
			FD_SET(i, &readfds);
			FD_SET(i, &writefds);
		}

		if (select(maxfd + 1, &readfds, &writefds, &exfds, NULL) < 0) {
			rv = 1;
			warn("select (%i)", maxfd + 1);
			goto done;
		}

		if (FD_ISSET(sock, &readfds)) {
			fprintf(stderr, "about to read %i\n", sock);

			sock_len = sizeof(struct sockaddr);
			read_fd = accept(sock, (struct sockaddr *)&addr, &sock_len);
			if (read_fd < 0) {
				rv = 1;
				warn("accept");
				goto done;
			} else if (read_fd > FD_SETSIZE)
				close(read_fd);
			else
				fcntl(read_fd, F_SETFL, O_NONBLOCK);

			for (i = 0; i < maxfd + 1; i++) {
				ret = 0;
				if (i == sock)
					continue;

				if (FD_ISSET(i, &readfds))
					ret = recv(i, buf, LEN, 0);

				if (ret == 0 && FD_ISSET(i, &writefds))
					ret = write(i, "OK\r\n", 4);

				if (ret)
					close(i);
			}
		} else
			fprintf(stderr, "sock not in readfds\n");
	}
#if 0
		ret = tls_accept_socket(ctx, &cctx, sock);
fprintf(stderr, "tls_accept_socket: done\n");
#endif /* 0 */

#if 0
		switch (ret) {
		case -1:
			rv = 1;
			warn("tls_accept_socket");
			goto done;
			break;
		case TLS_READ_AGAIN:
		case TLS_WRITE_AGAIN:
			break;
		case 0:
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
			break;
		}
	} while (ret > 0);
#endif /* 0 */


#if 0
	for (;;) {
main_loop:
		memset(&pfds, 0, sizeof(pfds));
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

		to_client(cctx, "4\r\n");

		/*
		if (!(pfds[0].revents & POLLOUT)) {
			warn("POLLOUT");
			goto main_loop;
		}
		*/

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
#endif /* 0 */

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
#endif /* 0 */
