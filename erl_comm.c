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
