#ifndef ERL_COMM_H
#define ERL_COMM_H

typedef unsigned char byte;

int read_cmd(byte *);
int write_cmd(byte *, int);

#endif
