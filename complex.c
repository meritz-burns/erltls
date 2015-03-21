#include <stdlib.h>
#include "complex.h"

int
foo(int x) {
  return x+1;
}

struct green *
bob() {
  struct green *result;

  if ((result = malloc(sizeof(struct green))) == NULL)
    return NULL;

  result->x = 4;
  result->y = 2;

  return result;
}

int bar(int y) {
  return y*2;
}
