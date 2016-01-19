#include <stdio.h>

long input() {
  long i;
  printf("> ");
  fflush(stdout);
  scanf("%ld", &i);
  return i;
}

void output(long i) {
  printf("%ld\n", i);
  return;
}
