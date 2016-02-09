#include <inttypes.h>
#include <stdlib.h>

extern int64_t input();
extern void output(int64_t i);

int main(int argc, char** argv) {
  int64_t len = input();
  int64_t *a = malloc(len * sizeof(int64_t));
  unsigned int i = 0;
  while (i < len) {
    int64_t y = input();
    a[i] = y;
    i = i + 1;
  }

  i = 1;
  while (i < len) {
    int64_t y = a[i];
    int64_t j = i - 1;
    while (!(j < 0) && (a[j] > y)) {
      a[j+1] = a[j];
      j = j - 1;
    }
    a[j+1] = y;
    i = i + 1;
  }

  i = 0;
  while (i < len) {
    int64_t y = a[i];
    output(y);
    i = i + 1;
  }
  return 0;
}
