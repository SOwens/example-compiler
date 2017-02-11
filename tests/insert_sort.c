#include <inttypes.h>
#include <stdlib.h>

extern int64_t input();
extern void output(int64_t i);

int sort(int64_t a[], int64_t len) {
  int64_t i = 1;
  int64_t y = 1;
  int64_t j = 1;
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
  return i;
}

int64_t test(int64_t i2) {
  int64_t len = 0;
  int64_t *a;
  int64_t i = 0;
  int64_t y = 0;
  len = input();
  a = malloc(len * sizeof(int64_t));
  while (i < len) {
    int64_t y = input();
    a[i] = y;
    i = i + 1;
  }

  i = sort(a,len);

  i = 0;
  while (i < len) {
    int64_t y = a[i];
    output(y);
    i = i + 1;
  }
  return i;
}

int64_t r;

int main(int argc, char** argv) {
  r = test(1);
  return 0;
}
