/*
 * Example compiler
 * Copyright (C) 2015-2017 Scott Owens
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Trivial I/O runtime library */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <inttypes.h>

// Read in a 64 bit signed integer and return it. First print a > as prompt.
int64_t input() {
  int64_t i;
  printf("> ");
  fflush(stdout);
  scanf("%" PRId64, &i);
  if (errno == ERANGE) {
    printf("Input number does not fit in a 64-bit signed integer\n");
    exit(1);
  }
  return i;
}

// Print a given 64-bt
void output(int64_t i) {
  printf("%" PRId64 "\n", i);
  return;
}

// signal an error
void signal_error(int64_t errn) {
  switch (errn) {
    case 0:
      printf("array bounds error\n");
      break;
    case 1:
      printf("null pointer dereference\n");
      break;
    default:
      printf("unknown runtime error\n");
      break;
  }
  exit(1);
}
