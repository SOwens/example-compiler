/*
 * Example compiler
 * Copyright (C) 2015-2016 Scott Owens
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

/* Allocation of multi-dimensional arrays of 64-bit ints. For an array of
 * length n, store the length of the array in element 0, and the array in 1 to
 * n inclusive. We're casting between int64_t and int64_t* which is not
 * guaranteed to work by the C standard, but seems to work on standard OS X and
 * Linux C compilers. */

#include <stdlib.h>
#include <assert.h>

#define S sizeof(int64_t)

/* A 1 dimensional array */
int64_t* allocate1(int64_t dim1) {
  int64_t* x = malloc(S*(dim1+1));
  for (int64_t i = 0; i < dim1; i++)
    x[i+1] = 0;
  x[0] = dim1;
  return x;
}

/* A 2 dimensional array is an array of arrays */
int64_t* allocate2(int64_t dim1, int64_t dim2) {
  int64_t** x = malloc(S*(dim1+1));
  for (int64_t i = 0; i < dim1; i++) {
    int64_t* y = malloc(S*(dim2+1));
    x[i+1] = y;
    for (int64_t j = 0; j < dim2; j++)
      y[j+1] = 0;
    y[0] = dim2;
  }
  ((int64_t*)x)[0] = dim1;
  return (int64_t*) x;
}

/* Allocate dimensions dim to num_dim where the length of each dimension is
 * given by the list dims[] */
int64_t* allocate_n_help(int64_t dim, int64_t num_dim, int64_t dims[]) {
  if (dim == num_dim - 1)
    return allocate1(dims[dim]);
  else {
    int64_t** x = malloc(S*(dims[dim]+1));
    for (int i = 0; i < dims[dim]; i++) {
      x[i+1] = allocate_n_help(dim+1, num_dim, dims);
    }
    ((int64_t*)x)[0] = dims[dim];
    return (int64_t*) x;
  }
}

int64_t* allocate_n(int64_t num_dim, int64_t dims[]) {
  return allocate_n_help(0, num_dim, dims);
}

int64_t* allocate3(int64_t dim1, int64_t dim2, int64_t dim3) {
  int64_t dims[] = {dim1, dim2, dim3};
  return allocate_n(3, dims);
}

int64_t* allocate4(int64_t dim1, int64_t dim2, int64_t dim3, int64_t dim4) {
  int64_t dims[] = {dim1, dim2, dim3, dim4};
  return allocate_n(4, dims);
}

int64_t* allocate5(int64_t dim1, int64_t dim2, int64_t dim3, int64_t dim4, int64_t dim5) {
  int64_t dims[] = {dim1, dim2, dim3, dim4, dim5};
  return allocate_n(5, dims);
}

int64_t* allocate6(int64_t dim1, int64_t dim2, int64_t dim3, int64_t dim4, int64_t dim5, int64_t dim6) {
  int64_t dims[] = {dim1, dim2, dim3, dim4, dim5, dim6};
  return allocate_n(6, dims);
}

int64_t* allocate7(int64_t dim1, int64_t dim2, int64_t dim3, int64_t dim4, int64_t dim5, int64_t dim6, int64_t dim7) {
  int64_t dims[] = {dim1, dim2, dim3, dim4, dim5, dim6, dim7};
  return allocate_n(7, dims);
}
