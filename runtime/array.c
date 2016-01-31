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

#include <stdlib.h>
#include <assert.h>

long* allocate1(long dim1) {
  long* x = malloc(8*dim1);
  for (long i = 0; i < dim1; i++)
    x[i] = 0;
  return x;
}

long* allocate2(long dim1, long dim2) {
  long** x = malloc(8*dim1);
  for (long i = 0; i < dim1; i++) {
    long* y = malloc(8*dim2);
    x[i] = y;
    for (long j = 0; j < dim2; j++)
      y[j] = 0;
  }
  return (long*) x;
}

long* allocate_n_help(long dim, long num_dim, long dims[]) {
  if (dim == num_dim - 1)
    return allocate1(dims[dim]);
  else {
    long** x = malloc(8*dims[dim]);
    for (int i = 0; i < dims[dim]; i++) {
      x[i] = allocate_n_help(dim+1, num_dim, dims);
    }
    return (long*) x;
  }
}

long* allocate_n(long num_dim, long dims[]) {
  return allocate_n_help(0, num_dim, dims);
}

long* allocate3(long dim1, long dim2, long dim3) {
  long dims[] = {dim1, dim2, dim3};
  return allocate_n(3, dims);
}

long* allocate4(long dim1, long dim2, long dim3, long dim4) {
  long dims[] = {dim1, dim2, dim3, dim4};
  return allocate_n(4, dims);
}

long* allocate5(long dim1, long dim2, long dim3, long dim4, long dim5) {
  long dims[] = {dim1, dim2, dim3, dim4, dim5};
  return allocate_n(5, dims);
}

long* allocate6(long dim1, long dim2, long dim3, long dim4, long dim5, long dim6) {
  long dims[] = {dim1, dim2, dim3, dim4, dim5, dim6};
  return allocate_n(6, dims);
}

long* allocate7(long dim1, long dim2, long dim3, long dim4, long dim5, long dim6, long dim7) {
  long dims[] = {dim1, dim2, dim3, dim4, dim5, dim6, dim7};
  return allocate_n(7, dims);
}
