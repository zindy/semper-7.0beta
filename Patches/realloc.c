/* -----------------------------------------------------------------------------
  Copyright (c) 2005 L. D. Marks

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  ----------------------------------------------------------------------------- */

/* Replacement realloc */
#include <config.h>
#undef realloc
#include <sys/types.h>
void *realloc ();
/* Allocate an N-byte block of memory from the heap.
    If N is zero, allocate a 1-byte block. */
void *
rpl_realloc (size_t n)
{
   if (n == 0)
     n = 1;
   return realloc (n);
}
