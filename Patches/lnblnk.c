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

/* Replacement for lnblnk: works with gcc but not HP's cc */

#include "ftypes.h"

F_integer lnblnk_ (string, string_len)
char *string;
long  string_len;
{
    F_integer ret_val;
    F_integer j, m;

    m = string_len;
    for (j = string_len; j >= 1; --j) {
        if (*(unsigned char *)&string[j - 1] != ' ') {
            goto L10;
        }
        --m;
    }
L10:
    ret_val = m;
    return ret_val;
}
F_integer lnblnk (string, string_len)
char *string;
long  string_len;
{
        return lnblnk_ (string, string_len) ;
}
