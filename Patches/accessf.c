#include <unistd.h>
#include "ftypes.h"
/* Patch for f2c lack of access (also inquire is broken with -Df2c_i2)

   int access(const char *pathname, int mode); */

integerfunction access_ ( path, pmode ,path_len, pmode_len)
char *path, *pmode ;
long path_len, pmode_len ;
{
        return access (path , R_OK );
}

