#include <unistd.h>
#include "ftypes.h"

/* Buffer routine for getcwd not in f2c */

logicalfunction getcwd_ ( string, string_len)
char *string;
long  string_len;
{
/* Can I get away with this, or obscure= is needed */
        getcwd( string, string_len);
	return F_TRUE ;
}
