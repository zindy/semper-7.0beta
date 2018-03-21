/*
 *  Semper primitive routine to return hardware specific host id
 *
 *  Author:  Frank Suess
 *  Date:    19th April 1993
 *  Modified 26th April 1994
 *           19th May 1995
 */

#include "ftypes.h"

#ifndef NULL
#define NULL 0
#endif

unsigned long sysid ( unsigned char * );

logicalfunction gethid_ ( host_id )
/*=================================*/

unsigned long *host_id;
{

/*  Fetch host id  */

    *host_id = sysid ( NULL );

    return ( F_FALSE );

/*  Copyright (c) 1993,1994:  Synoptics Ltd,  All Rights Reserved  */
}
