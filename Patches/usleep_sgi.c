/*  Version of the SUN function USLEEP for Silicon Graphics
 *
 *  Coded to be callable from FORTRAN
 */

#include <sys/param.h>
#include "ftypes.h"

usleep_ ( micro_seconds )
/*=====================*/

F_longinteger *micro_seconds;

{
    long ticks;				/*  Number of ticks to sleep for */

    /*  Calculate the number of ticks equivalent to the number of usecs  */

    ticks = ((*micro_seconds) * HZ) / 1000000;

    /*  And nap for that number of ticks  */

    (void) sginap ( ticks );

/* Copyright (C) 1988:  Synoptics Ltd,  All Rights Reserved    */
}
