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

#include <sys/param.h>
#include "ftypes.h"
#include <time.h>
#include <errno.h>
#include <stdio.h>

usleep_ ( micro_seconds )
/*=====================*/

F_longinteger *micro_seconds;

{
    long ticks;				/*  Number of ticks to sleep for */

    struct timespec interval, remainder;
    long   wait_sec, real_seconds;
    long   wait_nsec;
    real_seconds = *micro_seconds ;
    wait_sec = real_seconds / 1000000 ;
    wait_nsec = 1000*(real_seconds - wait_sec*1000000) ;
    
    interval.tv_sec = wait_sec ;
/*    interval.tv_nsec = 10000000 ; */
    interval.tv_nsec = wait_nsec ;
           if (nanosleep(&interval, &remainder) == -1) {
               if (errno == EINTR) {
/*                   (void)printf("nanosleep interrupted\n");
                   (void)printf("Remaining secs: %d\n", remainder.tv_sec);
                   (void)printf("Remaining nsecs: %d\n", remainder.tv_nsec); */
               }
               else perror("nanosleep");
           }

    /*  Calculate the number of ticks equivalent to the number of usecs  */

/*    ticks = ((*micro_seconds) * HZ) / 1000000; */

/*  And nap for that number of ticks  */

/*    (void) sginap ( ticks ); */


/* Copyright (C) 1988:  Synoptics Ltd,  All Rights Reserved    */
}
usleep ( micro_seconds )
/*=====================*/

F_longinteger *micro_seconds;

{
        usleep_ ( micro_seconds );
}

