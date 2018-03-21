#include "ftypes.h"

/*  Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved  */

/* ---------------------------------------------------------------------

      SUBROUTINE MCTIME (N)
      ---------------------

      PARAMETERS:

      integer n : OUTPUT - The current date and time.

      Returns current date and time in array N(1-7) as follows:

         N(1) = year     (e.g. 1986)
         N(2) = month    (1-12)
         N(3) = day      (1-31)
         N(4) = hour     (0-23)
         N(5) = min      (0-59)
         N(6) = sec      (0-59)
         N(7) = centisec (0-99)

------------------------------------------------------------------------ */

#include <time.h>
#include <sys/time.h>
subroutine mctime_( time_array )

F_integer time_array[];
{
    struct timeval  time_struct;
    struct timezone zone_struct;
    struct tm      *date_struct;

    (void) gettimeofday ( &time_struct, &zone_struct );

    date_struct = localtime ( &time_struct.tv_sec );

    time_array[0] = (F_integer) ( 1900 + date_struct->tm_year );
    time_array[1] = (F_integer) ( 1 + date_struct->tm_mon );
    time_array[2] = (F_integer) date_struct->tm_mday;
    time_array[3] = (F_integer) date_struct->tm_hour;
    time_array[4] = (F_integer) date_struct->tm_min;
    time_array[5] = (F_integer) date_struct->tm_sec;
    time_array[6] = (F_integer) ( ( time_struct.tv_usec + 5000 ) / 10000 );

    return;
}
subroutine mctime( time_array )

F_integer time_array[];
{
        mctime_( time_array );
}
