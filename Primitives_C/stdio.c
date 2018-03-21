/*
 *   Semper primitive routines for doing standard i/o
 *
 *   Author:  Frank Suess
 *   Date:    14th April 1993
 */

#include <unistd.h>
#include <errno.h>
#include "ftypes.h"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

logicalfunction readsi_ ( TEXT, N, text_length )
/*==============================================*/

char      *TEXT;
F_integer *N;
long       text_length;
{
    char *buffer;
    int   n;
    char  cha;
    int   nread;

/*  Fetch pointer to character buffer  */

    buffer = TEXT;

/*  Initialise character count  */

    n = 0;

/*  Loop round till end of line or end of file  */

    do
    {

/*  Read single character from standard input  */

        do
        {
            nread = read ( 0, &cha, 1 );
        }

/*  Try again if interrupted read  */

        while ( nread < 0 && errno == EINTR );

/*  Check for input character  */

        if ( nread > 0 )
        {

/*  Check for end of line (newline character)  */

            if ( cha != '\n' )
            {

/*  Add character to return buffer unless buffer already full  */

                if ( n < text_length ) *buffer++ = cha;

/*  Increment input character count  */

                n++;
            }
        }
    }

/*  Go back for more unless error, end of file or end of line  */

    while ( ! ( nread < 1 || cha == '\n' ) );

/*  Return character count (flag end of file only if line is empty)  */

    if ( nread == 0 && n == 0 )
    {
        *N = -1;
    }

    else
    {
        *N = n > text_length ? text_length : n;
    }

/*  Return status is o.k. unless read error detected  */

    return ( nread < 0 ? F_TRUE : F_FALSE );

/*  Copyright (c) 1993:  Synoptics Ltd,  All Rights Reserved  */
}

logicalfunction writso_ ( TEXT, N, text_length )
/*==============================================*/

char      *TEXT;
F_integer *N;
long       text_length;
{
    char *buffer;
    int   n;

/*  Fetch Fortran arguments  */

    buffer = TEXT;
    n      = *N;

/*  Fault character count which exceeds length of character variable  */

    if ( n > text_length ) return ( F_TRUE );

/*  Write line of text to standard output  */

    if ( writfd ( 1, buffer, n ) ) return ( F_TRUE );

/*  Write <newline> to standard output  */

    if ( writfd ( 1, "\n", 1 ) ) return ( F_TRUE );

/*  Text output o.k.  */

    return ( F_FALSE );

/*  Copyright (c) 1993:  Synoptics Ltd,  All Rights Reserved  */
}

logicalfunction writse_ ( TEXT, N, text_length )
/*==============================================*/

char      *TEXT;
F_integer *N;
long       text_length;
{
    char* buffer;
    int   n;

/*  Fetch Fortran arguments  */

    buffer = TEXT;
    n      = *N;

/*  Fault character count which exceeds length of character variable  */

    if ( n > text_length ) return ( F_TRUE );

/*  Write line of text to standard error  */

    if ( writfd ( 2, buffer, n ) ) return ( F_TRUE );

/*  Write <newline> to standard error  */

    if ( writfd ( 2, "\n", 1 ) ) return ( F_TRUE );

/*  Text output o.k.  */

    return ( F_FALSE );

/*  Copyright (c) 1993:  Synoptics Ltd,  All Rights Reserved  */
}

int writfd ( fd, buffer, n )
/*========================*/

int   fd;
char *buffer;
int   n;
{
    int nwrite;

/*  Loop round until all the text is output  */

    do
    {

/*  Write text  */

        do
        {
            nwrite = write ( fd, buffer, n );
        }

/*  Try again if interrupted write  */

        while ( nwrite < 0 && errno == EINTR );

/*  Check for any characters output  */

        if ( nwrite > 0 )
        {

/*  Increment pointer, decrement count  */

            buffer += nwrite;
            n      -= nwrite;
        }
    }

/*  Go back for more unless error or all text written  */

    while ( ! ( nwrite < 0 || n < 1 ) );

/*  Return status is o.k. unless write error detected  */

    return ( nwrite < 0 ? TRUE : FALSE );

/*  Copyright (c) 1993:  Synoptics Ltd,  All Rights Reserved  */
}

logicalfunction readsi ( TEXT, N, text_length )
/*==============================================*/

char      *TEXT;
F_integer *N;
long       text_length;
{
        return readsi_ ( TEXT, N, text_length );
}

logicalfunction writso ( TEXT, N, text_length )
/*==============================================*/

char      *TEXT;
F_integer *N;
long       text_length;
{
        return writso_ ( TEXT, N, text_length );
}

logicalfunction writse ( TEXT, N, text_length )
/*==============================================*/

char      *TEXT;
F_integer *N;
long       text_length;
{
        return writse_ ( TEXT, N, text_length );
}

