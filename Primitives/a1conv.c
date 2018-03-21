/* Semper 6.4 primitive a1conv                                  */
/* Author:   M.W. Raisbeck     17th March 1988                  */
/* Modified: M.W. Raisbeck     23rd November 1989               */
/* Modified: M.W. Raisbeck     7th June 1991 for Myriad		*/
/* Modified: M.W. Raisbeck     22nd August 1991 for DECstation  */
/* Modified: M.W. Raisbeck     22nd January 1992 for AppleMac   */
/* Modified: M.W. Raisbeck     29th January 1993 for Windows 3  */

#include "ftypes.h"


subroutine a1conv_ ( ibuf, n )

F_integer *ibuf;
F_integer *n;
{
    register F_integer i;		/*  Loop counter		     */
    register F_integer *ptr;		/*  Pointer to char being converted  */
    register int shift;			/*  Amount to shift integer by	     */

    shift = (sizeof(F_integer) - 1) * 8;

    /*  Go through all the 'characters' in the integer buffer converting
     *  them in place from FORTRAN A1 form.
     */

    ptr = ibuf;
    for ( i = *n; i ; i-- )
    {
#ifdef BIGENDIAN
        *ptr = (((unsigned)*ptr) >> shift) & 0xff;
#else
        *ptr = (((unsigned)*ptr)) & 0xff;
#endif
        ptr++;
    }

/* Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved    */
}
subroutine a1conv ( ibuf, n )

F_integer *ibuf;
F_integer *n;
{
    register F_integer i;		/*  Loop counter		     */
    register F_integer *ptr;		/*  Pointer to char being converted  */
    register int shift;			/*  Amount to shift integer by	     */

    shift = (sizeof(F_integer) - 1) * 8;

    /*  Go through all the 'characters' in the integer buffer converting
     *  them in place from FORTRAN A1 form.
     */

    ptr = ibuf;
    for ( i = *n; i ; i-- )
    {
#ifdef BIGENDIAN
        *ptr = (((unsigned)*ptr) >> shift) & 0xff;
#else
        *ptr = (((unsigned)*ptr)) & 0xff;
#endif
        ptr++;
    }

/* Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved    */
}
