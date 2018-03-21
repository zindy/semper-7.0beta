/*  These include files are used for interfacing to FORTRAN code  */

#include "ftypes.h"
#include "params.h"

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION MCTP61 ( IOP, IDEV, N, IBUFF, IERROR )
 *       -------------------------------------------------------
 *
 *       PARAMETERS:
 *
 *       integer iop : INPUT -  Operation to be carried out.
 *
 *                     iop = 1: Position tape device IDEV to BOT.
 *                     iop = 2: Reads a block from IDEV, tranferring up to
 *                              into IBUFF, resetting N is less is transferred.
 *                     iop = 3: Writes a block of N bytes to IDEV.
 *                     iop = 4: Reserved.
 *                     iop = 5: Writes an EOT mark to IDEV.
 *                     iop = 6: Skips forwards one block, fail if beyond EOT.
 *                     iop = 7: Skips backwards one block.
 *                     iop = 8: Skip forward one file, stop beyond EOF.
 *                     iop = 9: Skip backward one file, stop beyond EOF.
 *                     iop = 10: Assign tape as device IDEV for read/write,
 *                               mounting if needed.
 *                     iop = 11: Assign tape as device IDEV for read only,
 *                               mounting if needed.
 *                     iop = 12: Deassign device IDEV, dismounting if N = 0.
 *
 *
 *       integer idev : INPUT - The device number (from zero) to assign.
 *
 *       integer*4 n : INPUT - Number of bytes to read/write.
 *
 *       integer*(*) ibuff - INPUT/OUTPUT - Buffer for reading or writing.
 *
 *       integer ierror - OUTPUT - Error code.
 *
 *       Performs tape assignment/deassignment, positioning and read/write.
 *
 *----------------------------------------------------------------------
 */

logicalfunction mctp61_ ( iop, idev, n, ibuff, ierror )

F_integer *iop;
F_integer *idev;
F_longinteger *n;
F_integer *ibuff;
F_integer *ierror;

{
    /*  Null function for now  */

    if ( *iop > 10 )
    {
        *ierror = 44;
    }
    else if ( *iop == 2 | *iop == 6 | *iop == 8 | *iop == 9 )
    {
        *ierror = 2;
    }
    else
    {
        *ierror = 1;
    }

    return ( F_TRUE );

/*  Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved  */
}

logicalfunction mctp61 ( iop, idev, n, ibuff, ierror )

F_integer *iop;
F_integer *idev;
F_longinteger *n;
F_integer *ibuff;
F_integer *ierror;

{
        return mctp61_ ( iop, idev, n, ibuff, ierror );
}

