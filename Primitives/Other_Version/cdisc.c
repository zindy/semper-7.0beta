#include "ftypes.h"
#include "dcache.h"

#ifdef ANSI_C
F_logical csemtou ( char* );
#else
F_logical csemtou ( );
#endif

logicalfunction cdisc_ ( IOP, DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF, IERROR )

F_integer     *IOP;
F_integer     *DEVICE;
F_longinteger *NITEMS;
F_integer     *MEM;
F_longinteger *BLKN;
F_integer     *MEMF;
F_integer     *DISCF;
F_integer     *IERROR;
{
    int           opcode;
    DeviceNumber  device_number;
    DeviceNumber  i;
    DeviceNumber  i1;
    DeviceNumber  i2;
    StatusMask    status_mask;
    DeviceSize    device_size;
    CacheIndex    number_found;
    CacheIndexPtr found;

    opcode = *IOP;

    switch ( opcode )
    {
	case 0:
	{
	    found = &number_found;
	    if ( create_cache ( 32, 8192, found ) )
            {
		if ( number_found > 0 )
		{
/*
		    Try again with something smaller
*/
		    if ( !create_cache ( number_found, 8192, found ) )
		    {
			return ( F_FALSE );
		    }
		}

 		csemtou ( "Unable to create default disc cache" );
		*IERROR = 10; return ( F_TRUE );
            }
	}
	break;

	case 1:
	{
	    if ( read_device_data ( DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF ) )
	   {
                csemtou ( "Error case 1" );
		*IERROR = 8; return ( F_TRUE );
	    }
	}
	break;

	case 2:
	{
	    if ( write_device_data ( DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF ) )
	    {
                csemtou ( "Error case 2" );
		*IERROR = 8; return ( F_TRUE );
	    }
	}
	break;

	case 3:
	case 5:
	{
	    device_number = *DEVICE;

	    if ( device_number < 0 )
	    {
                i1 = 0;
                i2 = MAXDEV;
            }

            else
            {
                i1 = device_number;
                i2 = device_number;
            }

            for ( i = i1; i <= i2; i++ )
            {
                query_device ( i, &status_mask, &device_size );

                if ( status_mask & DEVICE_OPEN )
                {
                    if ( flush_device ( i, 0, device_size ) )
                    {
                         csemtou ( "Error case 3" );
                        *IERROR = 8; return ( F_TRUE );
                    }
                }
	    }
	}
	break;

	case 4:
        {
            if ( free_cache ( ) )
            {
		csemtou ( "Error while trying to free cache" );

		*IERROR = 10; return ( F_TRUE );
            }
        }
        break;

	case 6:
	{
	    device_number = *DEVICE;
	    if ( open_memory ( device_number ) )
	    {
		csemtou ( "Error while trying to enable memory buffering" );
		*IERROR = 10; return ( F_TRUE );
	    }
        }
	break;

	case 7:
	{
	    device_number = *DEVICE;

	    if ( close_memory ( device_number ) )
	    {
		csemtou ( "Error while trying to disable memory buffering" );

		*IERROR = 10; return ( F_TRUE );
	    }
	}
	break;

	default:
	{
	    csemtou ( "Invalid opcode in DISC" );

	    *IERROR = 10; return ( F_TRUE );
	}
    }

    return ( F_FALSE );
}
logicalfunction cdisc ( IOP, DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF, IERROR )

F_integer     *IOP;
F_integer     *DEVICE;
F_longinteger *NITEMS;
F_integer     *MEM;
F_longinteger *BLKN;
F_integer     *MEMF;
F_integer     *DISCF;
F_integer     *IERROR;
{
        return cdisc_ ( IOP, DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF, IERROR );
}

