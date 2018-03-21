/*
    Disc access routines - Semper primitive level

    All routines in this module are
    Copyright (C) 1987-1993 : Synoptics Ltd  All Rights Reserved

    Modify: Mark Raisbeck	27/May/1992 - Named scratch files
	    Mark Raisbeck	04/Aug/1992 - WINDOWS port
            Frank Suess         01/Sep/1992 - Ditto + tidy
	    Zoe Harding		15/Oct/1993 - Myriad memory management
*/

#include "ftypes.h"
#include "params.h"
#include "dcache.h"

#ifndef NULL
#define NULL 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifdef ANSI_C
FileName  make_file_name ( F_integer* );
F_logical csemtou ( char* );
#else
FileName  make_file_name ( );
F_logical csemtou ( );
#endif
subroutine mcdc61_ ( IOP, IDEV, N, NUM, IBUFF, IERROR )

F_integer	*IOP;
F_integer	*IDEV;
F_longinteger	*N;
F_integer	*NUM;
F_integer	*IBUFF;
F_integer	*IERROR;
{
    int		 opcode;
    DeviceNumber device_number;
    BlockNumber  number_of_blocks;
    FileName     file_name;
    DeviceSize   new_device_size;

    opcode = *IOP;
/*    printf( "Entrance %d\n", opcode ); */

    switch ( opcode )
    {
	case 3:
	{
	    file_name = make_file_name ( IBUFF );

	    if ( file_name == (FileName) NULL )
	    {
		csemtou ( "Error trying to allocate memory at 3 in MCDC61" );
		(void)printf( "***Error at 3 in MCDC61\n" );
		*IERROR = 10; return;
	    }

	    device_number = *IDEV;

	    if ( open_old_file ( device_number, file_name, TRUE ) )
	    {
                free_memory ( file_name );

		*IERROR = 44; return;
	    }

            free_memory ( file_name );
	}
	break;

	case 4:
	{
	    file_name = make_file_name ( IBUFF );

	    if ( file_name == (FileName) NULL )
	    {
		csemtou ( "Error trying to allocate memory at 4 in MCDC61" );
		(void)printf( "***Error at 4 in MCDC61\n" );
		*IERROR = 10; return;
	    }

	    device_number = *IDEV;

	    if ( open_old_file ( device_number, file_name, FALSE ) )
	    {
                free_memory ( file_name );

		*IERROR = 44; return;
	    }

            free_memory ( file_name );
	}
	break;

	case 5:
	{
	    file_name = make_file_name ( IBUFF );

	    if ( file_name == (FileName) NULL )
	    {
		csemtou ( "Error trying to allocate memory at 5 in MCDC61" );
		(void)printf( "***Error at 5 in MCDC61\n" );
		*IERROR = 10; return;
	    }

	    device_number = *IDEV;

            if ( open_new_file ( device_number, file_name ) )
            {
                free_memory ( file_name );

                *IERROR = 44; return;
            }

            free_memory ( file_name );

            number_of_blocks = *N;

	    new_device_size = number_of_blocks * LNBLK;

	    if ( resize_device ( device_number, new_device_size ) )
	    {
		close_file ( device_number, TRUE );

		*IERROR = 44; return;
	    }
	}
	break;

	case 6:
	{
	    device_number = *IDEV;

	    if ( close_file ( device_number, FALSE ) )
	    {
		*IERROR = 44; return;
	    }

	    if ( close_memory ( device_number ) )
	    {
		*IERROR = 44; return;
	    }
	}
	break;

	case 7:
	{
	    device_number = *IDEV;

	    if ( close_file ( device_number, TRUE ) )
	    {
		*IERROR = 44; return;
	    }

	    if ( close_memory ( device_number ) )
	    {
		*IERROR = 44; return;
	    }
	}
	break;

	case 8:
	{
	    file_name = make_file_name ( IBUFF );

	    if ( file_name == (FileName) NULL )
	    {
		csemtou ( "Error trying to allocate memory at 8 in MCDC61" );
		(void)printf( "***Error at 8 in MCDC61\n" );
		*IERROR = 10; return;
	    }

	    device_number = *IDEV;

            if ( open_scratch_file ( device_number, file_name ) )
            {
                free_memory ( file_name );

                *IERROR = 44; return;
            }

            free_memory ( file_name );

            number_of_blocks = *N;

	    new_device_size = number_of_blocks * LNBLK;

	    if ( resize_device ( device_number, new_device_size ) )
	    {
                close_file ( device_number, TRUE );

                *IERROR = 44; return;
	    }
	}
	break;

	case 9:
	{
	    device_number = *IDEV;

            if ( open_memory ( device_number ) )
	    {
		csemtou ( "Error trying to open memory device at 9a in MCDC61" );
		(void)printf( "***Error at 9a in MCDC61\n" );
		*IERROR = 10; return;
	    }

            new_device_size = *N;

            if ( resize_device ( device_number, new_device_size ) )
	    {
		(void)printf( "***Error at 9b in MCDC61\n" );
                close_memory ( device_number );
		csemtou ( "Error trying to resize memory device at 9b in MCDC61" );
		*IERROR = 10; return;
	    }
	}
	break;

	case 10:
	{
	    device_number = *IDEV;

            if ( close_memory ( device_number ) )
	    {
		csemtou ( "Error trying to close memory device at 10 in MCDC61" );
		(void)printf( "***Error at 10 in MCDC61\n" );
		*IERROR = 10; return;
	    }
	}
	break;

	default:
	{
	    csemtou ( "Invalid opcode in MCDC61" );

	    *IERROR = 10; return;
	}
    }
}

FileName make_file_name ( IBUFF )
/*=============================*/

F_integer *IBUFF;
{
    int      i;
    int      number_of_characters;
    FileName file_name;

    number_of_characters = (int) IBUFF [ 0 ];

    if ( number_of_characters < 0 )
    {
        file_name = (FileName) NULL;
    }

    else
    {
        file_name = (FileName) allocate_memory ( number_of_characters + 1 );

        if ( file_name != (FileName) NULL )
        {
            for ( i = 0; i < number_of_characters; i++ )
            {
                file_name [ i ] = (unsigned char) IBUFF [ i + 1 ];
            }

            file_name [ number_of_characters ] = '\0';
        }
    }

    return ( file_name );
}
subroutine mcdc61 ( IOP, IDEV, N, NUM, IBUFF, IERROR )

F_integer       *IOP;
F_integer       *IDEV;
F_longinteger   *N;
F_integer       *NUM;
F_integer       *IBUFF;
F_integer       *IERROR;
{
        mcdc61_ ( IOP, IDEV, N, NUM, IBUFF, IERROR );
}
