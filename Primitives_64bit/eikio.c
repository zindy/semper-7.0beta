/*  IO routines used by erdwrt.f etc
 *  --------------------------------
 *
 *  Author:   Douglas Reid
 *  Modified: Mark Raisbeck
 *            Frank Suess
 *            Zoe Harding
 *
 *  All routines Copyright (C) 1989-1994 : Synoptics Ltd, All Rights Reserved
 *
 */

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

/*  These include files are used for interfacing to FORTRAN code  */

#include "ftypes.h"
#include "params.h"

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKEXI ( FILE_NAME, EXISTS )
 *      ---------------------------------------------
 *
 *      PURPOSE:
 *
 *      Checks for existence of file with path name FILE_NAME.
 *
 *
 *      PARAMETERS:
 *
 *      CHARACTER*(*) FILE_NAME : INPUT - filename (including path).
 *
 *      LOGICAL EXISTS          : OUTPUT - flag to indicate whether file exists.
 *
 *                                EXISTS = .TRUE.  : file found
 *                                EXISTS = .FALSE. : file not found or some
 *                                                   other error
 *
 *
 *      FUNCTION VALUE:
 *
 *      Function always returns .FALSE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eikexi_ ( file_name, exists, file_name_length )
/*================================================================*/

F_string        file_name;
F_logical       *exists;
F_string_length  file_name_length;
{
    char path [ FILMAX + 1 ];
    int  i;

    /*  Copy path name into local array  */

    for ( i = 0; i < file_name_length; i++ ) path [ i ] = file_name [ i ];

    path [ i ] = '\0';

    /*  Check for existence of file  */

    if ( access ( path, F_OK ) == -1 )
    {
	*exists = F_FALSE;
    }

    else
    {
	*exists = F_TRUE;
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKDEL ( FILE_NAME )
 *      -------------------------------------
 *
 *      PURPOSE:
 *
 *      Deletes file whose path name is FILE_NAME.
 *
 *      PARAMETERS:
 *
 *      CHARACTER*(*) FILE_NAME : INPUT - filename (including path).
 *
 *      FUNCTION VALUE:
 *
 *      Function always returns .FALSE. if file successfully deleted else
 *      returns .TRUE.
 *
 *---------------------------------------------------------------
 */


logicalfunction eikdel_ ( file_name, file_name_length )
/*========================================================*/

F_string         file_name;
F_string_length  file_name_length;
{
    char path [ FILMAX + 1 ];
    int  i;

    /*  Copy path name into local array  */

    for ( i = 0; i < file_name_length; i++ ) path [ i ] = file_name [ i ];

    path [ i ] = '\0';

    /*  Delete the file  */

    if ( unlink( path ) == -1 )
    {
	return ( F_TRUE );
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKOPE ( IOP, FD, FILE )
 *      -----------------------------------------
 *
 *      PURPOSE:
 *
 *      Opens file with path name FILE_NAME for read only/write only according
 *      to IOP.  Returns file descriptor in FD.
 *
 *      PARAMETERS:
 *
 *      INTEGER IOP             : INPUT - Type of open to be performed.
 *
 *                                IOP = 1: Open for reading only.
 *                                IOP = 2: Open for writing only.
 *
 *      INTEGER FD              : OUTPUT - file descriptor of successfully
 *                                         opened file.
 *
 *      CHARACTER*(*) FILE_NAME : INPUT - filename (including path).
 *
 *      FUNCTION VALUE:
 *
 *      Function returns .TRUE. if unable to open file, else returns .FALSE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eikope_ ( iop, fd, file_name, file_name_length )
/*=================================================================*/

F_integer       *iop;
F_integer       *fd;
F_string         file_name;
F_string_length  file_name_length;
{
    char path [ FILMAX + 1 ];
    int  i;
    int  opcode;
    int  file_descriptor;

    /*  Copy path name into local array  */

    for ( i = 0; i < file_name_length; i++ ) path [ i ] = file_name [ i ];

    path [ i ] = '\0';

    opcode = *iop;

    /*  Open file  */

    if ( opcode == 1 )
    {
	file_descriptor = open ( path, O_RDONLY );
    }

    else
    {
	file_descriptor = open ( path, O_WRONLY | O_CREAT | O_EXCL, S_IRGRP | S_IRUSR | S_IWUSR );
    }

    if ( file_descriptor == -1 )
    {
	return ( F_TRUE );
    }

    *fd = file_descriptor;

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKCHA ( IOP, FD, STRING )
 *      -------------------------------------------
 *
 *      PURPOSE:
 *
 *      Reads/writes character variable STRING from/to file with file
 *      descriptor FD according to IOP.
 *
 *      PARAMETERS:
 *
 *      INTEGER IOP          : INPUT - whether file is to be read from
 *                                     or written to.
 *                             IOP = 1: read file.
 *                             IOP = 2: write file.
 *
 *      INTEGER FD           : INPUT - file descriptor.
 *
 *      CHARACTER*(*) STRING : INPUT/OUTPUT - character variable to be
 *                                            read to/written from.
 *
 *      FUNCTION VALUE:
 *
 *      Function returns .FALSE. if read/write succeeded else returns .TRUE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eikcha_ ( iop, fd, string, string_length )
/*===========================================================*/

F_integer       *iop;
F_integer       *fd;
F_string         string;
F_string_length  string_length;
{
    int   opcode;
    int   file_descriptor;
    char *byte_address;
    int   byte_count;

    opcode          = *iop;
    file_descriptor = *fd;
    byte_address    = (char *) string;
    byte_count      = (int) string_length;

    if ( opcode == 1 )
    {
	if ( read ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    else
    {
	if ( write ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKINT ( IOP, FD, DATA )
 *      -----------------------------------------
 *
 *      PURPOSE:
 *
 *      Reads/writes integer variable DATA from/to file with file
 *      descriptor FD according to IOP.
 *
 *      PARAMETERS:
 *
 *      INTEGER IOP  : INPUT - whether file is to be read from or written
 *                             to.
 *                     IOP = 1: read file.
 *                     IOP = 2: write file.
 *
 *      INTEGER FD   : INPUT - file descriptor.
 *
 *      INTEGER DATA : INPUT/OUTPUT - integer variable to be read to/
 *                                    written from.
 *
 *      FUNCTION VALUE:
 *
 *      Function returns .FALSE. if read/write succeeded else returns .TRUE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eikint_ ( iop, fd, data )
/*=======================================*/

F_integer *iop;
F_integer *fd;
F_integer *data;
{
    int   opcode;
    int   file_descriptor;
    char *byte_address;
    int   byte_count;

    opcode          = *iop;
    file_descriptor = *fd;
    byte_address    = (char *) data;
    byte_count      = sizeof ( F_integer );

    if ( opcode == 1 )
    {
	if ( read ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    else
    {
	if ( write ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKLIN ( IOP, FD, DATA )
 *      -----------------------------------------
 *
 *      PURPOSE:
 *
 *      Reads/writes long integer variable DATA from/to file with file
 *      descriptor FD according to IOP.
 *
 *      PARAMETERS:
 *
 *      INTEGER IOP    : INPUT - whether file is to be read from or written
 *                               to.
 *                       IOP = 1: read file.
 *                       IOP = 2: write file.
 *
 *      INTEGER FD     : INPUT - file descriptor.
 *
 *      INTEGER*4 DATA : INPUT/OUTPUT - integer variable to be read to/
 *                                      written from.
 *
 *      FUNCTION VALUE:
 *
 *      Function returns .FALSE. if read/write succeeded else returns .TRUE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eiklin_ ( iop, fd, data )
/*=======================================*/

F_integer     *iop;
F_integer     *fd;
F_longinteger *data;
{
    int   opcode;
    int   file_descriptor;
    char *byte_address;
    int   byte_count;

    opcode          = *iop;
    file_descriptor = *fd;
    byte_address    = (char *) data;
    byte_count      = sizeof ( F_longinteger );

    if ( opcode == 1 )
    {
	if ( read ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    else
    {
	if ( write ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKBYA ( IOP, FD, DATA, COUNT )
 *      ------------------------------------------------
 *
 *      PURPOSE:
 *
 *      Reads/writes byte array DATA of COUNT bytes from/to file with file
 *      descriptor FD according to IOP.
 *
 *      PARAMETERS:
 *
 *      INTEGER IOP       : INPUT - whether file is to be read from or written
 *                                  to.
 *                          IOP = 1: read file.
 *                          IOP = 2: write file.
 *
 *      INTEGER FD        : INPUT - file descriptor.
 *
 *      INTEGER*1 DATA(*) : INPUT/OUTPUT - byte array to be read to/
 *                                         written from.
 *
 *      INTEGER COUNT     : INPUT - number of bytes to be read/written starting
 *                                  at beginning of array.
 *
 *      FUNCTION VALUE:
 *
 *      Function returns .FALSE. if read/write succeeded else returns .TRUE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eikbya_ ( iop, fd, data, count )
/*==============================================*/

F_integer    *iop;
F_integer    *fd;
F_byte_array  data;
F_integer    *count;
{
    int   opcode;
    int   file_descriptor;
    char *byte_address;
    int   byte_count;

    opcode          = *iop;
    file_descriptor = *fd;
    byte_address    = (char *) data;
    byte_count      = (int) *count;

    if ( opcode == 1 )
    {
	if ( read ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    else
    {
	if ( write ( file_descriptor, byte_address, byte_count ) != byte_count )
	{
	    return ( F_TRUE );
	}
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      LOGICAL FUNCTION EIKCLO ( FD )
 *      ------------------------------
 *
 *
 *      PURPOSE:
 *
 *      Closes file with file descriptor FD.
 *
 *
 *      PARAMETERS:
 *
 *      INTEGER FD     : INPUT - file descriptor.
 *
 *
 *      FUNCTION VALUE:
 *
 *      Function returns .FALSE. if close succeeded else returns .TRUE.
 *
 *---------------------------------------------------------------
 */

logicalfunction eikclo_ ( fd )
/*============================*/

F_integer *fd;
{
    int file_descriptor;

    file_descriptor = *fd;

    if ( close ( file_descriptor ) == -1 )
    {
	return ( F_TRUE );
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------
 *
 *      SUBROUTINE IROWSW ( DATA_IN, DATA_OUT, COUNT )
 *      ----------------------------------------------
 *
 *
 *      PURPOSE:
 *
 *      Swaps byte ordering of Fortran INTEGER array
 *
 *
 *      PARAMETERS:
 *
 *      INTEGER DATA_IN(*)   : INPUT  - integer array
 *      INTEGER DATA_OUT(*)  : OUTPUT - integer array
 *
 *      INTEGER COUNT        : INPUT - length of DATA_IN and DATA_OUT
 *
 *---------------------------------------------------------------
 */

subroutine irowsw_ ( data_in, data_out, count )
/*=============================================*/

F_integer_array  data_in;
F_integer_array  data_out;
F_integer       *count;
{
    F_byte_array src;
    F_byte_array dst;
    int          n;
    int          i;
    F_byte       byte;

    src = (F_byte_array) data_in;
    dst = (F_byte_array) data_out;
    n   = *count - 1;

    for ( i = n; i > 0; i-- )
    {
	byte   = *src++;
	*dst++ = *src++;
	*dst++ = byte;
    }

    byte   = *src++;
    *dst++ = *src;
    *dst   = byte;
}

/*
 *----------------------------------------------------------------
 *
 *      SUBROUTINE FROWSW ( DATA_IN, DATA_OUT, COUNT )
 *      ----------------------------------------------
 *
 *
 *      PURPOSE:
 *
 *      Swaps byte ordering of Fortran REAL or INTEGER*4 array
 *
 *
 *      PARAMETERS:
 *
 *      INTEGER*4 DATA_IN   : INPUT  - array
 *      INTEGER*4 DATA_OUT  : OUTPUT - array
 *
 *      INTEGER COUNT       : INPUT - length of DATA_IN and DATA_OUT
 *
 *---------------------------------------------------------------
 */

subroutine frowsw_ ( data_in, data_out, count )
/*=============================================*/

F_longinteger_array  data_in;
F_longinteger_array  data_out;
F_integer           *count;
{
    F_byte_array src;
    F_byte_array dst;
    int          n;
    int          i;
    F_byte       byte2;
    F_byte       byte3;
    F_byte       byte4;

    src = (F_byte_array) data_in;
    dst = (F_byte_array) data_out;
    n   = *count - 1;

    for ( i = n; i > 0; i-- )
    {
	byte4  = *src++;
	byte3  = *src++;
	byte2  = *src++;
	*dst++ = *src++;
	*dst++ = byte2;
	*dst++ = byte3;
	*dst++ = byte4;
    }

    byte4  = *src++;
    byte3  = *src++;
    byte2  = *src++;
    *dst++ = *src;
    *dst++ = byte2;
    *dst++ = byte3;
    *dst   = byte4;
}
/* Buffer calls to catch '_' */

logicalfunction eikexi ( file_name, exists, file_name_length )
/*================================================================*/

F_string        file_name;
F_logical       *exists;
F_string_length  file_name_length;
{
        return eikexi_ ( file_name, exists, file_name_length );
}
logicalfunction eikdel ( file_name, file_name_length )
/*========================================================*/

F_string         file_name;
F_string_length  file_name_length;
{
        return eikdel_ ( file_name, file_name_length );
}
logicalfunction eikope ( iop, fd, file_name, file_name_length )
/*=================================================================*/

F_integer       *iop;
F_integer       *fd;
F_string         file_name;
F_string_length  file_name_length;
{
        return eikope_ ( iop, fd, file_name, file_name_length );
}

logicalfunction eikcha ( iop, fd, string, string_length )
/*===========================================================*/

F_integer       *iop;
F_integer       *fd;
F_string         string;
F_string_length  string_length;
{
        return eikcha_ ( iop, fd, string, string_length ) ;
}
logicalfunction eikint ( iop, fd, data )
/*=======================================*/

F_integer *iop;
F_integer *fd;
F_integer *data;
{
        return eikint_ ( iop, fd, data );
}
logicalfunction eiklin ( iop, fd, data )
/*=======================================*/

F_integer     *iop;
F_integer     *fd;
F_longinteger *data;
{
        return eiklin_ ( iop, fd, data );
}
logicalfunction eikbya ( iop, fd, data, count )
/*==============================================*/

F_integer    *iop;
F_integer    *fd;
F_byte_array  data;
F_integer    *count;
{
        return eikbya_ ( iop, fd, data, count );
}
logicalfunction eikclo ( fd )
/*============================*/

F_integer *fd;
{
        return eikclo_ ( fd );
}
subroutine irowsw ( data_in, data_out, count )
/*=============================================*/

F_integer_array  data_in;
F_integer_array  data_out;
F_integer       *count;
{
        irowsw_ ( data_in, data_out, count );
}
subroutine frowsw ( data_in, data_out, count )
/*=============================================*/

F_longinteger_array  data_in;
F_longinteger_array  data_out;
F_integer           *count;
{
        frowsw_ ( data_in, data_out, count );
}

