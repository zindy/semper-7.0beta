/*
    Disc cache access routines - lowest level

    All routines in this module are
    Copyright (C) 1987-1993 : Synoptics Ltd  All Rights Reserved

    Modify: Mark Raisbeck       27/May/1992 - Named scratch files
    Modify: Frank Suess          1/Sep/1992 - M/S Windows support
    Modify: Frank Suess          1/Nov/1992 - Memory device support
    Modify: Mark Raisbeck        8/Jun/1993 - Remove Sparc reference
    Modify: Zoe Harding         14/Oct/1993 - io buffer allocation
    Modify: Mark Raisbeck       17/Mar/1994 - Return count from create_cache
    Modify: Frank Suess         19/May/1995 - Changes for Solaris 2.4
*/

/* MONITOR activates debug p/o verifying fn calls */
/* #define MONITOR */

/* #define CACHE_MMAP */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/mman.h>

#include "ftypes.h"
#include "params.h"
#include "dcache.h"

/* Patch since malloc.h is being obsoleted */
#if defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif


#ifndef NULL
#define NULL 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef BUFSIZ
#define BUFSIZ 512
#endif

/*  Device table  */

static StatusMask     device_status_mask    [ MAXDEV + 1 ];
static DeviceSize     device_storage_size   [ MAXDEV + 1 ];
static BufferAddress  device_buffer_address [ MAXDEV + 1 ];
static FileHandle     device_file_handle    [ MAXDEV + 1 ];
static FileName       device_file_name      [ MAXDEV + 1 ];
static SegmentIndex   device_file_segments  [ MAXDEV + 1 ];
static SegmentTable   device_cache_index    [ MAXDEV + 1 ];

/*  Cache table  */

static CacheIndex      cache_segments;
static SegmentSize     cache_segment_size;
static CacheIndex      cache_free_segments;
static CacheIndex      cache_used_segments;
static CacheIndex      cache_free_index;
static CacheIndex      cache_oldest_index;

static SegmentAddress *cache_segment_address;
static StatusMask     *cache_status_mask;
static DeviceNumber   *cache_device_number;
static SegmentIndex   *cache_segment_index;
static CacheIndex     *cache_old_index;
static CacheIndex     *cache_new_index;

/*  I/O buffer - for non-cached I/O  */

static BufferAddress io_buffer;
static DeviceSize    io_buffer_size;

/*  Byte sizes for Fortran data forms  */

int form_to_bytes ( data_form )
/*===========================*/

int data_form;
{
    switch ( data_form )
    {
	case NFMBYT:  return ( 1 );
	case NFMINT:  return ( LNINT );
	case NFMFP :  return ( LNREAL );
	case NFMCOM:  return ( LNCOMP );
	default:      return ( 0 );
    }
}

/*  String copy routine  */

String duplicate_string ( string )
/*==============================*/

String string;
{
    int    i;
    int    n;
    String new_string;

#ifdef MONITOR
    printf ( "duplicate_string: s = %s\n",
	     string );
#endif

    for ( n = 0; string [ n ]; n++ );

    new_string = (String) allocate_memory ( n + 1 );

    if ( new_string != (String) NULL )
    {
	for ( i = 0; i < n; i++ )
	{
	    new_string [ i ] = string [ i ];
	}

	new_string [ n ] = '\0';
    }

    return ( new_string );
}

/*  Memory handling routines  */

MemoryAddress allocate_memory ( data_size )
/*=======================================*/

MemorySize data_size;
{
    MemoryAddress  data_address;

#ifdef MONITOR
    printf ( "allocate_memory: d_s = %d\n",
	     data_size );
#endif

    data_address = (MemoryAddress) NULL;

    if ( data_size > 0 )
    {
	data_address = (MemoryAddress) malloc ( data_size );
    }

    return ( data_address );
}

void free_memory ( data_address )
/*=============================*/

MemoryAddress data_address;
{
#ifdef MONITOR
    printf ( "free_memory\n" );
#endif

    if ( data_address != (MemoryAddress) NULL )
    {
	free ( data_address );
    }
}

MemoryAddress resize_memory ( data_address, data_size )
/*===================================================*/

MemoryAddress data_address;
MemorySize    data_size;
{
    MemoryAddress  new_data_address;

#ifdef MONITOR
    printf ( "resize_memory: d_s = %d\n",
	     data_size );
#endif

    new_data_address = (MemoryAddress) NULL;

    if ( data_address != (MemoryAddress) NULL && data_size > 0 )
    {
	new_data_address = (MemoryAddress) realloc ( data_address, data_size );
    }

    return ( new_data_address );
}

void copy_memory ( source_buffer, destination_buffer, data_count )
/*==============================================================*/

BufferAddress source_buffer;
BufferAddress destination_buffer;
DeviceSize    data_count;
{
    if ( data_count > 0 )
    {
	memcpy ( destination_buffer, source_buffer, data_count );
    }
}

void zero_memory ( data_buffer, data_count )
/*========================================*/

BufferAddress data_buffer;
DeviceSize    data_count;
{
    if ( data_count > 0 )
    {
	memset ( data_buffer, 0, data_count );
    }
}

/*  File handling routines  */

BooleanFlag open_file ( file_name, write_access, file_handle, file_size )
/*=====================================================================*/

FileName     file_name;
BooleanFlag  write_access;
FileHandle  *file_handle;
DeviceSize  *file_size;
{
    FileHandle handle;
    DeviceSize size;

#ifdef MONITOR
    printf ( "open_file: f_n, w_a = %s, %d\n",
	     file_name, write_access );
#endif

    if ( write_access )
    {
	if ( access ( file_name, F_OK | R_OK | W_OK ) == -1 ) return ( TRUE );

	handle = open ( file_name, O_RDWR );
    }

    else
    {
	if ( access ( file_name, F_OK | R_OK ) == -1 ) return ( TRUE );

	handle = open ( file_name, O_RDONLY );
    }

    if ( handle == -1 ) return ( TRUE );

    size = lseek ( handle, 0, L_XTND );

    if ( size == -1 )
    {
	close ( handle );

	return ( TRUE );
    }

    *file_handle = handle;
    *file_size   = size;

    return ( FALSE );
}

BooleanFlag create_file ( file_name, scratch_file, file_handle )
/*============================================================*/

FileName     file_name;
BooleanFlag  scratch_file;
FileHandle  *file_handle;
{
    FileHandle handle;

#ifdef MONITOR
    printf ( "create_file: f_n = %s\n",
	     file_name );
#endif

    if ( access ( file_name, F_OK ) != -1 )
    {
	if ( unlink ( file_name ) == -1 ) return ( TRUE );
    }

    handle = creat ( file_name, 0644 );

    if ( handle == -1 ) return ( TRUE );

    if ( close ( handle ) == -1 ) return ( TRUE );

    handle = open ( file_name, O_RDWR );

    if ( handle == -1 ) return ( TRUE );

    if ( scratch_file )
    {
	if ( unlink ( file_name ) == -1 )
	{
	    close ( handle );

	    return ( TRUE );
	}
    }

    *file_handle = handle;

    return ( FALSE );
}

BooleanFlag resize_file ( file_handle, new_file_size )
/*==================================================*/

FileHandle file_handle;
DeviceSize new_file_size;
{
    DeviceSize    old_file_size;

#ifdef MONITOR
    printf ( "resize_file: f_h, f_s = %d, %d\n",
	     file_handle, new_file_size );
#endif

    if ( new_file_size < 0 ) return ( TRUE );

    old_file_size = lseek ( file_handle, 0, L_XTND );

    if ( old_file_size == -1 ) return ( TRUE );

    if ( new_file_size < old_file_size )
    {
	if ( ftruncate ( file_handle, new_file_size ) == -1 ) return ( TRUE );
    }

    else if ( new_file_size > old_file_size )
    {
	if ( zero_file ( file_handle, old_file_size, new_file_size - old_file_size ) )
	{
	    ftruncate ( file_handle, old_file_size );

	    return ( TRUE );
	}
    }

    return ( FALSE );
}

BooleanFlag read_file ( file_handle, file_offset, data_buffer, data_count )
/*=======================================================================*/

FileHandle    file_handle;
DeviceSize    file_offset;
BufferAddress data_buffer;
DeviceSize    data_count;
{
#ifdef MONITOR
    printf ( "read_file: f_h, f_o, d_c = %d, %d, %d\n",
	     file_handle, file_offset, data_count );
#endif

    if ( data_count > 0 )
    {
	if ( lseek ( file_handle, file_offset, L_SET ) == -1 ) return ( TRUE );

	if ( read ( file_handle, data_buffer, data_count ) != data_count ) return ( TRUE );
    }

    return ( FALSE );
}

BooleanFlag write_file ( file_handle, file_offset, data_buffer, data_count )
/*========================================================================*/

FileHandle    file_handle;
DeviceSize    file_offset;
BufferAddress data_buffer;
DeviceSize    data_count;
{
#ifdef MONITOR
    printf ( "write_file: f_h, f_o, d_c = %d, %d, %d\n",
	     file_handle, file_offset, data_count );
#endif

    if ( data_count > 0 )
    {
	if ( lseek ( file_handle, file_offset, L_SET ) == -1 ) return ( TRUE );

	if ( write ( file_handle, data_buffer, data_count ) != data_count ) return ( TRUE );
    }

    return ( FALSE );
}

BooleanFlag zero_file ( file_handle, file_offset, data_count )
/*==========================================================*/

FileHandle    file_handle;
DeviceSize    file_offset;
DeviceSize    data_count;
{
    BufferAddress zero_buffer;
    DeviceSize    zero_size;
    DeviceSize    zero_count;

#ifdef MONITOR
    printf ( "zero_file: f_h, f_o, d_c = %d, %d, %d\n",
	     file_handle, file_offset, data_count );
#endif

    if ( data_count > 0 )
    {
	if ( lseek ( file_handle, file_offset, L_SET ) == -1 ) return ( TRUE );

	if ( io_buffer == (BufferAddress) NULL )
	{
	    io_buffer_size = BUFSIZ * ( 1 + ( 32768 - 1 ) / BUFSIZ );

	    io_buffer = allocate_memory ( io_buffer_size );

	    if (io_buffer == (BufferAddress) NULL )
	    {
		io_buffer_size = 0;

		return ( TRUE );
	    }
	}

	zero_buffer = io_buffer;

	zero_size = io_buffer_size;

	if ( zero_size > data_count ) zero_size = data_count;

	zero_memory ( zero_buffer, zero_size );

	zero_count = 0;

	while ( zero_count + zero_size < data_count )
	{
	    if ( write ( file_handle, zero_buffer, zero_size ) != zero_size ) return ( TRUE );

	    zero_count += zero_size;
	}

	zero_size = data_count - zero_count;

	if ( write ( file_handle, zero_buffer, zero_size ) != zero_size ) return ( TRUE );
    }

    return ( FALSE );
}

BooleanFlag shut_file ( file_handle )
/*=================================*/

FileHandle file_handle;
{
#ifdef MONITOR
    printf ( "shut_file: f_h = %d\n",
	     file_handle );
#endif

    if ( close ( file_handle ) == -1 ) return ( TRUE );

    return ( FALSE );
}

BooleanFlag delete_file ( file_name )
/*=================================*/

FileName file_name;
{
#ifdef MONITOR
    printf ( "delete_file: f_n = %s\n",
	     file_name );
#endif

    if ( unlink ( file_name ) == -1 ) return ( TRUE );

    return ( FALSE );
}

BooleanFlag create_cache ( number_of_segments, segment_size, number_found )
/*=======================================================================*/

CacheIndex    number_of_segments;
SegmentSize   segment_size;
CacheIndexPtr number_found;
{
    CacheIndex   max_number_of_segments;
    SegmentSize  max_segment_size;
    SegmentSize  segment_size_factor;
    SegmentSize  n;
    BooleanFlag  segment_allocation_error;
    CacheIndex   i, found_segments;
    DeviceNumber j;

#ifdef MONITOR
    printf ( "create_cache: n_o_s, s_s = %d, %d\n",
	     number_of_segments, segment_size );
#endif

    *number_found = 0;

    query_cache_limits ( &max_number_of_segments,
			 &max_segment_size,
			 &segment_size_factor );

    if ( number_of_segments < 1 || segment_size < 1 ) return ( TRUE );

    if ( max_number_of_segments > 0 )
    {
	if ( number_of_segments > max_number_of_segments ) return ( TRUE );
    }

    if ( max_segment_size > 0 )
    {
	if ( segment_size > max_segment_size ) return ( TRUE );
    }

    n = segment_size / segment_size_factor;

    if ( n * segment_size_factor != segment_size ) return ( TRUE );

    if ( free_cache ( ) ) return ( TRUE );

    segment_allocation_error = FALSE;
    found_segments = 0;

    cache_segment_address = (SegmentAddress *) allocate_memory ( number_of_segments * sizeof (SegmentAddress) );
    cache_status_mask     = (StatusMask     *) allocate_memory ( number_of_segments * sizeof (StatusMask) );
    cache_device_number   = (DeviceNumber   *) allocate_memory ( number_of_segments * sizeof (DeviceNumber) );
    cache_segment_index   = (SegmentIndex   *) allocate_memory ( number_of_segments * sizeof (SegmentIndex) );
    cache_old_index       = (CacheIndex     *) allocate_memory ( number_of_segments * sizeof (CacheIndex) );
    cache_new_index       = (CacheIndex     *) allocate_memory ( number_of_segments * sizeof (CacheIndex) );

#ifdef CACHE_MALLOC
    if ( cache_segment_address != (SegmentAddress *) NULL )
    {
	for ( i=0; (i<number_of_segments) && (!segment_allocation_error); i++ )
	{
	    cache_segment_address [ i ] = (SegmentAddress) allocate_memory ( segment_size );

	    if ( cache_segment_address [ i ] == (SegmentAddress) NULL )
	    {
/*
		Don't look any further
*/
		segment_allocation_error = TRUE;
	    }
	    else
	    {
		found_segments++;
	    }
	}
    }
#endif

    *number_found = found_segments;

    if ( cache_segment_address == (SegmentAddress  *) NULL ||
	 cache_status_mask     == (StatusMask      *) NULL ||
	 cache_device_number   == (DeviceNumber    *) NULL ||
	 cache_segment_index   == (SegmentIndex    *) NULL ||
	 cache_old_index       == (CacheIndex      *) NULL ||
	 cache_new_index       == (CacheIndex      *) NULL ||
	 segment_allocation_error )
    {

#ifdef CACHE_MALLOC
	if ( segment_allocation_error )
	{
	    for ( i = 0; i < found_segments; i++ )
	    {
		free_memory ( cache_segment_address [ i ] );
	    }
	}
#endif

	if ( cache_segment_address != (SegmentAddress *) NULL )
	{
	    free_memory ( cache_segment_address );

	    cache_segment_address = (SegmentAddress *) NULL;
	}

	if ( cache_status_mask != (StatusMask *) NULL )
	{
	    free_memory ( cache_status_mask );

	    cache_status_mask = (StatusMask *) NULL;
	}

	if ( cache_device_number != (DeviceNumber *) NULL )
	{
	    free_memory ( cache_device_number );

	    cache_device_number = (DeviceNumber *) NULL;
	}

	if ( cache_segment_index != (SegmentIndex *) NULL )
	{
	    free_memory ( cache_segment_index );

	    cache_segment_index = (SegmentIndex *) NULL;
	}

	if ( cache_old_index != (CacheIndex *) NULL )
	{
	    free_memory ( cache_old_index );

	    cache_old_index = (CacheIndex *) NULL;
	}

	if ( cache_new_index != (CacheIndex *) NULL )
	{
	    free_memory ( cache_new_index );

	    cache_new_index = (CacheIndex *) NULL;
	}

	return ( TRUE );
    }

    else
    {
	cache_segments      = number_of_segments;
	cache_segment_size  = segment_size;
	cache_free_segments = number_of_segments;
	cache_used_segments = 0;
	cache_free_index    = 0;

	for ( i = 0; i < number_of_segments; i++ )
	{
	    cache_old_index [ i ] = i - 1;
	    cache_new_index [ i ] = i + 1;
	}

	cache_old_index [ 0 ] = number_of_segments - 1;
	cache_new_index [ number_of_segments - 1 ] = 0;

	for ( j = 0; j <= MAXDEV; j++ )
	{
	    if ( create_segment_table ( j ) ) return ( TRUE );
	}

	return ( FALSE );
    }
}

BooleanFlag free_cache ( void )

/*===========================*/

{
    DeviceNumber i;
    CacheIndex   j;

#ifdef MONITOR
    printf ( "free_cache\n" );
#endif

    if ( cache_segments > 0 )
    {
	for ( i = 0; i <= MAXDEV; i++ )
	{
	    if ( flush_segment_table ( i ) ) return ( TRUE );

	    if ( free_segment_table ( i ) ) return ( TRUE );
	}

	if ( cache_segment_address != (SegmentAddress *) NULL )
	{
#ifdef CACHE_MALLOC
	    for ( j = 0; j < cache_segments; j++ )
	    {
		free_memory ( cache_segment_address [ j ] );
	    }
#endif

	    free_memory ( cache_segment_address );

	    cache_segment_address = (SegmentAddress *) NULL;
	}

	if ( cache_status_mask != (StatusMask *) NULL )
	{
	    free_memory ( cache_status_mask );

	    cache_status_mask = (StatusMask *) NULL;
	}

	if ( cache_device_number != (DeviceNumber *) NULL )
	{
	    free_memory ( cache_device_number );

	    cache_device_number = (DeviceNumber *) NULL;
	}

	if ( cache_segment_index != (SegmentIndex *) NULL )
	{
	    free_memory ( cache_segment_index );

	    cache_segment_index = (SegmentIndex *) NULL;
	}

	if ( cache_old_index != (CacheIndex *) NULL )
	{
	    free_memory ( cache_old_index );

	    cache_old_index = (CacheIndex *) NULL;
	}

	if ( cache_new_index != (CacheIndex *) NULL )
	{
	    free_memory ( cache_new_index );

	    cache_new_index = (CacheIndex *) NULL;
	}

	cache_segments = 0;
	cache_segment_size = 0;
    }


    if ( io_buffer_size > 0 )
    {
	if ( io_buffer != (BufferAddress) NULL )
	{
	    free_memory ( io_buffer );

	    io_buffer = (BufferAddress) NULL;
	}

	io_buffer_size = 0;
    }


    return ( FALSE );
}

void query_cache_limits ( max_number_of_segments, max_segment_size,
			  segment_size_factor )
/*===============================================================*/

CacheIndex  *max_number_of_segments;
SegmentSize *max_segment_size;
SegmentSize *segment_size_factor;
{
    *max_number_of_segments = 0;
    *max_segment_size       = 0;
#ifdef CACHE_MALLOC
    *segment_size_factor    = 8;
#endif
#ifdef CACHE_MMAP
    *segment_size_factor    = getpagesize ( );
#endif
}

void query_cache ( number_of_segments, segment_size )
/*=================================================*/

CacheIndex  *number_of_segments;
SegmentSize *segment_size;
{
    *number_of_segments = cache_segments;
    *segment_size       = cache_segment_size;
}

BooleanFlag create_segment_table ( device_number )
/*==============================================*/

DeviceNumber device_number;
{
    StatusMask   status_mask;
    DeviceSize   file_size;
    SegmentIndex file_segments;
    SegmentTable segment_table;
    SegmentIndex i;

#ifdef MONITOR
    printf ( "create_segment_table: d_n = %d\n",
	     device_number );
#endif
    if ( free_segment_table ( device_number ) ) return ( TRUE );

    if ( cache_segments > 0 )
    {
	status_mask = device_status_mask [ device_number ];

	if ( ( status_mask & FILE_OPEN ) && ( ! ( status_mask & MEMORY_OPEN ) ) )
	{
	    file_size = device_storage_size [ device_number ];

	    file_segments = file_size / cache_segment_size;

	    if ( file_segments * cache_segment_size < file_size )
	    {
		file_segments += 1;
	    }

	    if ( file_segments > 0 )
	    {
		segment_table = (SegmentTable) allocate_memory ( file_segments * sizeof (CacheIndex) );

		if ( segment_table == (SegmentTable) NULL ) return ( TRUE );

		device_cache_index [ device_number ] = segment_table;
	    }

	    for ( i = 0; i < file_segments; i++ )
	    {
		segment_table [ i ] = -1;
	    }

	    device_file_segments [ device_number ] = file_segments;
	}
    }

    return ( FALSE );
}

BooleanFlag free_segment_table ( device_number )
/*============================================*/

DeviceNumber device_number;
{
    StatusMask   status_mask;
    SegmentIndex file_segments;
    SegmentIndex i;
    SegmentTable segment_table;

#ifdef MONITOR
    printf ( "free_segment_table: d_n = %d\n",
	     device_number );
#endif

    if ( cache_segments > 0 )
    {
	status_mask = device_status_mask [ device_number ];

	if ( status_mask & FILE_OPEN )
	{
	    file_segments = device_file_segments [ device_number ];

	    if ( file_segments > 0 )
	    {
		for ( i = 0; i < file_segments; i++ )
		{
		    if ( free_segment ( device_number, i ) ) return ( TRUE );
		}

		segment_table = device_cache_index [ device_number ];

		free_memory ( segment_table );

		device_file_segments [ device_number ] = 0;
	    }
	}
    }

    return ( FALSE );
}

BooleanFlag flush_segment_table ( device_number )
/*=============================================*/

DeviceNumber device_number;
{
    StatusMask   status_mask;
    SegmentIndex file_segments;
    CacheIndex   i;

#ifdef MONITOR
    printf ( "flush_segment_table: d_n = %d\n",
	     device_number );
#endif

    if ( cache_segments > 0 )
    {
	status_mask = device_status_mask [ device_number ];

	if ( status_mask & FILE_OPEN )
	{
	    file_segments = device_file_segments [ device_number ];

	    for ( i = 0; i < file_segments; i++ )
	    {
		if ( flush_segment ( device_number, i ) ) return ( TRUE );
	    }
	}
    }

    return ( FALSE );
}

BooleanFlag allocate_segment ( device_number, segment_index )
/*=========================================================*/

DeviceNumber device_number;
SegmentIndex segment_index;
{
    CacheIndex     cache_index;
#ifdef CACHE_MMAP
    FileHandle     file_handle;
    DeviceSize     file_offset;
    DeviceSize     file_size;
    SegmentSize    segment_size;
    StatusMask     status_mask;
#endif
    SegmentAddress segment_address;
    DeviceNumber   old_device_number;
    SegmentIndex   old_segment_index;
    BooleanFlag    empty_segment;
    CacheIndex     old_index;
    CacheIndex     new_index;

#ifdef MONITOR
    printf ( "allocate_segment: d_n, s_i = %d, %d\n",
		device_number, segment_index );
#endif

    cache_index = device_cache_index [ device_number ] [ segment_index ];

    if ( cache_index < 0 || cache_index >= cache_segments )
    {
#ifdef CACHE_MMAP
	file_handle = device_file_handle [ device_number ];

	file_offset = segment_index * cache_segment_size;

	file_size = device_storage_size [ device_number ];

	if ( file_size < file_offset + cache_segment_size )
	{
	    segment_size = file_size - file_offset;
	}

	else
	{
	    segment_size = cache_segment_size;
	}

	status_mask = device_status_mask [ device_number ];

	if ( status_mask & WRITE_ACCESS )
	{
	    segment_address = mmap ( (char *) NULL, segment_size,
				     PROT_READ | PROT_WRITE, MAP_SHARED,
				     file_handle, file_offset );
	}

	else
	{
	    segment_address = mmap ( (char *) NULL, segment_size,
				     PROT_READ, MAP_SHARED,
				     file_handle, file_offset );
	}

	if ( segment_address == (SegmentAddress) -1 ) return ( TRUE );

#endif
	if ( cache_free_segments == 0 )
	{
	    old_device_number = cache_device_number [ cache_oldest_index ];
	    old_segment_index = cache_segment_index [ cache_oldest_index ];

	    if ( flush_segment ( old_device_number, old_segment_index ) ) return ( TRUE );

	    if ( free_segment ( old_device_number, old_segment_index ) ) return ( TRUE );
	}

	empty_segment = ( cache_index == -2 );

	cache_index = cache_free_index;

	cache_device_number [ cache_index ] = device_number;
	cache_segment_index [ cache_index ] = segment_index;

#ifdef CACHE_MMAP
	cache_segment_address [ cache_index ] = segment_address;
#else
	segment_address = cache_segment_address [ cache_index ];
#endif

	if ( empty_segment )
	{
	    zero_memory ( segment_address, cache_segment_size );

	    cache_status_mask [ cache_index ] |= SEGMENT_LOADED;
	    cache_status_mask [ cache_index ] &= ~SEGMENT_SYNCED;
	}

	else
	{
	    cache_status_mask [ cache_index ] &= ~SEGMENT_LOADED;
	}

	new_index = cache_new_index [ cache_index ];
	old_index = cache_old_index [ cache_index ];

	cache_new_index [ old_index ] = new_index;
	cache_old_index [ new_index ] = old_index;

	cache_free_index = new_index;

	if ( cache_used_segments == 0 )
	{
	    cache_old_index [ cache_index ] = cache_index;
	    cache_new_index [ cache_index ] = cache_index;

	    cache_oldest_index = cache_index;
	}

	else
	{
	    new_index = cache_oldest_index;
	    old_index = cache_old_index [ new_index ];

	    cache_new_index [ cache_index ] = new_index;
	    cache_old_index [ cache_index ] = old_index;

	    cache_new_index [ old_index ] = cache_index;
	    cache_old_index [ new_index ] = cache_index;
	}

	cache_free_segments = cache_free_segments - 1;
	cache_used_segments = cache_used_segments + 1;

	device_cache_index [ device_number ] [ segment_index ] = cache_index;
    }

    else
    {
	new_index = cache_new_index [ cache_index ];

	if ( new_index != cache_oldest_index )
	{
	    old_index = cache_old_index [ cache_index ];

	    cache_new_index [ old_index ] = new_index;
	    cache_old_index [ new_index ] = old_index;

	    if ( cache_oldest_index == cache_index ) cache_oldest_index = new_index;

	    new_index = cache_oldest_index;
	    old_index = cache_old_index [ new_index ];

	    cache_new_index [ cache_index ] = new_index;
	    cache_old_index [ cache_index ] = old_index;

	    cache_new_index [ old_index ] = cache_index;
	    cache_old_index [ new_index ] = cache_index;
	}
    }

    return ( FALSE );
}

BooleanFlag free_segment ( device_number, segment_index )
/*=====================================================*/

DeviceNumber device_number;
SegmentIndex segment_index;
{
    CacheIndex     cache_index;
#ifdef CACHE_MMAP
    DeviceSize     file_offset;
    DeviceSize     file_size;
    SegmentSize    segment_size;
    SegmentAddress segment_address;
#endif
    CacheIndex     old_index;
    CacheIndex     new_index;

#ifdef MONITOR
    printf ( "free_segment: d_n, s_i = %d, %d\n",
	     device_number, segment_index );
#endif

    cache_index = device_cache_index [ device_number ] [ segment_index ];

    if ( ! ( cache_index < 0 || cache_index >= cache_segments ) )
    {
#ifdef CACHE_MMAP
	segment_address = cache_segment_address [ cache_index ];

	file_offset = segment_index * cache_segment_size;

	file_size = device_storage_size [ device_number ];

	if ( file_size < file_offset + cache_segment_size )
	{
	    segment_size = file_size - file_offset;
	}

	else
	{
	    segment_size = cache_segment_size;
	}

	if ( munmap ( segment_address, segment_size ) != 0 ) return ( TRUE );

#endif
	new_index = cache_new_index[ cache_index ];
	old_index = cache_old_index[ cache_index ];

	cache_new_index [ old_index ] = new_index;
	cache_old_index [ new_index ] = old_index;

	if ( cache_oldest_index == cache_index ) cache_oldest_index = new_index;

	if ( cache_free_segments == 0 )
	{
	    cache_old_index[ cache_index ] = cache_index;
	    cache_new_index[ cache_index ] = cache_index;

	    cache_free_index = cache_index;
	}

	else
	{
	    new_index = cache_free_index;
	    old_index = cache_old_index [ new_index ];

	    cache_new_index [ cache_index ] = new_index;
	    cache_old_index [ cache_index ] = old_index;

	    cache_new_index [ old_index ] = cache_index;
	    cache_old_index [ new_index ] = cache_index;
	}

	cache_free_segments = cache_free_segments + 1;
	cache_used_segments = cache_used_segments - 1;

	device_cache_index [ device_number ] [ segment_index ] = -1;
    }

    return ( FALSE );
}

BooleanFlag load_segment ( device_number, segment_index )
/*=====================================================*/

DeviceNumber device_number;
SegmentIndex segment_index;
{
    CacheIndex     cache_index;
    StatusMask     status_mask;
#ifdef CACHE_MALLOC
    FileHandle     file_handle;
    DeviceSize     file_offset;
    DeviceSize     file_size;
    SegmentSize    segment_size;
    SegmentAddress segment_address;
#endif

#ifdef MONITOR
    printf ( "load_segment: d_n, s_i = %d, %d\n",
	     device_number, segment_index );
#endif

    cache_index = device_cache_index [ device_number ] [ segment_index ];

    if ( ! ( cache_index < 0 || cache_index >= cache_segments ) )
    {
	status_mask = cache_status_mask [ cache_index ];

	if ( ! ( status_mask & SEGMENT_LOADED ) )
	{
#ifdef CACHE_MALLOC
	    file_handle = device_file_handle [ device_number ];

	    file_offset = segment_index * cache_segment_size;

	    segment_address = cache_segment_address [ cache_index ];

	    file_size = device_storage_size [ device_number ];

	    if ( file_size < file_offset + cache_segment_size )
	    {
		segment_size = file_size - file_offset;
	    }

	    else
	    {
		segment_size = cache_segment_size;
	    }

	    if ( read_file ( file_handle, file_offset, segment_address, segment_size ) )
	    {
		return ( TRUE );
	    }

#endif
	    cache_status_mask [ cache_index ] |= SEGMENT_LOADED;
	    cache_status_mask [ cache_index ] |= SEGMENT_SYNCED;
	}
    }

    return ( FALSE );
}

BooleanFlag flush_segment ( device_number, segment_index )
/*======================================================*/

DeviceNumber device_number;
SegmentIndex segment_index;
{
    CacheIndex     cache_index;
    StatusMask     status_mask;
    DeviceSize     file_offset;
    DeviceSize     file_size;
    SegmentSize    segment_size;
    SegmentAddress segment_address;
#ifdef CACHE_MALLOC
    FileHandle     file_handle;
#endif

#ifdef MONITOR
    printf ( "flush_segment: d_n, s_i = %d, %d\n",
	     device_number, segment_index );
#endif

    cache_index = device_cache_index [ device_number ] [ segment_index ];

    if ( ! ( cache_index < 0 || cache_index >= cache_segments ) )
    {
	status_mask = cache_status_mask [ cache_index ];

	if ( ! ( status_mask & SEGMENT_SYNCED ) )
	{
	    segment_address = cache_segment_address [ cache_index ];

	    file_offset = segment_index * cache_segment_size;

	    file_size = device_storage_size [ device_number ];

	    if ( file_size < file_offset + cache_segment_size )
	    {
		segment_size = file_size - file_offset;
	    }

	    else
	    {
		segment_size = cache_segment_size;
	    }

#ifdef CACHE_MMAP
	    if ( msync ( segment_address, segment_size, MS_ASYNC ) != 0 ) return ( TRUE );
#endif
#ifdef CACHE_MALLOC
	    file_handle = device_file_handle [ device_number ];

	    if ( write_file ( file_handle, file_offset, segment_address, segment_size ) )
	    {
		return ( TRUE );
	    }
#endif

	    cache_status_mask [ cache_index ] |= SEGMENT_SYNCED;
	}
    }

    return ( FALSE );
}

void query_segment ( device_number, segment_index, segment_address, status_mask )
/*=============================================================================*/

DeviceNumber    device_number;
SegmentIndex    segment_index;
SegmentAddress *segment_address;
StatusMask     *status_mask;
{
    CacheIndex cache_index;

    *segment_address = (SegmentAddress) NULL;
    *status_mask     = 0;

    if ( ! ( device_number < 0 || device_number > MAXDEV ) )
    {
	if ( device_status_mask [ device_number ] & FILE_OPEN )
	{
	    if ( device_file_segments [ device_number ] > 0 )
	    {
		if ( ! ( segment_index < 0 || segment_index >= device_file_segments [ device_number ] ) )
		{
		    cache_index = device_cache_index [ device_number ] [ segment_index ];

		    if ( ! ( cache_index < 0 || cache_index >= cache_segments ) )
		    {
			*segment_address = cache_segment_address [ cache_index ];
			*status_mask     = cache_status_mask     [ cache_index ];
		    }
		}
	    }
	}
    }
}

BooleanFlag read_segment ( device_number, segment_index, segment_offset,
			   data_buffer, data_count )
/*====================================================================*/

DeviceNumber   device_number;
SegmentIndex   segment_index;
SegmentSize    segment_offset;
BufferAddress  data_buffer;
SegmentSize    data_count;
{
    CacheIndex     cache_index;
    SegmentAddress segment_address;

#ifdef MONITOR
    printf ( "read_segment: d_n, s_i, s_o, d_c = %d, %d, %d, %d\n",
	     device_number, segment_index, segment_offset, data_count );
#endif

    if ( data_count > 0 )
    {
	if ( allocate_segment ( device_number, segment_index ) ) return ( TRUE );

	if ( load_segment ( device_number, segment_index ) ) return ( TRUE );

	cache_index = device_cache_index [ device_number ] [ segment_index ];

	segment_address = cache_segment_address [ cache_index ];

	copy_memory ( segment_address + segment_offset, data_buffer, data_count );
    }

    return ( FALSE );
}

BooleanFlag write_segment ( device_number, segment_index, segment_offset,
			    data_buffer, data_count )
/*=====================================================================*/

DeviceNumber   device_number;
SegmentIndex   segment_index;
SegmentSize    segment_offset;
BufferAddress  data_buffer;
SegmentSize    data_count;
{
    CacheIndex     cache_index;
    SegmentAddress segment_address;

#ifdef MONITOR
    printf ( "write_segment: d_n, s_i, s_o, d_c = %d, %d, %d, %d\n",
	     device_number, segment_index, segment_offset, data_count );
#endif

    if ( data_count > 0 )
    {
	if ( allocate_segment ( device_number, segment_index ) ) return ( TRUE );

	cache_index = device_cache_index [ device_number ] [ segment_index ];

	if ( data_count != cache_segment_size )
	{
	    if ( load_segment ( device_number, segment_index ) ) return ( TRUE );
	}

	else
	{
	    cache_status_mask [ cache_index ] |= SEGMENT_LOADED;
	}

	segment_address = cache_segment_address [ cache_index ];

	copy_memory ( data_buffer, segment_address + segment_offset, data_count );

	cache_status_mask [ cache_index ] &= ~SEGMENT_SYNCED;
    }

    return ( FALSE );
}

BooleanFlag zero_segment ( device_number, segment_index, segment_offset, data_count )
/*=================================================================================*/

DeviceNumber   device_number;
SegmentIndex   segment_index;
SegmentSize    segment_offset;
SegmentSize    data_count;
{
    CacheIndex     cache_index;
    SegmentAddress segment_address;

#ifdef MONITOR
    printf ( "zero_segment: d_n, s_i, s_o, d_c = %d, %d, %d, %d\n",
	     device_number, segment_index, segment_offset, data_count );
#endif

    if ( data_count > 0 )
    {
	if ( data_count == cache_segment_size )
	{
	    if ( free_segment ( device_number, segment_index ) ) return ( TRUE );

	    device_cache_index [ device_number ] [ segment_index ] = -2;
	}

	else
	{
	    if ( allocate_segment ( device_number, segment_index ) ) return ( TRUE );

	    if ( load_segment ( device_number, segment_index ) ) return ( TRUE );

	    cache_index = device_cache_index [ device_number ] [ segment_index ];

	    segment_address = cache_segment_address [ cache_index ];

	    zero_memory ( segment_address + segment_offset, data_count );

	    cache_status_mask [ cache_index ] &= ~SEGMENT_SYNCED;
	}
    }

    return ( FALSE );
}

BooleanFlag read_segment_data ( device_number, segment_index, segment_offset,
				data_buffer, MEMF, DISCF, NITEMS )
/*=========================================================================*/

DeviceNumber     device_number;
SegmentIndex     segment_index;
SegmentSize      segment_offset;
BufferAddress    data_buffer;
F_integer       *MEMF;
F_integer       *DISCF;
F_longinteger   *NITEMS;
{
    CacheIndex     cache_index;
    SegmentAddress segment_address;
#ifdef MONITOR
    int            memory_data_form;
    int            disc_data_form;
    long int       data_count;

    memory_data_form = *MEMF;
    disc_data_form   = *DISCF;
    data_count       = *NITEMS;

    printf ( "read_segment_data: d_n, s_i, s_o, m_d_f, d_d_f, d_c = %d, %d, %d, %d, %d, %d\n",
	     device_number, segment_index, segment_offset, memory_data_form, disc_data_form, data_count );
#endif

    if ( *NITEMS > 0 )
    {
	if ( allocate_segment ( device_number, segment_index ) ) return ( TRUE );

	if ( load_segment ( device_number, segment_index ) ) return ( TRUE );

	cache_index = device_cache_index [ device_number ] [ segment_index ];

	segment_address = cache_segment_address [ cache_index ];

	cform_ ( segment_address + segment_offset, data_buffer, DISCF, MEMF, NITEMS );
    }

    return ( FALSE );
}

BooleanFlag write_segment_data ( device_number, segment_index, segment_offset,
				 data_buffer, partial_write,
				 MEMF, DISCF, NITEMS )
/*==========================================================================*/

DeviceNumber     device_number;
SegmentIndex     segment_index;
SegmentSize      segment_offset;
BufferAddress    data_buffer;
BooleanFlag      partial_write;
F_integer       *MEMF;
F_integer       *DISCF;
F_longinteger   *NITEMS;
{
    CacheIndex     cache_index;
    SegmentAddress segment_address;

#ifdef MONITOR
    int   memory_data_form;
    int   disc_data_form;
    long int   data_count;

    memory_data_form = *MEMF;
    disc_data_form = *DISCF;
    data_count = *NITEMS;

    printf ( "write_segment_data: d_n, s_i, s_o, p_w, m_d_f, d_d_f, d_c = %d, %d, %d, %d, %d, %d, %d\n",
	     device_number, segment_index, segment_offset, partial_write, memory_data_form, disc_data_form, data_count );
#endif

    if ( *NITEMS > 0 )
    {
	if ( allocate_segment ( device_number, segment_index ) ) return ( TRUE );

	cache_index = device_cache_index [ device_number ] [ segment_index ];

	if ( partial_write )
	{
	    if ( load_segment ( device_number, segment_index ) ) return ( TRUE );
	}

	else
	{
	    cache_status_mask [ cache_index ] |= SEGMENT_LOADED;
	}

	segment_address = cache_segment_address [ cache_index ];

	cform_ ( data_buffer, segment_address + segment_offset, MEMF, DISCF, NITEMS );

	cache_status_mask [ cache_index ] &= ~SEGMENT_SYNCED;
    }

    return ( FALSE );
}

BooleanFlag open_old_file ( device_number, file_name, write_access )
/*================================================================*/

DeviceNumber device_number;
FileName     file_name;
BooleanFlag  write_access;
{
    StatusMask    status_mask;
    DeviceSize    memory_size;
    FileHandle    file_handle;
    DeviceSize    file_size;
    FileName      duplicate_file_name;
    BufferAddress buffer_address;

#ifdef MONITOR
    printf ( "open_old_file: d_n, f_n, w_a = %d, %s, %d\n",
	     device_number, file_name, write_access );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & FILE_OPEN ) return ( TRUE );

    if ( status_mask & MEMORY_OPEN )
    {
	memory_size = device_storage_size [ device_number ];

	if ( memory_size > 0 ) return ( TRUE );
    }

    if ( open_file ( file_name, write_access, &file_handle, &file_size ) ) return ( TRUE );

    duplicate_file_name = (FileName) duplicate_string ( file_name );

    device_file_handle   [ device_number ] = file_handle;
    device_file_name     [ device_number ] = duplicate_file_name;
    device_file_segments [ device_number ] = 0;

    device_status_mask [ device_number ] &= ~SCRATCH_FILE;

    if ( write_access )
    {
	device_status_mask [ device_number ] |=  WRITE_ACCESS;
    }

    else
    {
	device_status_mask [ device_number ] &= ~WRITE_ACCESS;
    }

    if ( status_mask & MEMORY_OPEN )
    {
	if ( file_size > 0 )
	{
	    buffer_address = (BufferAddress) allocate_memory ( file_size );

	    if ( buffer_address == (BufferAddress) NULL )
	    {
		free_memory ( duplicate_file_name );

		shut_file ( file_handle );

		return ( TRUE );
	    }

	    device_buffer_address [ device_number ] = buffer_address;

	    if ( read_file ( file_handle, 0, buffer_address, file_size ) )
	    {
		free_memory ( buffer_address );

		free_memory ( duplicate_file_name );

		shut_file ( file_handle );

		return ( TRUE );
	    }

	    device_status_mask [ device_number ] |= MEMORY_SYNCED;
	}
    }

    device_storage_size [ device_number ] = file_size;

    device_status_mask [ device_number ] |= FILE_OPEN;

    if ( create_segment_table ( device_number ) ) return ( TRUE );

    return ( FALSE );
}

BooleanFlag open_new_file ( device_number, file_name )
/*==================================================*/

DeviceNumber device_number;
FileName     file_name;
{
    StatusMask    status_mask;
    FileHandle    file_handle;
    DeviceSize    file_size;
    FileName      duplicate_file_name;
    DeviceSize    memory_size;
    BufferAddress buffer_address;

#ifdef MONITOR
    printf ( "open_new_file: d_n, f_n = %d, %s\n",
	     device_number, file_name );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & FILE_OPEN ) return ( TRUE );

    if ( create_file ( file_name, FALSE, &file_handle ) ) return ( TRUE );

    duplicate_file_name = (FileName) duplicate_string ( file_name );

    device_file_handle   [ device_number ] = file_handle;
    device_file_name     [ device_number ] = duplicate_file_name;
    device_file_segments [ device_number ] = 0;

    device_status_mask [ device_number ] &= ~SCRATCH_FILE;
    device_status_mask [ device_number ] |= WRITE_ACCESS;

    if ( status_mask & MEMORY_OPEN )
    {
	memory_size = device_storage_size [ device_number ];

	if ( memory_size > 0 )
	{
	    buffer_address = device_buffer_address [ device_number ];

	    if ( write_file ( file_handle, 0, buffer_address, memory_size ) )
	    {
		free_memory ( duplicate_file_name );

		shut_file ( file_handle );

		delete_file ( file_name );

		return ( TRUE );
	    }

	    device_status_mask [ device_number ] |= MEMORY_SYNCED;
	}

	file_size = memory_size;
    }

    else
    {
	file_size = 0;
    }

    device_storage_size [ device_number ] = file_size;

    device_status_mask [ device_number ] |= FILE_OPEN;

    if ( create_segment_table ( device_number ) ) return ( TRUE );

    return ( FALSE );
}

BooleanFlag open_scratch_file ( device_number, file_name )
/*======================================================*/

DeviceNumber device_number;
FileName     file_name;
{
    StatusMask status_mask;
    DeviceSize memory_size;
    FileHandle file_handle;
    FileName   duplicate_file_name;
    DeviceSize file_size;

#ifdef MONITOR
    printf ( "open_scratch_file: d_n, f_n = %d, %s\n",
	     device_number, file_name );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & FILE_OPEN ) return ( TRUE );

    memory_size = device_storage_size [ device_number ];

    if ( create_file ( file_name, TRUE, &file_handle ) ) return ( TRUE );

    duplicate_file_name = (FileName) NULL;

    device_file_handle   [ device_number ] = file_handle;
    device_file_name     [ device_number ] = duplicate_file_name;
    device_file_segments [ device_number ] = 0;

    device_status_mask [ device_number ] |= SCRATCH_FILE;
    device_status_mask [ device_number ] |= WRITE_ACCESS;

    if ( status_mask & MEMORY_OPEN )
    {
	if ( memory_size > 0 )
	{
	    if ( resize_file ( file_handle, memory_size ) )
	    {
		free_memory ( duplicate_file_name );

		shut_file ( file_handle );

		delete_file ( file_name );

		return ( TRUE );
	    }

	    device_status_mask [ device_number ] &= ~MEMORY_SYNCED;
	}

	file_size = memory_size;
    }

    else
    {
	file_size = 0;
    }

    device_storage_size [ device_number ] = file_size;

    device_status_mask [ device_number ] |= FILE_OPEN;

    if ( create_segment_table ( device_number ) ) return ( TRUE );

    return ( FALSE );
}

BooleanFlag open_memory ( device_number )
/*=====================================*/

DeviceNumber device_number;
{
    StatusMask    status_mask;
    DeviceSize    file_size;
    BufferAddress buffer_address;
    FileHandle    file_handle;

#ifdef MONITOR
    printf ( "open_memory: d_n = %d\n",
	     device_number );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & MEMORY_OPEN ) return ( TRUE );

    if ( status_mask & FILE_OPEN )
    {
	file_size = device_storage_size [ device_number ];

	if ( file_size > 0 )
	{
	    if ( flush_segment_table ( device_number ) ) return ( TRUE );

	    if ( free_segment_table ( device_number ) ) return ( TRUE );

	    buffer_address = (BufferAddress) allocate_memory ( file_size );

	    if ( buffer_address == (BufferAddress) NULL ) return ( TRUE );

	    device_buffer_address [ device_number ] = buffer_address;

	    file_handle = device_file_handle [ device_number ];

	    if ( read_file ( file_handle, 0, buffer_address, file_size ) )
	    {
		free_memory ( buffer_address );

		return ( TRUE );
	    }

	    device_status_mask [ device_number ] |= MEMORY_SYNCED;
	}
    }

    else
    {
	device_storage_size [ device_number ] = 0;

	device_status_mask [ device_number ] |= WRITE_ACCESS;
    }

    device_status_mask [ device_number ] |= MEMORY_OPEN;

    return ( FALSE );
}

BooleanFlag close_file ( device_number, delete )
/*============================================*/

DeviceNumber device_number;
BooleanFlag  delete;
{
    StatusMask  status_mask;
    DeviceSize  file_size;
    FileHandle  file_handle;
    FileName    file_name;
    BooleanFlag return_status = FALSE;

#ifdef MONITOR
    printf ( "close_file: d_n, d = %d, %d\n",
	     device_number, delete );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & FILE_OPEN )
    {
	if ( ! ( delete || ( status_mask & SCRATCH_FILE ) ) )
	{
	    file_size = device_storage_size [ device_number ];

	    if ( flush_device ( device_number, 0, file_size ) ) return ( TRUE );
	}

	if ( free_segment_table ( device_number ) ) return ( TRUE );

	file_handle = device_file_handle [ device_number ];

	if ( shut_file ( file_handle ) ) return_status = TRUE;

	file_name = device_file_name [ device_number ];

	if ( file_name != (FileName) NULL )
	{
	    if ( delete )
	    {
		if ( delete_file ( file_name ) ) return_status = TRUE;
	    }

	    free_memory ( file_name );
	}

	device_status_mask [ device_number ] &= ~FILE_OPEN;
    }

    return ( return_status );
}

BooleanFlag close_memory ( device_number )
/*======================================*/

DeviceNumber device_number;
{
    StatusMask    status_mask;
    DeviceSize    memory_size;
    BufferAddress buffer_address;
    FileHandle    file_handle;

#ifdef MONITOR
    printf ( "close_memory: d_n = %d\n",
	     device_number );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & MEMORY_OPEN )
    {
	memory_size = device_storage_size [ device_number ];

	if ( memory_size > 0 )
	{
	    buffer_address = device_buffer_address [ device_number ];

	    if ( status_mask & FILE_OPEN )
	    {
		if ( ! ( status_mask & MEMORY_SYNCED ) )
		{
		    file_handle = device_file_handle [ device_number ];

		    if ( write_file ( file_handle, 0, buffer_address, memory_size ) )
		    {
			return ( TRUE );
		    }
		}
	    }

	    free_memory ( buffer_address );
	}

	device_status_mask [ device_number ] &= ~MEMORY_OPEN;
    }

    if ( create_segment_table ( device_number ) ) return ( TRUE );

    return ( FALSE );
}

BooleanFlag resize_device ( device_number, new_device_size )
/*========================================================*/

DeviceNumber device_number;
DeviceSize   new_device_size;
{
    StatusMask    status_mask;
    DeviceSize    old_device_size;
    FileHandle    file_handle;
    BufferAddress buffer_address;

#ifdef MONITOR
    printf ( "resize_device: d_n, n_d_s = %d, %d\n",
	     device_number, new_device_size );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) 
	{
		printf("Bad device number \n" );
		return ( TRUE );
	}
    if ( new_device_size < 0 )
        {
                printf("Negative device size \n" );
                return ( TRUE );
        }

    status_mask = device_status_mask [ device_number ];

    if ( ! ( status_mask & DEVICE_OPEN ) ) 
	       	{
                printf("Failed to open the device \n" );
                return ( TRUE );
        	}

    if ( ! ( status_mask & WRITE_ACCESS ) ) 
                {
                printf("No write access to device \n" );
                return ( TRUE );
                }

    old_device_size = device_storage_size [ device_number ];

    if ( flush_segment_table ( device_number ) ) 
                {
                printf("Failed to flush segment table 1 \n" );
                return ( TRUE );
                }

    if ( free_segment_table ( device_number ) ) 
                {
                printf("Failed to open the device \n" );
                return ( TRUE );
                }

    if ( status_mask & FILE_OPEN )
    {
	file_handle = device_file_handle [ device_number ];

	if ( resize_file ( file_handle, new_device_size ) ) 
                {
                printf("Failed to resize new device \n" );
                return ( TRUE );
                }
    }

    if ( status_mask & MEMORY_OPEN )
    {
	if ( old_device_size > 0 )
	{
	    buffer_address = device_buffer_address [ device_number ];

	    if ( new_device_size > 0 )
	    {
		buffer_address = (BufferAddress) resize_memory ( buffer_address, new_device_size );

		if ( buffer_address == (BufferAddress) NULL ) 
                {
                printf("Invalid buffer_address when resizing \n" );
                return ( TRUE );
                }

		device_buffer_address [ device_number ] = buffer_address;
	    }

	    else
	    {
		free_memory ( buffer_address );
	    }
	}

	else
	{
	    if ( new_device_size > 0 )
	    {
		buffer_address = (BufferAddress) allocate_memory ( new_device_size );

		if ( buffer_address == (BufferAddress) NULL ) 
                {
                printf("Invalid buffer_address when allocating\n" );
                return ( TRUE );
                }
		device_buffer_address [ device_number ] = buffer_address;
	    }
	}
    }

    device_storage_size [ device_number ] = new_device_size;

    if ( create_segment_table ( device_number ) )
                {
                printf("Create segment table failed \n" );
                return ( TRUE );
                }

    if ( new_device_size > old_device_size )
    {
	if ( zero_device ( device_number, old_device_size, new_device_size - old_device_size ) ) 
                {
                printf("Zero device failed \n" );
                return ( TRUE );
                }
    }

    return ( FALSE );
}

BooleanFlag flush_device ( device_number, device_offset, data_count )
/*=================================================================*/

DeviceNumber device_number;
DeviceSize   device_offset;
DeviceSize   data_count;
{
    StatusMask    status_mask;
    DeviceSize    device_size;
    FileHandle    file_handle;
    BufferAddress buffer_address;
    SegmentIndex  i;
    SegmentIndex  i1;
    SegmentIndex  i2;

#ifdef MONITOR
    printf ( "flush_device: d_n, d_o, d_c = %d, %d, %d\n",
	     device_number, device_offset, data_count );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    if ( device_offset < 0 || data_count < 0 ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( status_mask & FILE_OPEN )
    {
	device_size = device_storage_size [ device_number ];

	if ( device_offset + data_count > device_size ) return ( TRUE );

	if ( device_size > 0 )
	{
	    if ( status_mask & WRITE_ACCESS )
	    {
		if ( status_mask & MEMORY_OPEN )
		{
		    if ( ! ( status_mask & MEMORY_SYNCED ) )
		    {
			file_handle = device_file_handle [ device_number ];

			buffer_address = device_buffer_address [ device_number ];

			if ( write_file ( file_handle, device_offset,
					  buffer_address + device_offset,
					  data_count ) ) return ( TRUE );

			if ( data_count == device_size )
			{
			    device_status_mask [ device_number ] |= MEMORY_SYNCED;
			}
		    }
		}

		else if ( device_file_segments [ device_number ] > 0 )
		{
		    i1 = device_offset / cache_segment_size;
		    i2 = ( device_offset + data_count - 1 ) / cache_segment_size;

		    for ( i = i1; i <= i2; i++ )
		    {
			if ( flush_segment ( device_number, i ) ) return ( TRUE );
		    }
		}
	    }
	}
    }

    return ( FALSE );
}

void query_device ( device_number, status_mask, device_size )
/*=========================================================*/

DeviceNumber  device_number;
StatusMask   *status_mask;
DeviceSize   *device_size;
{
    *status_mask = 0;
    *device_size = 0;

    if ( ! ( device_number < 0 || device_number > MAXDEV ) )
    {
	if ( device_status_mask [ device_number ] & DEVICE_OPEN )
	{
	    *status_mask = device_status_mask  [ device_number ];
	    *device_size = device_storage_size [ device_number ];
	}
    }
}

void query_file ( device_number, file_handle, file_size, file_name )
/*================================================================*/

DeviceNumber device_number;
FileHandle  *file_handle;
DeviceSize  *file_size;
FileName    *file_name;
{
    *file_handle = -1;
    *file_size   = 0;
    *file_name   = (FileName) NULL;

    if ( ! ( device_number < 0 || device_number > MAXDEV ) )
    {
	if ( device_status_mask [ device_number ] & FILE_OPEN )
	{
	    *file_handle = device_file_handle  [ device_number ];
	    *file_size   = device_storage_size [ device_number ];
	    *file_name   = device_file_name    [ device_number ];
	}
    }
}

void query_memory ( device_number, buffer_address )
/*===============================================*/

DeviceNumber   device_number;
BufferAddress *buffer_address;
{
    *buffer_address = (BufferAddress) NULL;

    if ( ! ( device_number < 0 || device_number > MAXDEV ) )
    {
	if ( device_status_mask [ device_number ] & MEMORY_OPEN )
	{
	    if ( device_storage_size [ device_number ] > 0 )
	    {
		*buffer_address = device_buffer_address [ device_number ];
	    }
	}
    }
}

BooleanFlag read_device ( device_number, device_offset, data_buffer, data_count )
/*=============================================================================*/

DeviceNumber   device_number;
DeviceSize     device_offset;
BufferAddress  data_buffer;
DeviceSize     data_count;
{
    StatusMask    status_mask;
    DeviceSize    device_size;
    BufferAddress buffer_address;
    SegmentIndex  segment_index;
    SegmentSize   segment_offset;
    SegmentSize   segment_count;
    DeviceSize    data_offset;
    FileHandle    file_handle;

#ifdef MONITOR
    printf ( "read_device: d_n, d_o, d_c = %d, %d, %d\n",
	     device_number, device_offset, data_count );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    if ( device_offset < 0 || data_count < 0 ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( ! ( status_mask & DEVICE_OPEN ) ) return ( TRUE );

    device_size = device_storage_size [ device_number ];

    if ( device_offset + data_count > device_size ) return ( TRUE );

    if ( status_mask & MEMORY_OPEN )
    {
	buffer_address = device_buffer_address [ device_number ];

	copy_memory ( buffer_address + device_offset, data_buffer, data_count );
    }

    else if ( device_file_segments [ device_number ] > 0 )
    {
	segment_index  = device_offset / cache_segment_size;
	segment_offset = device_offset - segment_index * cache_segment_size;

	if ( segment_offset + data_count <= cache_segment_size )
	{
	    segment_count = data_count;

	    if ( read_segment ( device_number, segment_index,
				segment_offset, data_buffer,
				segment_count ) ) return ( TRUE );
	}

	else
	{
	    segment_count = cache_segment_size - segment_offset;

	    if ( read_segment ( device_number, segment_index++,
				segment_offset, data_buffer,
				segment_count ) ) return ( TRUE );

	    data_offset = segment_count;

	    segment_offset = 0;
	    segment_count  = cache_segment_size;

	    while ( data_offset + segment_count <= data_count )
	    {
		if ( read_segment ( device_number, segment_index++,
				    segment_offset, data_buffer + data_offset,
				    segment_count ) ) return ( TRUE );

		data_offset += segment_count;
	    }

	    if ( data_offset < data_count )
	    {
		segment_count = data_count - data_offset;

		if ( read_segment ( device_number, segment_index,
				    segment_offset, data_buffer + data_offset,
				    segment_count ) ) return ( TRUE );
	    }
	}
    }

    else
    {
	file_handle = device_file_handle [ device_number ];

	if ( read_file ( file_handle, device_offset, data_buffer, data_count ) ) return ( TRUE );
    }

    return ( FALSE );
}

BooleanFlag write_device ( device_number, device_offset, data_buffer, data_count )
/*==============================================================================*/

DeviceNumber   device_number;
DeviceSize     device_offset;
BufferAddress  data_buffer;
DeviceSize     data_count;
{
    StatusMask    status_mask;
    DeviceSize    device_size;
    BufferAddress buffer_address;
    SegmentIndex  segment_index;
    SegmentSize   segment_offset;
    SegmentSize   segment_count;
    DeviceSize    data_offset;
    FileHandle    file_handle;

#ifdef MONITOR
    printf ( "write_device: d_n, d_o, d_c = %d, %d, %d\n",
	     device_number, device_offset, data_count );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    if ( device_offset < 0 || data_count < 0 ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( ! ( status_mask & DEVICE_OPEN ) ) return ( TRUE );

    if ( ! ( status_mask & WRITE_ACCESS ) ) return ( TRUE );

    device_size = device_storage_size [ device_number ];

    if ( device_offset < 0 || device_offset + data_count > device_size ) return ( TRUE );

    if ( status_mask & MEMORY_OPEN )
    {
	buffer_address = device_buffer_address [ device_number ];

	copy_memory ( data_buffer, buffer_address + device_offset, data_count );

	device_status_mask [ device_number ] &= ~MEMORY_SYNCED;
    }

    else if ( device_file_segments [ device_number ] > 0 )
    {
	segment_index  = device_offset / cache_segment_size;
	segment_offset = device_offset - segment_index * cache_segment_size;

	if ( segment_offset + data_count <= cache_segment_size )
	{
	    segment_count = data_count;

	    if ( write_segment ( device_number, segment_index,
				 segment_offset, data_buffer,
				 segment_count ) ) return ( TRUE );
	}

	else
	{
	    segment_count = cache_segment_size - segment_offset;

	    if ( write_segment ( device_number, segment_index++,
				 segment_offset, data_buffer,
				 segment_count ) ) return ( TRUE );

	    data_offset = segment_count;

	    segment_offset = 0;
	    segment_count  = cache_segment_size;

	    while ( data_offset + segment_count <= data_count )
	    {
		if ( write_segment ( device_number, segment_index++,
				     segment_offset, data_buffer + data_offset,
				     segment_count ) ) return ( TRUE );

		data_offset += segment_count;
	    }

	    if ( data_offset < data_count )
	    {
		segment_count = data_count - data_offset;

		if ( write_segment ( device_number, segment_index,
				     segment_offset, data_buffer + data_offset,
				     segment_count ) ) return ( TRUE );
	    }
	}
    }

    else
    {
	file_handle = device_file_handle [ device_number ];

	if ( write_file ( file_handle, device_offset, data_buffer, data_count ) ) return ( TRUE );
    }

    return ( FALSE );
}

BooleanFlag zero_device ( device_number, device_offset, data_count )
/*================================================================*/

DeviceNumber   device_number;
DeviceSize     device_offset;
DeviceSize     data_count;
{
    StatusMask    status_mask;
    DeviceSize    device_size;
    BufferAddress buffer_address;
    SegmentIndex  segment_index;
    SegmentSize   segment_offset;
    SegmentSize   segment_count;
    DeviceSize    zero_count;
    FileHandle    file_handle;

#ifdef MONITOR
    printf ( "zero_device: d_n, d_o, d_c = %d, %d, %d\n",
	     device_number, device_offset, data_count );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    if ( device_offset < 0 || data_count < 0 ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( ! ( status_mask & DEVICE_OPEN ) ) return ( TRUE );

    if ( ! ( status_mask & WRITE_ACCESS ) ) return ( TRUE );

    device_size = device_storage_size [ device_number ];

    if ( device_offset < 0 || device_offset + data_count > device_size ) return ( TRUE );

    if ( status_mask & MEMORY_OPEN )
    {
	buffer_address = device_buffer_address [ device_number ];

	zero_memory ( buffer_address + device_offset, data_count );

	device_status_mask [ device_number ] &= ~MEMORY_SYNCED;
    }

    else if ( device_file_segments [ device_number ] > 0 )
    {
	segment_index  = device_offset / cache_segment_size;
	segment_offset = device_offset - segment_index * cache_segment_size;

	if ( segment_offset + data_count <= cache_segment_size )
	{
	    segment_count = data_count;

	    if ( zero_segment ( device_number, segment_index,
				segment_offset, segment_count ) ) return ( TRUE );
	}

	else
	{
	    segment_count = cache_segment_size - segment_offset;

	    if ( zero_segment ( device_number, segment_index++,
				segment_offset, segment_count ) ) return ( TRUE );

	    zero_count = segment_count;

	    segment_offset = 0;
	    segment_count  = cache_segment_size;

	    while ( zero_count + segment_count <= data_count )
	    {
		if ( zero_segment ( device_number, segment_index++,
				    segment_offset, segment_count ) ) return ( TRUE );

		zero_count += segment_count;
	    }

	    if ( zero_count < data_count )
	    {
		segment_count = data_count - zero_count;

		if ( zero_segment ( device_number, segment_index,
				    segment_offset, segment_count ) ) return ( TRUE );
	    }
	}
    }

    else
    {
	file_handle = device_file_handle [ device_number ];

	if ( zero_file ( file_handle, device_offset, data_count ) ) return ( TRUE );
    }

    return ( FALSE );
}

BooleanFlag read_device_data ( DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF )
/*===================================================================*/

F_integer     *DEVICE;
F_longinteger *NITEMS;
F_array        MEM; /* ****** CHANGE ****** MEM for *MEM; WOS 12.4.01 */
F_longinteger *BLKN;
F_integer     *MEMF;
F_integer     *DISCF;
{
    DeviceNumber    device_number;
    F_longinteger   data_count;
    BufferAddress   data_buffer;
    BlockNumber     block_number;
    int             memory_data_form;
    int             disc_data_form;
    int             memory_data_bytes;
    int             disc_data_bytes;
    StatusMask      status_mask;
    DeviceSize      device_size;
    DeviceSize      device_offset;
    DeviceSize      device_byte_count;
    BufferAddress   buffer_address;
    SegmentIndex    segment_index;
    SegmentSize     segment_offset;
    SegmentSize     disc_byte_count;
    SegmentSize     memory_byte_increment;
    SegmentSize     disc_byte_increment;
    DeviceSize      memory_byte_offset;
    DeviceSize      disc_byte_offset;
    FileHandle      file_handle;

    device_number    = *DEVICE;
    data_count       = *NITEMS;
    data_buffer      = (BufferAddress) MEM;
    block_number     = *BLKN;
    memory_data_form = *MEMF;
    disc_data_form   = *DISCF;

#ifdef MONITOR
    printf ( "read_device_data: d_n, d_c, b_n, m_d_f, d_d_f = %d, %d, %d, %d, %d\n",
	     device_number, data_count, block_number, memory_data_form, disc_data_form );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    if ( block_number < 1 || data_count < 0 ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( ! ( status_mask & DEVICE_OPEN ) ) return ( TRUE );

    device_offset = ( block_number - 1 ) * LNBLK;

    memory_data_bytes = form_to_bytes ( memory_data_form );
    disc_data_bytes   = form_to_bytes ( disc_data_form );

    device_byte_count = data_count * disc_data_bytes;

    device_size = device_storage_size [ device_number ];

    if ( device_offset < 0 || device_offset + device_byte_count > device_size ) return ( TRUE );

    if ( status_mask & MEMORY_OPEN )
    {
	buffer_address = device_buffer_address [ device_number ];

	cform_ ( buffer_address + device_offset, MEM, DISCF, MEMF, NITEMS );
    }

    else if ( device_file_segments [ device_number ] > 0 )
    {
	segment_index  = device_offset / cache_segment_size;
	segment_offset = device_offset - segment_index * cache_segment_size;

	if ( segment_offset + device_byte_count <= cache_segment_size )
	{
	    if ( read_segment_data ( device_number, segment_index,
				     segment_offset, data_buffer,
				     MEMF, DISCF, NITEMS ) ) return ( TRUE );
	}

	else
	{
	    disc_byte_count = cache_segment_size - segment_offset;

	    data_count = disc_byte_count / disc_data_bytes;

	    memory_byte_increment = data_count * memory_data_bytes;
	    disc_byte_increment   = data_count * disc_data_bytes;

	    if ( disc_byte_increment != disc_byte_count ) return ( TRUE );

	    if ( read_segment_data ( device_number, segment_index++,
				     segment_offset, data_buffer,
				     MEMF, DISCF, &data_count ) ) return ( TRUE );

	    segment_offset = 0;

	    memory_byte_offset = memory_byte_increment;
	    disc_byte_offset   = disc_byte_increment;

	    disc_byte_count = cache_segment_size;

	    data_count = disc_byte_count / disc_data_bytes;

	    memory_byte_increment = data_count * memory_data_bytes;
	    disc_byte_increment   = data_count * disc_data_bytes;

	    if ( disc_byte_increment != disc_byte_count ) return ( TRUE );

	    while ( disc_byte_offset + disc_byte_increment <= device_byte_count )
	    {
		if ( read_segment_data ( device_number, segment_index++,
					 segment_offset, data_buffer + memory_byte_offset,
					 MEMF, DISCF, &data_count ) ) return ( TRUE );

		memory_byte_offset += memory_byte_increment;
		disc_byte_offset   += disc_byte_increment;
	    }

	    if ( disc_byte_offset < device_byte_count )
	    {
		disc_byte_count = device_byte_count - disc_byte_offset;

		data_count = disc_byte_count / disc_data_bytes;

		if ( read_segment_data ( device_number, segment_index,
					 segment_offset, data_buffer + memory_byte_offset,
					 MEMF, DISCF, &data_count ) ) return ( TRUE );
	    }
	}
    }

    else
    {
	file_handle = device_file_handle [ device_number ];

	if ( memory_data_bytes == disc_data_bytes )
	{
	    if ( read_file ( file_handle, device_offset, data_buffer, device_byte_count ) ) return ( TRUE );
	}

	else
	{
	    if ( io_buffer == (BufferAddress) NULL )
	    {
		io_buffer_size = BUFSIZ * ( 1 + ( 32768 - 1 ) / BUFSIZ );

		io_buffer = allocate_memory ( io_buffer_size );

		if (io_buffer == (BufferAddress) NULL )
		{
		    io_buffer_size = 0;

		    return ( TRUE );
		}
	    }

	    if ( device_byte_count > io_buffer_size )
	    {
		io_buffer_size = device_byte_count;

		io_buffer = (BufferAddress) resize_memory ( io_buffer, io_buffer_size );

		if ( io_buffer == (BufferAddress) NULL )
		{
		    io_buffer_size = 0;

		    return ( TRUE );
		}
	    }

	    if ( read_file ( file_handle, device_offset, io_buffer, device_byte_count ) ) return ( TRUE );

	    cform_ ( io_buffer, MEM, DISCF, MEMF, NITEMS );
	}
    }

    return ( FALSE );
}

BooleanFlag write_device_data ( DEVICE, NITEMS, MEM, BLKN, MEMF, DISCF )
/*====================================================================*/

F_integer     *DEVICE;
F_longinteger *NITEMS;
F_array        MEM; /* ****** CHANGE ****** MEM for *MEM; WOS 12/4/01 */
F_longinteger *BLKN;
F_integer     *MEMF;
F_integer     *DISCF;
{
    DeviceNumber    device_number;
    F_longinteger   data_count;
    BufferAddress   data_buffer;
    BlockNumber     block_number;
    int             memory_data_form;
    int             disc_data_form;
    int             memory_data_bytes;
    int             disc_data_bytes;
    StatusMask      status_mask;
    DeviceSize      device_size;
    DeviceSize      device_byte_count;
    DeviceSize      device_offset;
    BufferAddress   buffer_address;
    SegmentIndex    segment_index;
    SegmentSize     segment_offset;
    SegmentSize     disc_byte_count;
    SegmentSize     memory_byte_increment;
    SegmentSize     disc_byte_increment;
    DeviceSize      memory_byte_offset;
    DeviceSize      disc_byte_offset;
    FileHandle      file_handle;

    device_number    = *DEVICE;
    data_count       = *NITEMS;
    data_buffer      = (BufferAddress) MEM;
    block_number     = *BLKN;
    memory_data_form = *MEMF;
    disc_data_form   = *DISCF;

#ifdef MONITOR
    printf ( "write_device_data: d_n, d_c, b_n, m_d_f, d_d_f = %d, %d, %d, %d, %d\n",
	     device_number, data_count, block_number, memory_data_form, disc_data_form );
#endif

    if ( device_number < 0 || device_number > MAXDEV ) return ( TRUE );

    if ( block_number < 1 || data_count < 0 ) return ( TRUE );

    status_mask = device_status_mask [ device_number ];

    if ( ! ( status_mask & DEVICE_OPEN ) ) return ( TRUE );

    if ( ! ( status_mask & WRITE_ACCESS ) ) return ( TRUE );

    device_offset = ( block_number - 1 ) * LNBLK;

    memory_data_bytes = form_to_bytes ( memory_data_form );
    disc_data_bytes   = form_to_bytes ( disc_data_form );

    device_byte_count = data_count * disc_data_bytes;

    device_size = device_storage_size [ device_number ];

    if ( device_offset < 0 || device_offset + device_byte_count > device_size ) return ( TRUE );

    if ( status_mask & MEMORY_OPEN )
    {
	buffer_address = device_buffer_address [ device_number ];

	cform_ ( MEM, buffer_address + device_offset, MEMF, DISCF, NITEMS );

	device_status_mask [ device_number ] &= ~MEMORY_SYNCED;
    }

    else if ( device_file_segments [ device_number ] > 0 )
    {
	segment_index  = device_offset / cache_segment_size;
	segment_offset = device_offset - segment_index * cache_segment_size;

	if ( segment_offset + device_byte_count <= cache_segment_size )
	{
	    if ( write_segment_data ( device_number, segment_index,
				      segment_offset, data_buffer, TRUE,
				      MEMF, DISCF, NITEMS ) ) return ( TRUE );
	}

	else
	{
	    disc_byte_count = cache_segment_size - segment_offset;

	    data_count = disc_byte_count / disc_data_bytes;

	    memory_byte_increment = data_count * memory_data_bytes;
	    disc_byte_increment   = data_count * disc_data_bytes;

	    if ( disc_byte_increment != disc_byte_count ) return ( TRUE );

	    if ( write_segment_data ( device_number,
				      segment_index++,
				      segment_offset,
				      data_buffer,
				      disc_byte_count != cache_segment_size,
				      MEMF, DISCF,
				      &data_count ) ) return ( TRUE );

	    segment_offset = 0;

	    memory_byte_offset = memory_byte_increment;
	    disc_byte_offset   = disc_byte_increment;

	    disc_byte_count = cache_segment_size;

	    data_count = disc_byte_count / disc_data_bytes;

	    memory_byte_increment = data_count * memory_data_bytes;
	    disc_byte_increment   = data_count * disc_data_bytes;

	    if ( disc_byte_increment != disc_byte_count ) return ( TRUE );

	    while ( disc_byte_offset + disc_byte_increment <= device_byte_count )
	    {
		if ( write_segment_data ( device_number,
					  segment_index++,
					  segment_offset,
					  data_buffer + memory_byte_offset,
					  FALSE, MEMF, DISCF,
					  &data_count ) ) return ( TRUE );

		memory_byte_offset += memory_byte_increment;
		disc_byte_offset   += disc_byte_increment;
	    }

	    if ( disc_byte_offset < device_byte_count )
	    {
		disc_byte_count = device_byte_count - disc_byte_offset;

		data_count = disc_byte_count / disc_data_bytes;

		if ( write_segment_data ( device_number,
					  segment_index,
					  segment_offset,
					  data_buffer + memory_byte_offset,
					  TRUE, MEMF, DISCF,
					  &data_count ) ) return ( TRUE );
	    }
	}
    }

    else
    {
	file_handle = device_file_handle [ device_number ];

	if ( memory_data_bytes == disc_data_bytes )
	{
	    if ( write_file ( file_handle, device_offset, data_buffer, device_byte_count ) ) return ( TRUE );
	}

	else
	{
	    if ( io_buffer == (BufferAddress) NULL )
	    {
		io_buffer_size = BUFSIZ * ( 1 + ( 32768 - 1 ) / BUFSIZ );

		io_buffer = allocate_memory ( io_buffer_size );

		if (io_buffer == (BufferAddress) NULL )
		{
		    io_buffer_size = 0;

		    return ( TRUE );
		}
	    }

	    if ( device_byte_count > io_buffer_size )
	    {
		io_buffer_size = device_byte_count;

		io_buffer = (BufferAddress) resize_memory ( io_buffer, io_buffer_size );

		if ( io_buffer == (BufferAddress) NULL )
		{
		    io_buffer_size = 0;

		    return ( TRUE );
		}
	    }

	    cform_ ( MEM, io_buffer, MEMF, DISCF, NITEMS );

	    if ( write_file ( file_handle, device_offset, io_buffer, device_byte_count ) ) return ( TRUE );
	}
    }

    return ( FALSE );
}

BooleanFlag allocate_buffer ( buffer_size )
/*========================================*/

DeviceSize buffer_size;
{
	if ( io_buffer == (BufferAddress) NULL || io_buffer_size < buffer_size )
	{
		io_buffer_size = buffer_size;

		io_buffer = allocate_memory ( io_buffer_size );

		if ( io_buffer == (BufferAddress) NULL )
		{
			io_buffer_size = 0;

			return ( TRUE );
		}
	}
	
	return ( FALSE );
}
