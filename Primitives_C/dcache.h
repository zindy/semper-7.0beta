#include "ftypes.h"
#include "params.h"

/*  Maximum device number  */

#define MAXDEV       NDVS

/*  Device status bits  */

#define FILE_OPEN       1
#define MEMORY_OPEN     2
#define DEVICE_OPEN     3
#define WRITE_ACCESS    4
#define SCRATCH_FILE    8
#define MEMORY_SYNCED  16

/*  Cache status bits  */

#define SEGMENT_LOADED  1
#define SEGMENT_SYNCED  2

/*  Typedefs for disc caching parameters  */

typedef          short int   BooleanFlag;
typedef unsigned short int   StatusMask;
typedef          short int   DeviceNumber;
typedef          long  int   SegmentIndex;
typedef          long  int   CacheIndex;
typedef          long  int   SegmentSize;
typedef          long  int   BlockNumber;
typedef          long  int   DeviceSize;
typedef          long  int   MemorySize;
typedef                int   FileHandle;
typedef               char * String;
typedef               char * FileName;
typedef         CacheIndex * SegmentTable;
typedef         CacheIndex * CacheIndexPtr;
typedef               char * SegmentAddress;
typedef               char * BufferAddress;
typedef               void * MemoryAddress;

/*  Function prototypes for disc caching routines  */

#ifdef ANSI_C
MemoryAddress  allocate_memory ( MemorySize );
BooleanFlag    allocate_segment ( DeviceNumber, SegmentIndex );
BooleanFlag    close_file ( DeviceNumber, BooleanFlag );
BooleanFlag    close_memory ( DeviceNumber );
void           copy_memory ( BufferAddress, BufferAddress, DeviceSize );
BooleanFlag    create_cache ( CacheIndex, SegmentSize, CacheIndexPtr );
BooleanFlag    create_file ( FileName, BooleanFlag, FileHandle* );
BooleanFlag    create_segment_table ( DeviceNumber );
BooleanFlag    delete_file ( FileName );
String         duplicate_string ( String );
BooleanFlag    flush_device ( DeviceNumber, DeviceSize, DeviceSize );
BooleanFlag    flush_segment ( DeviceNumber, SegmentIndex );
BooleanFlag    flush_segment_table ( DeviceNumber );
int            form_to_bytes ( int );
BooleanFlag    free_cache ( void );
void           free_memory ( MemoryAddress );
BooleanFlag    free_segment ( DeviceNumber, SegmentIndex );
BooleanFlag    free_segment_table ( DeviceNumber );
BooleanFlag    load_segment ( DeviceNumber, SegmentIndex );
BooleanFlag    open_file ( FileName, BooleanFlag, FileHandle*, DeviceSize* );
BooleanFlag    open_memory ( DeviceNumber );
BooleanFlag    open_new_file ( DeviceNumber, FileName );
BooleanFlag    open_old_file ( DeviceNumber, FileName, BooleanFlag );
BooleanFlag    open_scratch_file ( DeviceNumber, FileName );
void           query_cache ( CacheIndex*, SegmentSize* );
void           query_cache_limits ( CacheIndex*, SegmentSize*, SegmentSize* );
void           query_device ( DeviceNumber, StatusMask*, DeviceSize* );
void           query_file ( DeviceNumber, FileHandle*, DeviceSize*, FileName* );
void           query_memory ( DeviceNumber, BufferAddress* );
void           query_segment ( DeviceNumber, SegmentIndex, SegmentAddress*, StatusMask* );
BooleanFlag    read_device ( DeviceNumber, DeviceSize, BufferAddress, DeviceSize );
BooleanFlag    read_device_data ( F_integer*, F_longinteger*, F_array,
                                  F_longinteger*, F_integer*, F_integer* );
BooleanFlag    read_file ( FileHandle, DeviceSize, BufferAddress, DeviceSize );
BooleanFlag    read_segment ( DeviceNumber, SegmentIndex, SegmentSize,
                              BufferAddress, SegmentSize );
BooleanFlag    read_segment_data ( DeviceNumber, SegmentIndex, SegmentSize,
                                   BufferAddress,
                                   F_integer*, F_integer*, F_longinteger* );
BooleanFlag    resize_device ( DeviceNumber, DeviceSize );
BooleanFlag    resize_file ( FileHandle, DeviceSize );
MemoryAddress  resize_memory ( MemoryAddress, MemorySize );
BooleanFlag    shut_file ( FileHandle );
BooleanFlag    write_device ( DeviceNumber, DeviceSize, BufferAddress, DeviceSize );
BooleanFlag    write_device_data ( F_integer*, F_longinteger*, F_array,
                                   F_longinteger*, F_integer*, F_integer* );
BooleanFlag    write_file ( FileHandle, DeviceSize, BufferAddress, DeviceSize );
BooleanFlag    write_segment ( DeviceNumber, SegmentIndex, SegmentSize,
                               BufferAddress, SegmentSize );
BooleanFlag    write_segment_data ( DeviceNumber, SegmentIndex, SegmentSize,
                                    BufferAddress, BooleanFlag,
                                    F_integer*, F_integer*, F_longinteger* );
BooleanFlag    zero_device ( DeviceNumber, DeviceSize, DeviceSize );
BooleanFlag    zero_file ( FileHandle, DeviceSize, DeviceSize );
void           zero_memory ( BufferAddress, DeviceSize );
BooleanFlag    zero_segment ( DeviceNumber, SegmentIndex, SegmentSize, SegmentSize );

subroutine     cform_( F_array, F_array, F_integer*, F_integer*, F_longinteger* );
#else
MemoryAddress  allocate_memory ( );
BooleanFlag    allocate_segment ( );
BooleanFlag    close_file ( );
BooleanFlag    close_memory ( );
void           copy_memory ( );
BooleanFlag    create_cache ( );
BooleanFlag    create_file ( );
BooleanFlag    create_segment_table ( );
BooleanFlag    delete_file ( );
String         duplicate_string ( );
BooleanFlag    flush_device ( );
BooleanFlag    flush_segment ( );
BooleanFlag    flush_segment_table ( );
int            form_to_bytes ( );
BooleanFlag    free_cache ( );
void           free_memory ( );
BooleanFlag    free_segment ( );
BooleanFlag    free_segment_table ( );
BooleanFlag    load_segment ( );
BooleanFlag    open_file ( );
BooleanFlag    open_memory ( );
BooleanFlag    open_new_file ( );
BooleanFlag    open_old_file ( );
BooleanFlag    open_scratch_file ( );
void           query_cache ( );
void           query_cache_limits ( );
void           query_device ( );
void           query_file ( );
void           query_memory ( );
void           query_segment ( );
BooleanFlag    read_device ( );
BooleanFlag    read_device_data ( );
BooleanFlag    read_file ( );
BooleanFlag    read_segment ( );
BooleanFlag    read_segment_data ( );
BooleanFlag    resize_device ( );
BooleanFlag    resize_file ( );
MemoryAddress  resize_memory ( );
BooleanFlag    shut_file ( );
BooleanFlag    write_device ( );
BooleanFlag    write_device_data ( );
BooleanFlag    write_file ( );
BooleanFlag    write_segment ( );
BooleanFlag    write_segment_data ( );
BooleanFlag    zero_device ( );
BooleanFlag    zero_file ( );
void           zero_memory ( );
BooleanFlag    zero_segment ( );

subroutine     cform_( );
#endif
