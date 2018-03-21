#include "ftypes.h"
#include "dcache.h"

logicalfunction ccache_ ( IOP, BUFNUM, BUFSIZ, BUFFAC )
/*=====================================================*/

F_integer     *IOP;
F_longinteger *BUFNUM;
F_longinteger *BUFSIZ;
F_longinteger *BUFFAC;
{
    int          opcode;
    CacheIndex   max_number_of_segments;
    SegmentSize  max_segment_size;
    SegmentSize  segment_size_factor;
    CacheIndex   number_of_segments, number_found;
    SegmentSize  segment_size;

    CacheIndexPtr found;

    opcode = *IOP;

    switch ( opcode )
    {
	case 0:
	{
            query_cache_limits ( &max_number_of_segments,
                                 &max_segment_size,
                                 &segment_size_factor );

	    *BUFNUM = max_number_of_segments;
	    *BUFSIZ = max_segment_size;
	    *BUFFAC = segment_size_factor;
	}
	break;

	case 1:
	{
	    query_cache ( &number_of_segments, &segment_size );

	    *BUFNUM = number_of_segments;
	    *BUFSIZ = segment_size;
	}
	break;

	case 2:
	{
	    number_of_segments = (CacheIndex) *BUFNUM;
	    segment_size = (SegmentSize) *BUFSIZ;

	    if ( number_of_segments == 0 || segment_size == 0 )
	    {
		if ( free_cache ( ) ) return ( F_TRUE );
	    }

	    else
	    {
		found = &number_found;
		if ( create_cache ( number_of_segments, segment_size, found ) )
	 	{
		    return ( F_TRUE );
		}
	    }
	}
    }

    return ( F_FALSE );
}
logicalfunction ccache ( IOP, BUFNUM, BUFSIZ, BUFFAC )
/*=====================================================*/

F_integer     *IOP;
F_longinteger *BUFNUM;
F_longinteger *BUFSIZ;
F_longinteger *BUFFAC;
{
        return ccache_ ( IOP, BUFNUM, BUFSIZ, BUFFAC );
}

