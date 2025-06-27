#include <memory.h>
#include "ftypes.h"
#include "params.h"

/*  Semper 6 primitive cform - converts from any storage form to any other
 *
 *  Must be able to convert in place, so loop directions are important.
 *
 *  Coded C. Sullivan 10/3/88
 *  Mods. M. Raisbeck 23/11/89
 *  Mods. M. Raisbeck 23/01/92
 *  Mods. Frank Suess 26/08/92, 25/09/92
 *  July 2005, Portable form (LDM)
 */


subroutine cform_ ( ARRAY_IN, ARRAY_OUT, FORM_IN, FORM_OUT, N )

F_array       ARRAY_IN;
F_array       ARRAY_OUT;
F_integer     *FORM_IN;
F_integer     *FORM_OUT;
F_longinteger *N;

{
    F_byte_array    byte_in,
                    byte_out;
    F_integer_array int_in,
                    int_out;
    F_real_array    real_in,
                    real_out;
    F_complex_array complex_in,
                    complex_out;
    int form_in, form_out;
    int size_in, size_out;
    register long   i, n;

    /*  Fetch input and output data forms  */

    form_in  = *FORM_IN;
    form_out = *FORM_OUT;

    /*  Calculate the data element size for input and output data  */

    if      ( form_in  == NFMBYT ) size_in  = sizeof ( F_byte    );
    else if ( form_in  == NFMINT ) size_in  = sizeof ( F_integer );
    else if ( form_in  == NFMFP  ) size_in  = sizeof ( F_real    );
    else if ( form_in  == NFMCOM ) size_in  = sizeof ( F_complex );
    else                           return;

    if      ( form_out == NFMBYT ) size_out = sizeof ( F_byte    );
    else if ( form_out == NFMINT ) size_out = sizeof ( F_integer );
    else if ( form_out == NFMFP  ) size_out = sizeof ( F_real    );
    else if ( form_out == NFMCOM ) size_out = sizeof ( F_complex );
    else                           return;

    /*  Check if input and output forms are the same      */
    /*  If so, copy the data using a fast system routine  */

    if ( form_in == form_out )
    {

        /*  Calculate number of data bytes to copy  */

        n = *N * size_in;

        /*  Copy the data  */

	(void) memcpy ( (char *) ARRAY_OUT, (char *) ARRAY_IN, n );
    }

    else
    {
        /*  Set up number of times round conversion loop          */
        /*  Note:  First or last value is converted outside loop  */

        n = *N - 1;

	/*  Decide on the conversion needed  */

	if ( form_in == NFMBYT )
	{
	    if ( form_out == NFMINT )
	    {
	         /*  Convert byte to integer  */

                byte_in = (F_byte_array)    ARRAY_IN  + n;
        	int_out = (F_integer_array) ARRAY_OUT + n;

                *int_out = *byte_in;

		for ( i = n; i > 0; i-- )
		{
		    *--int_out = *--byte_in;
		}
	    }

	    else if ( form_out == NFMFP )
	    {
		/*  Convert byte to real  */

                byte_in = (F_byte_array) ARRAY_IN  + n;
        	real_out= (F_real_array) ARRAY_OUT + n;

                *real_out = *byte_in;

		for ( i = n; i > 0; i-- )
		{
		    *--real_out = *--byte_in;
		}
	    }

	    else if ( form_out == NFMCOM )
	    {
		/*  Convert byte to complex  */

                byte_in     = (F_byte_array)    ARRAY_IN  + n;
                complex_out = (F_complex_array) ARRAY_OUT + n;

                (*complex_out).re = *byte_in;
                (*complex_out).im = 0.0;

		for ( i = n; i > 0; i-- )
		{
                    (*--complex_out).re = *--byte_in;
                    (*  complex_out).im = 0.0;
		}
	    }
	}

	else if ( form_in == NFMINT )
	{
	    if ( form_out == NFMBYT )
	    {
		/*  Convert integer to byte  */

         	int_in   = (F_integer_array) ARRAY_IN;
         	byte_out = (F_byte_array)    ARRAY_OUT;

		for ( i = n; i > 0; i-- )
		{
		    *byte_out++ = *int_in++;
		}

                *byte_out = *int_in;
	    }

	    else if ( form_out == NFMFP )
	    {
		/*  Convert integer to real  */

         	int_in   = (F_integer_array) ARRAY_IN  + n;
        	real_out = (F_real_array)    ARRAY_OUT + n;

                *real_out = *int_in;

		for ( i = n; i > 0; i-- )
		{
		    *--real_out = *--int_in;
		}
	    }

	    else if ( form_out == NFMCOM )
	    {
		/*  Convert integer to complex  */

         	int_in      = (F_integer_array) ARRAY_IN  + n;
                complex_out = (F_complex_array) ARRAY_OUT + n;

                (*complex_out).re = *int_in;
                (*complex_out).im = 0.0;

		for ( i = n; i > 0; i-- )
		{
                    (*--complex_out).re = *--int_in;
                    (*  complex_out).im = 0.0;
		}
	    }
	}

	else if ( form_in == NFMFP )
	{
	    if ( form_out == NFMBYT )
	    {
		/*  Convert real to byte  */

        	real_in  = (F_real_array) ARRAY_IN;
         	byte_out = (F_byte_array) ARRAY_OUT;

		for ( i = n; i > 0; i-- )
		{
		    *byte_out++ = *real_in++;
		}

                *byte_out = *real_in;
	    }

	    else if ( form_out == NFMINT )
	    {
		/*  Convert real to integer  */

        	real_in = (F_real_array)    ARRAY_IN;
        	int_out = (F_integer_array) ARRAY_OUT;

		for ( i = n; i > 0; i-- )
		{
		    *int_out++ = *real_in++;
		}

                *int_out = *real_in;
	    }

	    else if ( form_out == NFMCOM )
	    {
		/*  Convert real to complex   */

        	real_in     = (F_real_array)    ARRAY_IN  + n;
                complex_out = (F_complex_array) ARRAY_OUT + n;

                (*complex_out).re = *real_in;
                (*complex_out).im = 0.0;

		for ( i = n; i > 0; i-- )
		{
                    (*--complex_out).re = *--real_in;
                    (*  complex_out).im = 0.0;
		}
	    }
	}

	else if ( form_in == NFMCOM )
	{
	    if ( form_out == NFMBYT )
	    {
		/*  Convert complex to byte  */

                complex_in = (F_complex_array) ARRAY_IN;
         	byte_out   = (F_byte_array)    ARRAY_OUT;

		for ( i = n; i > 0; i-- )
		{
                    *byte_out++ = (*complex_in++).re;
		}

                *byte_out = (*complex_in).re;
	    }

	    else if ( form_out == NFMINT )
	    {
		/*  Convert complex to integer  */

                complex_in = (F_complex_array) ARRAY_IN;
        	int_out    = (F_integer_array) ARRAY_OUT;

		for ( i = n; i > 0; i-- )
		{
		    *int_out++ = (*complex_in++).re;
		}

                *int_out = (*complex_in).re;
	    }

	    else if ( form_out == NFMFP )
	    {
		/*  Convert complex to real  */

                complex_in = (F_complex_array) ARRAY_IN;
        	real_out   = (F_real_array)    ARRAY_OUT;

		for ( i = n; i > 0; i-- )
		{
                    *real_out++ = (*complex_in++).re;
		}

                *real_out = (*complex_in).re;
	    }
        }
    }

/*  Copyright (C) Synoptics 1988-1992: All Rights Reserved  */
}

/*  Figment/Semper 6 primitive cfpcom - convert byte to complex with holes
 *
 *  Must be able to convert in place, so loop directions are important.
 *
 *  Coded Mark Raisbeck 13/SEP/1988 based on cform
 *  Mods. Mark Raisbeck 23/NOV/1989
 *  Mods. Frank Suess   19/AUG/1992, 25/SEP/1992
 */

subroutine cfpcom_ ( ARRAY_IN, ARRAY_OUT, N )

F_array   ARRAY_IN;
F_array   ARRAY_OUT;
F_integer *N;
{
    F_byte_array    byte_in;
    F_complex_array complex_out;
    register int i, n;

    /*  Set up number of times round conversion loop          */
    /*  Note:  first or last value is converted outside loop  */

    n = *N - 1;

    /*  Set up pointers for input and output arrays  */

    byte_in     = (F_byte_array)    ARRAY_IN  + n;
    complex_out = (F_complex_array) ARRAY_OUT + n;

    /*  Convert byte to complex (real part only)  */

    (*complex_out).re = *byte_in;

    for ( i = n; i > 0; i-- )
    {
        (*--complex_out).re = *--byte_in;
    }

/*  Copyright (C) Synoptics 1988-1992: All Rights Reserved  */
}
subroutine cform ( ARRAY_IN, ARRAY_OUT, FORM_IN, FORM_OUT, N )

F_array       ARRAY_IN;
F_array       ARRAY_OUT;
F_integer     *FORM_IN;
F_integer     *FORM_OUT;
F_longinteger *N;

{
       cform_ ( ARRAY_IN, ARRAY_OUT, FORM_IN, FORM_OUT, N );
}
subroutine cfpcom ( ARRAY_IN, ARRAY_OUT, N )

F_array   ARRAY_IN;
F_array   ARRAY_OUT;
F_integer *N;
{
        cfpcom_ ( ARRAY_IN, ARRAY_OUT, N );
}

subroutine cformi ( ARRAY_IN, ARRAY_OUT, FORM_IN, FORM_OUT, N )

F_integer_array ARRAY_IN;
F_integer_array ARRAY_OUT;  
F_integer       *FORM_IN;
F_integer       *FORM_OUT;
F_longinteger   *N;

{
    cform_ ( (F_array)ARRAY_IN, (F_array)ARRAY_OUT, FORM_IN, FORM_OUT, N );
}