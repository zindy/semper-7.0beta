/*
     Semper main loop and special condition handlers

     Copyright (C) : Synoptics Ltd 1989-1996

     Author:   Mark Raisbeck  08/December/1989
     Modifier: Mark Raisbeck  04/January/1990     SUN system
	       Mark Raisbeck  28/June/1991        MYRIAD system
	       Mark Raisbeck  18/July/1991        MYRIAD problems
	       Mark Raisbeck  27/January/1992     APPLE port
	       Mark Raisbeck  03/September/1992   WINDOWS port
	       Mark Raisbeck  10/May/1993         Fixes for new SG port
	       Mark Raisbeck  25/August/1993      MYRIAD security
	       Mark Raisbeck  28/January/1994     Use BBmess in WINDOWS
	       Frank Suess    19/May/1995         Solaris 2.4 port
               Mark Raisbeck  4/June/1996         MYRIAD wait sensitivity
               LDM            July 2005           Autoconf compatibility
*/

#include <sys/types.h>
#include <signal.h>
#include <setjmp.h>
#include <stdio.h>
#include "ftypes.h"

/* Compatible Fortran prototypes */
#define semini_F77 F77_FUNC(semini,SEMINI)
#define semain_F77 F77_FUNC(semain,SEMAIN)

static int break_count = 0;             /*  Number of breaks read   */
static unsigned break_active = 0;       /*  Break queue opened      */

static int division_count = 0;
static int overflow_count = 0;
static int invalid_count = 0;
static int underflow_count = 0;
static int math_count = 0;

static jmp_buf sem_env;

subroutine semhan_()
{
    void afperr ( void );

/*
	Set up non-returning signal catchers to longjmp to the
	stored environment
*/

    if ( setjmp ( sem_env ) )
    {
	(void) printf("Unexpected fatal signal during initialisation\n");
    }
    else
    {

	    /* Setup signal handler for FP exceptions */

	signal ( SIGFPE, afperr );
/*    	semini_ (); */
        semini_F77 ();
	do
	{
	    /* Setup signal handler for FP exceptions */

	    signal ( SIGFPE, afperr );
	    if ( setjmp ( sem_env ) )
	    {
/*
	Signal return
*/
	    }
/*	    else semain_ (); */
            else semain_F77 ();
	} while (1);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXBO
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Opens the break queue.  Sets up the break signal handling function.
 *
 *----------------------------------------------------------------------
 */

subroutine eqxxbo_ ( )

{
    void abreak ( void );               /*  Break handling function  */

    /*   Zero the break count  */

    break_count = 0;
    break_active = 1;

    /*  Set up the signal handler  */

    signal ( SIGINT, abreak );
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXBC
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Closes the break queue.  Unsets the signal break handler
 *
 *----------------------------------------------------------------------
 */

subroutine eqxxbc_ ( )

{
    /*  Zero the break count  */

    break_count = 0;
    break_active = 0;

    /*  Unset the signal handler  */

    signal ( SIGINT, SIG_DFL );
}
subroutine eqxxbc ( )

{
        eqxxbc_ ( );
}
/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXZB
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Zeros the break count.
 *
 *----------------------------------------------------------------------
 */

subroutine eqxxzb_ ( )

{
    /*  Zero the break count  */

    break_count = 0;
}

#define MAXBREAKS 16

/*
 *----------------------------------------------------------------------
 *
 *      signal handler abreak ( )
 *      -------------------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Break handling function.  If maximum number of breaks not
 *      exceeded, queues a break.  Otherwise, writes a message, and
 *      exits.
 *
 *----------------------------------------------------------------------
 */


void abreak ( void )
{
    /*  Ignore any extra breaks during this routine   */

    signal ( SIGINT, SIG_IGN );

    /*  Increment break count, and if too many breaks, exit. Otherwise,
     *  queue a break  */

    break_count++;
#define eqxxkc_F77 F77_FUNC(eqxxkc,EQXXKC)                                                                               
#define eqxxbc_F77 F77_FUNC(eqxxbc,EQXXBC)                                                                               
#define semend_F77 F77_FUNC(semend,SEMEND)  
#define semcon_F77 F77_FUNC(semcon,SEMCON)
    if ( break_count == MAXBREAKS )
    {
	/*  Give the poor user a way out!  Exit  */

	break_active = 0;
	(void) semcon_F77 ( "Semper 6 plus: too many breaks", (long) 30 );
	eqxxkc_F77 ();
	eqxxbc_F77 ();
	semend_F77 ();
    }
    else
    {
#define eqxxbe_F77 F77_FUNC(eqxxbe,EQXXBE)
	eqxxbe_F77 ( );
    }

    /*  Restore action */

    if ( break_active != 0 )
	signal ( SIGINT, abreak );
    else
	signal ( SIGINT, SIG_DFL );


}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION ABMATH ( IER )
 *      -------------------------------
 *
 *      PARAMETERS:
 *
 *           integer ier : INPUT - error number
 *
 *
 *----------------------------------------------------------------------
 */

logicalfunction abmath_ ( ier )
F_integer *ier;
{
    F_logical status;


    /*  Read and zero the error count(s)  */

    status = F_TRUE;

    if ( invalid_count != 0 )
    {
	*ier = (F_integer) 92;
	invalid_count = 0;
    }
    else if ( overflow_count != 0 )
    {
	*ier = (F_integer) 93;
	overflow_count = 0;
    }
    else if ( division_count != 0 )
    {
	*ier = (F_integer) 94;
	division_count = 0;
    }
    else if ( underflow_count != 0 )
    {
	*ier = (F_integer) 91;
	underflow_count = 0;
    }
    else status = F_FALSE;

    if ( math_count != 0 )
    {
	math_count = 0;
/*
	If no specific errors found then return general error
	(Could improve on this later)
*/
	if ( status == F_FALSE )
	{
	    *ier = (F_integer) 92;
	    status = F_TRUE;
	}
    }

    return (status);
}

logicalfunction abmath ( ier )
F_integer *ier;
{
        return abmath_ (ier);
}

/*
     Max number of FP errors to ignore before automatic quit.
     Because of the way the signal mechanism works, this value
     does not need to be very big, but should be at least 3
*/

#define MAXFPERRS 4
/*
 *----------------------------------------------------------------------
 *
 *      void fpexit
 *      -----------
 *
 *      Called when FP errors are being ignored !
 *
 *----------------------------------------------------------------------
 */

subroutine fpexit()

{
    /*  Give the poor user a way out!  Exit  */

    (void) semcon_F77 ( "Semper 6 plus: too many FP errors", (long) 33 );
    eqxxkc_F77 ();
    eqxxbc_F77 ();
    semend_F77 ();
}

/*
 *----------------------------------------------------------------------
 *
 *      signal handler afperr ( )
 *      -------------------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      FP exception handler.
 *
 *----------------------------------------------------------------------
 */

void afperr ( void )
{

    /*  Ignore any extra errors during this routine   */

    signal ( SIGFPE, SIG_IGN );

    /*  Increment math count  */

    math_count++;

    /*
	If maths errors appear to be being ignored then we are probably
	stuck in an infinite loop - so exit with some grace
    */

    if ( math_count >= MAXFPERRS ) fpexit();

    /*  Restore action and return to head of program */

    signal ( SIGFPE, afperr );

    longjmp ( sem_env, 1 );

}

subroutine semhan ()
{
        semhan_();
}
subroutine eqxxbo ( )

{
        eqxxbo_ ( ) ;
}
subroutine eqxxzb ( )

{
        eqxxzb_ ( );
}

