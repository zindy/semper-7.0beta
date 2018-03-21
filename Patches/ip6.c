/*
 IP VME base address access routines

 This routine, tegether with the routines in ipbase.c is used to
 obtain the appropriate Kernel Virtual base addresses for the A16, A24
 and A32 Non-Privileged address spaces on an IP6 processor

 Author:  Mark Raisbeck 15th February 1990
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/IP6.h>

long int ip6_base ( sec )
int sec;
{

/* sec is the required VME section (16, 24 or 32) */

    switch (sec)
    {
    case 16:
	return (VME_A16NPBASE);

    case 24:
	return (VME_A24NPBASE);

    case 32:
	return (VME_A32NPBASE);
    }
}
