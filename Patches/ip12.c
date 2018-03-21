/*
 IP VME base address access routines

 This routine, tegether with the routines in ipbase.c is used to
 obtain the appropriate Kernel Virtual base addresses for the A16, A24
 and A32 Non-Privileged address spaces on an IP12 processor

 Author:  Mark Raisbeck 2nd July 1991
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/IP12.h>

long int ip12_base ( sec )
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
