/*
 IP VME base address access routines

 These routines, and the routines in ip4.c, ip5.c etc. are used to
 obtain the appropriate Kernel Virtual base addresses for the A16, A24
 and A32 Non-Privileged address spaces

 Author:        Mark Raisbeck 15th February 1990
 Modified:      Mark Raisbeck  5th October 1990 (IP7)
                Mark Raisbeck  2nd July 1991 (IP12)
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/immu.h>
#include <invent.h>
#define phystokv1(x)    ((x) | K1SEG)

int ip_number ( )
{
    inventory_t *inv;
    int what;
    char buffer[256];

    while ((inv = getinvent ( )) != NULL)
    {
        if (inv->class==INV_PROCESSOR)
        {
            if (inv->type==INV_CPUBOARD)
            {
                switch (inv->state)
                {
                case INV_IP4BOARD:
                    what = 4;break;

                case INV_IP5BOARD:
                    what = 5;break;

                case INV_IP6BOARD:
                    what = 6;break;

                case INV_IP7BOARD:
                    what = 7;break;

                case INV_IP9BOARD:
                    what = 9;break;

                case INV_IP12BOARD:
                    what = 12;break;

                case INV_IP17BOARD:
                    what = 17;break;

                default:
                    sprintf(buffer,"\nUnknown processor type %d\n",inv->state);
                    csemtou ( buffer );
                    what = 0;break;
                }
                endinvent ( );
                return ( what );

            }
        }
    }
    csemtou ("Cannot determine processor type\n");
    endinvent ( );
    return ( 0 );

/*  Copyright (C) 1990,1991:  Synoptics Ltd,  All Rights Reserved  */
}


long int vme_base ( ip, sec )
int ip;
int sec;
{
    int ip4_base(), ip5_base(), ip6_base(), ip12_base();
    char buffer[256];

/* ip is the IP number, sec is the required VME section (16, 24 or 32) */

    if ((sec==16) || (sec==24) || (sec==32))
    {
        switch (ip)
        {
        case 4:
            return (ip4_base( sec ));

        case 5: case 7: case 9: case 17:
            return (ip5_base( sec ));

        case 6:
            return (ip6_base( sec ));

        case 12:
            return (ip12_base( sec ));

        }

        sprintf(buffer,"\nIP base not set or unknown:IP%d\n",ip);
        csemtou(buffer);
        return (-1);
    }
    else
    {
        sprintf(buffer,"Unknown VME section requested:A%d\n",sec);
        csemtou(buffer);
        return (-1);
    }

/*  Copyright (C) 1990,1991:  Synoptics Ltd,  All Rights Reserved  */
}

int get_bases(a16,a24,a32)
long int *a16,*a24,*a32;
{
    int ip;
    long int base;

    ip = ip_number();

    if ((base = vme_base ( ip, 16 ))==-1) return ( 0 );
    *a16 = phystokv1 ( base );

    if ((base = vme_base ( ip, 24 ))==-1) return ( 0 );
    *a24 = phystokv1 ( base );

    if ((base = vme_base ( ip, 32 ))==-1) return ( 0 );
    *a32 = phystokv1 ( base );
    return ( 1 );

/*  Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved  */
}
