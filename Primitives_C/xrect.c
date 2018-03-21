/*
 * X Version 11 / DEC windows port - C level Semper support
 *				     Rectangle mapping
 */

/*
 *  All routines in this file are protected by copyright
 *
 *  Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
 *
 */

#include <X11/Xlib.h>

#include <stdio.h>
#include <ctype.h>
#include "ftypes.h"

#define	draw_virt	xsdvir

/*
    These routines maintain a coarse grid of areas of the frame
    that may need updating.
*/

#define shiftr (7)
#define max_dim (128)
/* max_dim is 2 to the power shiftr */

#define arr_dim (max_dim * max_dim)

/*
    touched holds the grid of touched areas
*/

static Bool touched[arr_dim];

/*
    x_size and y_size are the full frame size, x_scale and y_scale are
    the shift factors required to convert grid to frame co-ordinates
*/

static int x_size, y_size, x_scale, y_scale;

/*
    x_min etc. are used to hold the boundaries of all of the touched areas
*/

static int x_min, x_max, y_min, y_max;


/*
    sxrclr resets the touched grid and boundary record
*/

void sxrclr()
{
    long int index;
    Bool *ptr;

    ptr = touched;
    for (index=arr_dim;index;index--) *ptr++ = False;
    x_max = -1;
    y_max = -1;
    x_min = max_dim + 1;
    y_min = max_dim + 1;
}

/*
    sxscal calculates the shift value from the frame size
*/

int sxscal(i)
int i;
{
    int res;

    if (i <= 1024) res = 3;
    else if (i <= 2048) res = 4;
    else if (i <= 4096) res = 5;
    else if (i <= 8192) res = 6;
    else if (i <= 16384) res = 7;
    else res = 8;

    return(res);
}

/*
    sxrsiz is called to define the frame size (in fsas61) and initialise
	   the stored data
*/

void sxrsiz(x,y)
int x,y;
{
    x_size = x;		x_scale = sxscal(x);
    y_size = y;		y_scale = sxscal(y);
    (void) sxrclr();
}

/*
    sxrupd returns the best update rate for image displays
*/

int sxrupd()
{
    if (x_scale > y_scale) return(1 << x_scale);
    else		   return(1 << y_scale);
}

/*
    sxrins inserts a rectangle in frame co-ordinates into the grid.
	   If the new positions are outside of the current boundaries
	   the boundaries are updated
*/

void sxrins(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
    int x1a,y1a,x2a,y2a, indx,indy;
    long int offset;
    Bool *ptr;

    x1a = x1 >> x_scale;
    x2a = x2 >> x_scale;
    y1a = y1 >> y_scale;
    y2a = y2 >> y_scale;

/*
    Make x1a,y1a be the top left co-ordinate and x2a,y2a the bottom right
*/

    if (x2a < x1a)
    {
	indx = x1a;
	x1a = x2a;
	x2a = indx;
    };
    if (y2a < y1a)
    {
	indy = y1a;
	y1a = y2a;
	y2a = indy;
    };

/*
    Check boundary limits
*/
    if (x1a < x_min) x_min = x1a;
    if (x2a > x_max) x_max = x2a;
    if (y1a < y_min) y_min = y1a;
    if (y2a > y_max) y_max = y2a;

/*
    Fill the relevant part of the grid
*/
    for (indy=y1a;indy<=y2a;indy++)
    {
	offset = (indy << shiftr) + x1a;
	ptr = &touched[offset];
	for (indx=x1a;indx<=x2a;indx++) *ptr++ = True;
    }

}

/*
    sxrdrw is used to repaint part of the screen. It is supplied with a
	   horizontal strip to update. If the next strips down are also
	   fairly full they are repainted as well. This technique reduces
	   some of the repaint overhead.
*/

void sxrdrw(x,y,w)
int x,y,w;
{
    long int offset;
    Bool *ptr, *again, more;
    int failed;

    int h, indx,indy;

    h = 0;

/*
    First project this strip downwards if possible - this should work
    for the first strip at least!
*/
    indy = y;
    more = True;
    while (more && (indy <= y_max))
    {
/*
	Count the number of clear areas in failed
*/
	failed = 0;
	offset = (indy << shiftr) + x;
	ptr = &touched[offset];
	again = ptr;
	if (indy > y)
	{
	    for (indx=0;indx<w;indx++)
	    {
		if (!*ptr++) failed++;
	    }
	};

/*
	Merge if one quarter or less is unset
*/
	if (((w+1)>>2) >= failed)
	{
	    indy++;
	    h = h + 1;
	    ptr = again;
/*
	    Now clear this area
*/
	    for (indx=0;indx<w;indx++) *ptr++ = False;
	}
	else more = False;
    };

/*
    Convert to frame co-ordinates
*/
    x <<= x_scale;
    w <<= x_scale;
    y <<= y_scale;
    h <<= y_scale;
    if ((x+w-1) > x_size) w = x_size - x + 1;
    if ((y+h-1) > y_size) h = y_size - y + 1;

/*
    and request a draw!
*/
    (void) draw_virt(x,y,w,h);
}

/*
    sxrout scans the grid for horizontal strips that need updating
*/

void sxrout()
{
    int indx, indy, startx, sizex;

    long int offset;
    Bool *ptr, seen;

    sizex = 1;
    if (x_max != -1)
    {
	for (indy=y_min;indy<=y_max;indy++)
	{
	    offset = (indy << shiftr) + x_min;
	    ptr = &touched[offset];
	    seen = False;
	    for (indx=x_min;indx<=x_max;indx++,ptr++)
	    {
		if (*ptr)
		{
		    if (seen) sizex++;
		    else
		    {
			seen = True;
			startx = indx;
			sizex = 1;
		    }
		    *ptr = False;
		}
		else
		{
		    if (seen)
		    {
			sxrdrw(startx,indy,sizex);
			seen = False;
		    }
		}
	    };
	    if (seen) sxrdrw(startx,indy,sizex);
	}
    };
/*
    Restart counters as everything has been drawn
*/
    x_max = -1;
    y_max = -1;
    x_min = max_dim + 1;
    y_min = max_dim + 1;
}
