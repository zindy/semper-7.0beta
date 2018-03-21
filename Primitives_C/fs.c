/*
 * X Version 11 - C level Semper support
 */

/*
 *  All routines in this file are protected by copyright
 *
 *  Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
 *
 */

#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>



/*  These include files are used for interfacing to FORTRAN code  */

#include "ftypes.h"
#include "params.h"
#include "icset.h"
#include "events.h"
#define semuln_F77 F77_FUNC(semuln,SEMULN)
#define semdln_F77 F77_FUNC(semdln,SEMDLN)
#define semend_F77 F77_FUNC(semend,SEMEND)
#define eqxxke_F77 F77_FUNC(eqxxke,EQXXKE)
#define fsx11m_F77 F77_FUNC(fsx11m,FSX11M)
#define uixxrm_F77 F77_FUNC(uixxrm,UIXXRM)
#define eqxxpe_F77 F77_FUNC(eqxxpe,EQXXPE)
#define eqxxke_F77 F77_FUNC(eqxxke,EQXXKE)
#define eqxxse_F77 F77_FUNC(eqxxse,EQXXSE)
#define fsx11p_F77 F77_FUNC(fsx11p,FSX11P)

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

typedef long int medium;
typedef unsigned long int u_medium;


/****************** X WINDOW STRUCTURES *******************/

/*
  All declared static for now - remove 'static' if using seperate
  compilation at a later date
*/

static char termDisplayName[256];
/*
static char frameDisplayName[256];
static char menuDisplayName[256];
*/

/*
  Fixed font names for now - could put in user override at assign later?
*/

static char *termFontName1 = "vtsingle";
static char *termFontName2 = "-DEC-Terminal-Medium-*-Normal-*-iso*";

static char *menuFontName1 = "vtsingle";
static char *menuFontName2 = "-DEC-Terminal-Medium-*-Normal-*-iso*";

static char *frameFontName1 = "9x15";
static char *frameFontName2 = "-DEC-Terminal-Medium-*-Normal-*-iso*";

static Display *termDisplay;
/* static Display *frameDisplay, *menuDisplay; */
static XFontStruct *termFont,*frameFont,*menuFont;

/* Font metrics converted to useful values */

static int termXleft,  termYupper,  termCharWidth,  termCharHeight;
static int frameXleft, frameYupper, frameCharWidth, frameCharHeight;

static int menuXleft, menuYupper, menuCharWidth, menuCharHeight;

/* Size of view frames */

static int termWidth,  termHeight;
static int frameWidth, frameHeight;
static int menuWidth, menuHeight;

/* Cursors definitions we will use */

static Cursor termCursor  = NULL;
static Cursor arrowCursor = NULL;
static Cursor blankCursor = NULL;

static int termScreen;
/* static int frameScreen, menuScreen;  */

static u_medium termMask =
	ButtonPressMask | ButtonReleaseMask | KeyPressMask |
	KeymapStateMask | ExposureMask | StructureNotifyMask |
	EnterWindowMask | LeaveWindowMask |
	PointerMotionMask | PointerMotionHintMask;
static u_medium frameMask =
	ButtonPressMask | ButtonReleaseMask | KeyPressMask |
	KeymapStateMask | ExposureMask | StructureNotifyMask |
	EnterWindowMask | LeaveWindowMask |
	PointerMotionMask | PointerMotionHintMask;
static u_medium menuMask =
	ButtonPressMask | ButtonReleaseMask | KeyPressMask |
	KeymapStateMask | ExposureMask | StructureNotifyMask |
	EnterWindowMask | LeaveWindowMask |
	PointerMotionMask | PointerMotionHintMask;

static Window termWin, frameWin, menuWin;
/*
static Window termIconWin, frameIconWin, menuIconWin;
*/

static XSetWindowAttributes termAttributes, frameAttributes, menuAttributes;

static GC foreGC, backGC, menuGC, frameGC, pixmapGC;

static Bool termIsIcon,  termExpose,  termInFocus;
static Bool frameIsIcon, frameExpose, frameInFocus, frameIgnore;
static Bool menuIsIcon,  menuExpose,  menuInFocus;

static int termLastX, termLastY;
static int frameLastX, frameLastY;
static int menuLastX, menuLastY;

static Bool warpingNow = False;

static XEvent report;

static Bool openedDisp  = False;

static Bool termMade  = False;
static Bool frameMade = False;
static Bool menuMade = False;
static Bool menuLoaded = False;

static Bool termDying  = False;
static Bool frameDying = False;
static Bool menuDying = False;

static F_integer frameLUT = 1;

static unsigned char XimRed   [LUTNUM] [256];
static unsigned char XimGreen [LUTNUM] [256];
static unsigned char XimBlue  [LUTNUM] [256];

/* Colour cell translation map */

static unsigned short XimCell[256];

static int cellCount = 256;

static unsigned short menuMap[8];

static char *menuColourName[8] = {
				   "black",
				   "white",
				   "red",
				   "green",
				   "blue",
				   "cyan",
				   "magenta",
				   "yellow"
				 };

static int menuColourCount = 0;

static unsigned long redMap[256];
static unsigned long greenMap[256];
static unsigned long blueMap[256];

static XVisualInfo termVisual, frameVisual, menuVisual;

static XVisualInfo defaultVisual;
static XVisualInfo grayScaleVisual, pseudoColorVisual, trueColorVisual;

static Colormap defaultColormap, termColormap, menuColormap, frameColormap;

static Colormap grayScalemap   = NULL;
static Colormap pseudoColormap = NULL;
static Colormap trueColormap   = NULL;

static int frameTrueColorRedShift;
static int frameTrueColorGreenShift;
static int frameTrueColorBlueShift;
static int frameImageMemoryBits;

static unsigned short XovCell[8];
static unsigned short XovMap[256];
static unsigned long  XovRGB[256];

static unsigned char XovVisibilityMask;

static unsigned char XovRed   [ 8 ];
static unsigned char XovGreen [ 8 ];
static unsigned char XovBlue  [ 8 ];

static unsigned char XovInitRed   [ 8 ] = { 255,255,  0,  0,  0,255,255,255 };
static unsigned char XovInitGreen [ 8 ] = { 255,  0,255,  0,255,  0,255,  0 };
static unsigned char XovInitBlue  [ 8 ] = { 255,  0,  0,255,255,255,  0,  0 };

static int XovGraphics;
static int XovRubberband;
static int XovCursor;

static unsigned char XovBitMask [ 8 ] = { 1, 2, 4, 8, 16, 32, 64, 128 };

/* maximum pixel value */

static unsigned char Xpixmax;

/* Input lookup tables from virtual framestore */
/*     Declared 256 in case of garbage input   */

static unsigned char bytMap[256];
static F_integer     intMap[256];
static F_real         fpMap[256];

/* Range of above */

static F_real bytMin, bytMax, intMin, intMax, fpMin, fpMax;

/* and validity */

static Bool bytValid = False;
static Bool intValid = False;
static Bool  fpValid = False;

static XSizeHints termSizeHints;
static XSizeHints frameSizeHints;
static XSizeHints menuSizeHints;

static XWMHints termWMHints, frameWMHints, menuWMHints;

/*
   Size of maximum terminal screen
*/

#define chars_per_line   (120)
#define lines_per_page   (60)

#define init_width   (80)
#define init_height  (25)

#define chars_per_page   (chars_per_line * lines_per_page)

/* Special characters */

#define ch_botr      (11)
#define ch_topr      (12)
#define ch_topl      (13)
#define ch_botl      (14)
#define ch_hor       (18)
#define ch_ver       (25)

/* Screen pointers */

static int line_pointers[lines_per_page];
static int menu_pointers[lines_per_page];

/* Screen character and attribute maps */

static unsigned char term_screen_map[chars_per_page];
static unsigned char term_screen_att[chars_per_page];

/* Menu character and attribute maps */

static unsigned char menu_screen_map[chars_per_page];
static unsigned char menu_screen_att[chars_per_page];

static Bool termVideo;
static Bool termCurse;

static int termXpos = 0;
static int termYpos = 0;

static int menuXpos = 0;
static int menuYpos = 0;

static int menuFgc = 1;
static int menuBgc = 0;
static int menuAtt = 1;         /* fgc + bgc << 4*/

/*  Offset of display view screen from virtual frame  */

static int frameXOffset = 0;
static int frameYOffset = 0;

/* Size of virtual frame */

static int frameVirtWidth = 768;
static int frameVirtHeight = 512;

static int frameCount = 0;

/* Pointer to frame buffer memory maps */

static unsigned char *frameBuffer [ 3 ] = { NULL, NULL, NULL };
static unsigned char *XovBuffer = NULL;

static Pixmap charPixmap;
static unsigned char *charBuffer = NULL;

#define sps (chars_per_line)

static unsigned char space[sps];

static char message[256];

/* Frame buffer update variables */

static Bool frameStale;
static int frameCountI, frameLimitI, frameCountV, frameLimitV;

/* Semper Cursor control stuff */

static Bool fsximm = False;
static Bool fsxsee = False;
static Bool fsxvis = False;
static int  fsxsct = 1;
static int  fsxcrx, fsxcry;

/* Blank cursor bitmap data */

static char blank_bits[] =
	{
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000,
		0x0000, 0x0000, 0x0000, 0x0000
	};

static Pixmap blankMap;


/*
   Redefinitions to prevent longer names in symbol maps
*/

#define      wait_event      xswevt
#define      show_cursor     xsshcu
#define      hide_cursor     xshicu
#define      draw_term       xsdter
#define      draw_menu       xsdmen
#define      draw_note       xsdnot
#define      draw_disp       xsddis
#define      draw_virt       xsdvir
#define      move_virt       xsmvir
#define      draw_string     xsdstr
#define      menu_string     xsmstr
#define      scroll_term     xsster
#define      show_message    xsshme
#define      force_update    xsfupd
#define      queue_string    xsqstr
#define      note_destroyed  xsndes
#define      note_unknown    xsnunk
#define      write_lut       xswrlt
#define      mapMenu         xsmmen
#define      unmapMenu       xsumen

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE WAIT_EVENT( )
 *       ------------------------
 *
 *  Wait for the next X11 event and then handle it
 *
 *----------------------------------------------------------------------
 */

void wait_event()
{
    void sx11ce_();

    (void) XPeekEvent(termDisplay, &report);
    sx11ce_();
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SHOW_CURSOR( )
 *       -------------------------
 *
 *  Show terminal cursor
 *
 *----------------------------------------------------------------------
 */

void show_cursor()
{
    int x, y, l;
    unsigned char *ptr,*att;

    x = (termXpos * termCharWidth) + termXleft;
    y = (termYpos * termCharHeight) + termYupper;
    l = line_pointers[termYpos];
    ptr = &term_screen_map[l];
    ptr += termXpos;
    att = &term_screen_att[l];
    att += termXpos;
    if (*att)
	(void) XDrawImageString(termDisplay, termWin, backGC,
				x, y, ptr, 1);
    else
	(void) XDrawImageString(termDisplay, termWin, foreGC,
				x, y, ptr, 1);
    termCurse = True;
    (void) XFlush(termDisplay);
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE DRAW_TERM( X,Y, W,H )
 *       --------------------------------
 *
 *  Redraw area of screen positioned at x,y size w,h
 *
 *----------------------------------------------------------------------
 */

void draw_term ( x,y, w,h )
int x,y, w,h;
{
    unsigned char *ptr, *att;
    unsigned char k;
    int scr_x,scr_y;
    int bln_x;
/*    int bln_y; */
    int lineno,i,j,ic,wl;

    if ( termIsIcon )
    {
/*      Ignore while unmapped   */
    }
    else
    {

/*
   Align to bounding character box  -  modify size as well
   and convert to character positions
*/

	ic    = x / termCharWidth;
	scr_x = ic * termCharWidth;
	w    += x - scr_x;
	x     = ic;
	w     = (w + termCharWidth - 1) / termCharWidth;

	ic    = y / termCharHeight;
	scr_y = ic * termCharHeight;
	h    += y - scr_y;
	y     = ic;
	h     = (h + termCharHeight - 1) / termCharHeight;

/*
	Modify start position for font metrics
*/
	scr_x += termXleft;
	scr_y += termYupper;

/*
	Any exposed areas off our mapped screen ?
*/
	if ((y+h) >= lines_per_page)
	{
/*
	Blank excess lines at bottom

	    for (i = lines_per_page; i < (y+h); i++ )
	    {
		bln_y = (i * termCharHeight) + termYupper;
		bln_x = scr_x;
		for (j = 0; j < w; j=j+sps)
		{
		    if ((j + sps) <= w) ic = sps;
		    else ic = w - j;
		    (void) XDrawImageString(termDisplay, termWin, foreGC,
					    bln_x, bln_y, space, ic);
		    bln_x += (termCharWidth * ic);
		}
	    };

	Reset height of area to draw
*/
	    h = lines_per_page - y;
	};

	if ((x+w) >= chars_per_line)
	{
/*
	Blank excess area to right of display

	    for (i = y; i < (y+h); i++ )
	    {
		bln_y = (i * termCharHeight) + termYupper;
		bln_x = (chars_per_line * termCharWidth) + termXleft;;
		for (j = chars_per_line; j < (x+w); j=j+sps)
		{
		    if ((j + sps) <= (x + w)) ic = sps;
		    else ic = x + w - j;
		    (void) XDrawImageString(termDisplay, termWin, foreGC,
					    bln_x, bln_y, space, ic);
		    bln_x += (termCharWidth * ic);
		}
	    };

	Reset width of area to draw
*/
	    w = chars_per_line - x;
	};

	lineno = y;
/*
	For all lines exposed
*/
	if (w > 0)
	{
	    for (i = 0; i < h; i++,lineno++ )
	    {
/*
	Redraw line section
*/
		ic   = line_pointers[lineno];
		ptr  = &term_screen_map[ic];
		ptr += x;
		att  = &term_screen_att[ic];
		att += x;
		bln_x = scr_x;
		wl = w;
/*
		Locate characters in same video
*/
		do
		{
		    j = 1;
		    k = *att++;
		    while ((*att == k) && j < wl)
		    {
			j++;
			att++;
		    };
		    if ( k )
			(void) XDrawImageString(termDisplay, termWin, foreGC,
						bln_x, scr_y, ptr, j);
		    else
			(void) XDrawImageString(termDisplay, termWin, backGC,
						bln_x, scr_y, ptr, j);
/*
		Advance pointer along line
*/
		    ptr += j;
		    bln_x += (j * termCharWidth);
		    wl -= j;
		} while ( wl > 0 );
/*
		Advance for next line
*/
		scr_y += termCharHeight;
	    }
	}
	if ( termCurse ) show_cursor();
	else (void) XFlush(termDisplay);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE DRAW_MENU( X,Y, W,H )
 *       --------------------------------
 *
 *  Redraw area of menu positioned at x,y size w,h
 *
 *----------------------------------------------------------------------
 */

void draw_menu ( x,y, w,h )
int x,y, w,h;
{
    unsigned char *ptr, *att;
    unsigned char k, n, ch, buff[chars_per_line];
    int scr_x,scr_y,buff_c;
    int bln_x, x1,x2,x3, y1,y2,y3;
/*    int bln_y; */
    int lineno,i,j,ic,mc,wl;

    if ( menuIsIcon )
    {
/*      Ignore while unmapped   */
    }
    else
    {

/*
   Align to bounding character box  -  modify size as well
   and convert to character positions
*/

	ic    = x / menuCharWidth;
	scr_x = ic * menuCharWidth;
	w    += x - scr_x;
	x     = ic;
	w     = (w + menuCharWidth - 1) / menuCharWidth;

	ic    = y / menuCharHeight;
	scr_y = ic * menuCharHeight;
	h    += y - scr_y;
	y     = ic;
	h     = (h + menuCharHeight - 1) / menuCharHeight;

/*
	Modify start position for font metrics
*/
	scr_x += menuXleft;
	scr_y += menuYupper;

/*
	Any exposed areas off our mapped screen ?
*/
	if ((y+h) >= lines_per_page)
	{
/*

	    (void) XSetForeground(termDisplay, menuGC, menuMap[ 1 ]);
	    (void) XSetBackground(termDisplay, menuGC, menuMap[ 0 ]);

	Blank excess lines at bottom

	    for (i = lines_per_page; i < (y+h); i++ )
	    {
		bln_y = (i * menuCharHeight) + menuYupper;
		bln_x = scr_x;
		for (j = 0; j < w; j=j+sps)
		{
		    if ((j + sps) <= w) ic = sps;
		    else ic = w - j;
		    (void) XDrawImageString(termDisplay, menuWin, menuGC,
					    bln_x, bln_y, space, ic);
		    bln_x += (menuCharWidth * ic);
		}
	    }

	Reset height of area to draw
*/
	    h = lines_per_page - y;
	}

	if ((x+w) >= chars_per_line)
	{
/*

	    (void) XSetForeground(termDisplay, menuGC, menuMap[ 1 ]);
	    (void) XSetBackground(termDisplay, menuGC, menuMap[ 0 ]);

	Blank excess area to right of display

	    for (i = y; i < (y+h); i++ )
	    {
		bln_y = (i * menuCharHeight) + menuYupper;
		bln_x = (chars_per_line * menuCharWidth) + menuXleft;
		for (j = chars_per_line; j < (x+w); j=j+sps)
		{
		    if ((j + sps) <= (x + w)) ic = sps;
		    else ic = x + w - j;
		    (void) XDrawImageString(termDisplay, menuWin, menuGC,
					    bln_x, bln_y, space, ic);
		    bln_x += (menuCharWidth * ic);
		}
	    }

	Reset width of area to draw
*/
	    w = chars_per_line - x;
	}

	lineno = y;
/*
	For all lines exposed
*/
	if (w > 0)
	{
	    for (i = 0; i < h; i++,lineno++ )
	    {
/*
	Redraw line section
*/
		ic   = menu_pointers[lineno];
		ptr  = &menu_screen_map[ic];
		ptr += x;
		att  = &menu_screen_att[ic];
		att += x;
		bln_x = scr_x;
		wl = w;
/*
		Set current video
*/
		k = *att;
/*
		Set Foreground/Background using k
*/
		mc = k & 15;
		(void) XSetForeground(termDisplay, menuGC, menuMap[mc]);

		mc = (k>>4) & 15;
		(void) XSetBackground(termDisplay, menuGC, menuMap[mc]);

		buff_c = 0;
		for (j = 0;j < wl; j++)
		{
		    n = *att++;
		    if (n != k)
		    {
/*
		Attribute changed - draw character
*/
			if (buff_c > 0)
			{
			    (void) XDrawImageString(termDisplay, menuWin, menuGC,
						    bln_x, scr_y, buff, buff_c);
			    bln_x += (buff_c * menuCharWidth);
			    buff_c = 0;
			}
/*
		and change GC
*/
			k = n;
			mc = k & 15;
			(void) XSetForeground(termDisplay, menuGC, menuMap[mc]);

			mc = (k>>4) & 15;
			(void) XSetBackground(termDisplay, menuGC, menuMap[mc]);
		    }

		    ch = *ptr++;
		    if (ch < 32)
		    {
/*
		Line draw - flush buffered characters
*/
			if (buff_c > 0)
			{
			    (void) XDrawImageString(termDisplay, menuWin, menuGC,
						    bln_x, scr_y, buff, buff_c);
			    bln_x += (buff_c * menuCharWidth);
			    buff_c = 0;
			}

/*
		Draw the background to the line
*/
			buff[0] = ' ';
			(void) XDrawImageString(termDisplay, menuWin, menuGC,
						bln_x, scr_y, buff, 1);
/*
		Locate edges and centre
*/
			x1 = bln_x - menuXleft;
			x3 = x1 + menuCharWidth;
			x2 = x1 + menuCharWidth/2;

			y1 = scr_y - menuYupper;
			y3 = y1 + menuCharHeight;
			y2 = y1 + menuCharHeight/2;
/*
		Determine line style
*/
			switch (ch)
			{
			    case ch_hor:
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x1, y2, x3, y2);
				break;

			    case ch_ver:
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x2, y1, x2, y3);
				break;

			    case ch_botr:
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x1, y2, x2, y2);
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x2, y1, x2, y2);
				break;

			    case ch_botl:
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x3, y2, x2, y2);
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x2, y1, x2, y2);
				break;

			    case ch_topr:
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x1, y2, x2, y2);
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x2, y3, x2, y2);
				break;

			    case ch_topl:
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x3, y2, x2, y2);
				(void) XDrawLine(termDisplay, menuWin, menuGC,
						 x2, y3, x2, y2);
				break;

			    default:
				break;
			}
/*
		Advance one character
*/
			bln_x += menuCharWidth;
		    }
/*
		Buffer character
*/
		    else buff[buff_c++] = ch;
		}
/*
		Draw any buffered characters
*/
		if (buff_c > 0)
		{
		    (void) XDrawImageString(termDisplay, menuWin, menuGC,
					    bln_x, scr_y, buff, buff_c);
		}
/*
		Advance for next line
*/
		scr_y += menuCharHeight;
	    }
	}
	(void) XFlush(termDisplay);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE DRAW_STRING( X,Y,W )
 *       -------------------------------
 *
 *  Redraw area of screen positioned at character x,y length w chars
 *
 *----------------------------------------------------------------------
 */

void draw_string ( x,y,w )
int x,y,w;
{
    w = w * termCharWidth;
    x = x * termCharWidth;
    y = y * termCharHeight;

    draw_term(x,y,w,termCharHeight);
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE MENU_STRING( X,Y,W )
 *       -------------------------------
 *
 *  Redraw area of menu positioned at character x,y length w chars
 *
 *----------------------------------------------------------------------
 */

void menu_string ( x,y,w )
int x,y,w;
{
    w = w * menuCharWidth;
    x = x * menuCharWidth;
    y = y * menuCharHeight;

    draw_menu(x,y,w,menuCharHeight);
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SCROLL_TERM( H, T )
 *       ------------------------------
 *
 *  Scroll terminal by H lines and redraw if T
 *
 *----------------------------------------------------------------------
 */

void scroll_term ( h, t )
int h;
Bool t;
{
    int i,j,l;
    unsigned char *ptr, *att;

    for ( i=0; i < h; i++)
    {
/*
	Bubble first line in scrolling region to end
*/
	for ( j=0; j<(termHeight-1); j++ )
	{
	    l = line_pointers[j];
	    line_pointers[j] = line_pointers[j+1];
	    line_pointers[j+1] = l;
	};
/*
	Blank new last line
*/
	l = line_pointers[termHeight - 1];
	ptr = &term_screen_map[l];
	att = &term_screen_att[l];
	for ( j=0; j<chars_per_line; j++ )
	{
	    *ptr++ = ' ';
	    *att++ = True;
	};
/*
	Move cursor up
*/
	if (termYpos > 0) termYpos--;
    };

    if ( t )
    {
/*
	Redraw scrolled area by graphics copy
	Will get expose event for area off screen if don't adjust height
*/
	i = termWidth * termCharWidth;
	l = h * termCharHeight;
	j = (termHeight-h) * termCharHeight;
	(void) XCopyArea(termDisplay,termWin,termWin,foreGC,
			 0,l,i,j,0,0);
/*
	Explicit redraw of new lines
*/
	for (i=termHeight-h; i<termHeight; i++) draw_string(0,i,termWidth);
    }
    else
    {
/*
	Redraw scrolled area explicitly
*/
	for (i=0; i<termHeight; i++) draw_string(0,i,termWidth);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE HIDE_CURSOR( )
 *       -------------------------
 *
 *  Hide terminal cursor
 *
 *----------------------------------------------------------------------
 */

void hide_cursor()
{
    termCurse = False;
    draw_string(termXpos, termYpos, 1);
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11CU ( XP, YP )
 *       ----------------------------
 *
 *       Move menu cursor to XP, YP
 *
 *----------------------------------------------------------------------
 */

void sx11cu_ ( xp, yp )
F_integer *xp, *yp;

{
    int x,y;

    if ( menuMade )
    {
	x = (int) *xp;
	y = (int) *yp;
	if ((x != menuXpos) || (y != menuYpos))
	{
	    if ((y >= 0) && (y < menuHeight))
	    {
		if ((x >= 0) && (x < chars_per_line))
		{
		    menuXpos = x;
		    menuYpos = y;
		}
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11CR ( XP, YP )
 *       ----------------------------
 *
 *       Save menu cursor position
 *
 *----------------------------------------------------------------------
 */

void sx11cr_ ( xp, yp )
F_integer *xp, *yp;

{
    if ( menuMade )
    {
	*xp = (F_integer) menuXpos;
	*yp = (F_integer) menuYpos;
    }
    else
    {
	*xp = (F_integer) -1;
	*yp = *xp;
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11MC ( FGC, BGC )
 *       ------------------------------
 *
 *       Set foreground and background writing colours for menus
 *
 *----------------------------------------------------------------------
 */

void sx11mc_ ( fgc, bgc )
F_integer *fgc, *bgc;

{
    menuFgc = (int) *fgc;
    menuBgc = (int) *bgc;
    menuAtt = menuFgc + (menuBgc << 4);
}

/****************** MENU TERMINAL ROUTINES *******************/

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION MENTOU ( STRING )
 *       ----------------------------------
 *
 *       PARAMETERS:
 *
 *       character*(*) string : INPUT - The string to write to the menu
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical mentou_ ( astring, string_length )

char *astring;
medium string_length;

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    int i;                              /* Loop variable                */
    int lineind;
    unsigned char *ptr,*att;
    char *string;

    string = astring;

    if (menuMade)
    {
	lineind = menu_pointers[menuYpos];
	ptr  = &menu_screen_map[lineind];
	ptr += menuXpos;
	att  = &menu_screen_att[lineind];
	att += menuXpos;

	nchars = (int) string_length;
	if ( nchars > 0 )
	{
	    for ( i = 0; i < nchars; i++ )
	    {
		*ptr++ = *string++;
		*att++ = menuAtt;
	    };
	    menu_string(menuXpos, menuYpos, nchars);
	    menuXpos += nchars;
	    if (menuXpos >= chars_per_line) menuXpos = chars_per_line - 1;
	};
	status = F_FALSE;
    }
    else status = F_TRUE;

    /*  All done  */

    return ( status );

}

/****************** SEMPER TERMINAL ROUTINES *******************/

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTOU ( STRING )
 *       ----------------------------------
 *
 *       PARAMETERS:
 *
 *       character*(*) string : INPUT - The string to write to
 *                              standard output, normally the terminal.
 *
 *       Writes the given string to the terminal output.
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtou_ ( astring, string_length )

char *astring;
medium string_length;

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    int i;                              /* Loop variable                */
    int lineind;
    unsigned char *ptr,*att;
    char *string;

    string = astring;
    status = F_FALSE;
/* Test 
    string_length = (medium) strlen ( astring ); */
    if (termMade)
    {
	lineind = line_pointers[termYpos];
	ptr  = &term_screen_map[lineind];
	ptr += termXpos;
	att  = &term_screen_att[lineind];
	att += termXpos;

	nchars = (int) string_length;
	if ( nchars > 0 )
	{
	    hide_cursor();
	    for ( i = 0; i < nchars; i++ )
	    {
		*ptr++ = *string++;
		*att++ = termVideo;
	    };
	    draw_string(termXpos, termYpos, nchars);
	    termXpos += nchars;
	    if (termXpos >= chars_per_line) termXpos = chars_per_line - 1;
	    show_cursor();
	};
    }
    else
    {

    /*  Write to standard output  */
	nchars = write ( 1, string, (int) string_length );
	if ( nchars != (int) string_length )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */

	    perror ( "SEMTOU" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/* C callable form of function */

F_logical csemtou ( astring )
char *astring;

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    int i;                              /* Loop variable                */
    int lineind;
    unsigned char *ptr,*att;
    char *string;

    medium string_length;

    string = astring;
    status = F_FALSE;

    string_length = (medium) strlen ( string );

    if (termMade)
    {
	lineind = line_pointers[termYpos];
	ptr  = &term_screen_map[lineind];
	ptr += termXpos;
	att  = &term_screen_att[lineind];
	att += termXpos;

	nchars = (int) string_length;
	if ( nchars > 0 )
	{
	    hide_cursor();
	    for ( i = 0; i < nchars; i++ )
	    {
		*ptr++ = *string++;
		*att++ = termVideo;
	    };
	    draw_string(termXpos, termYpos, nchars);
	    termXpos += nchars;
	    if (termXpos >= chars_per_line) termXpos = chars_per_line - 1;
	    show_cursor();
	};
    }
    else
    {

    /*  Write to standard output  */
	nchars = write ( 1, string, (int) string_length );
	if ( nchars != (int) string_length )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */

	    perror ( "CSEMTOU" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SX11VO ( STRING )
 *       ----------------------------------
 *
 *       PARAMETERS:
 *
 *       character*(*) string : INPUT - The string to write to
 *                                      out vertically
 *
 *       Writes the given string vertically on the menu display
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical sx11vo_ ( astring, string_length )

char *astring;
medium string_length;

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    int i;                              /* Loop variable                */
    int y;                              /* Manual loop variable         */
    int lineind;
    unsigned char *ptr;
    char *string;

    string = astring;

    if (menuMade)
    {
	nchars = (int) string_length;
	y = menuYpos;
	for (i = 0; ((i < nchars) && (y < menuHeight)); i++)
	{
	    lineind = menu_pointers[y];
	    ptr = &menu_screen_map[lineind];
	    ptr += menuXpos;
	    *ptr = *string++;
	    ptr = &menu_screen_att[lineind];
	    ptr += menuXpos;
	    *ptr = menuAtt;

	    menu_string(menuXpos, y, 1);

	    y++;
	};

	status = F_FALSE;
    }
    else
    {
	status = F_TRUE;
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTBS ( STEPS )
 *       ---------------------------------
 *
 *       PARAMETERS:
 *
 *       integer steps: INPUT - The number of backspaces to do.
 *
 *       Backspaces the cursor
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtbs_ ( steps )

F_integer *steps;

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    int i;
    char bs = 8;

    status = F_FALSE;

    if (termMade)
    {
	hide_cursor();
	termXpos -= (int) *steps;
	if (termXpos < 0) termXpos = 0;
	show_cursor();
    }
    else
    {

    /*  Write to standard output  */
	for (i = (int) *steps; i > 0; i-- )
	{
	    nchars = write ( 1, &bs, 1 );
	    if ( nchars != 1 )
	    {

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */
		perror ( "SEMTBS" );
		status = F_TRUE;
	    }
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTCR
 *       -----------------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Places the cursor at the beginning of the line
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtcr_ ()

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    char cr = 13;

    status = F_FALSE;

    if (termMade)
    {
	hide_cursor();
	termXpos = 0;
	show_cursor();
    }
    else
    {

    /*  Write to standard output  */

	nchars = write ( 1, &cr, 1 );
	if ( nchars != 1 )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */
	    perror ( "SEMTCR" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTCU
 *       -----------------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Places the cursor on the previous line
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtcu_ ()

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    char cup[3];
    cup[0] = 27;
    cup[1] = '[';
    cup[2] = 'A';

    status = F_FALSE;

    if (termMade)
    {
	hide_cursor();
	if (termYpos > 0) termYpos--;
	show_cursor();
    }
    else
    {

    /*  Write to standard output  */

	nchars = write ( 1, &cup[0], 3 );
	if ( nchars != 3 )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */
	    perror ( "SEMTCU" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTFL
 *       -----------------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Update all screen information
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtfl_ ()

{
    if (termMade)
    {
	(void) XFlush(termDisplay);
    };

    /*  All done  */

    return ( F_FALSE );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTLF
 *       -----------------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Places the cursor on the next line (scroll if necessary)
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtlf_ ()

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    char lf = 10;

    status = F_FALSE;

    if (termMade)
    {
	hide_cursor();
	termYpos++;
	if (termYpos >= termHeight)
	{
	    nchars = termYpos - termHeight + 1;
	    scroll_term(nchars, True);
	    (void) XFlush(termDisplay);
	}
	show_cursor();
    }
    else
    {

    /*  Write to standard output  */

	nchars = write ( 1, &lf, 1 );
	if ( nchars != 1 )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */
	    perror ( "SEMTLF" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTNV
 *       -----------------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Places the terminal in normal video
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtnv_ ()

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    char norm[4];
    norm[0] = 27;
    norm[1] = '[';
    norm[2] = '0';
    norm[3] = 'm';

    status = F_FALSE;

    if (termMade)
    {
	termVideo = True;
    }
    else
    {

    /*  Write to standard output  */

	nchars = write ( 1, &norm[0], 4 );
	if ( nchars != 4 )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */
	    perror ( "SEMTNV" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SEMTRV
 *       -----------------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Places the terminal in reverse video
 *
 *       Returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical semtrv_ ()

{
    F_logical status;                   /* Return status                */
    int nchars;                         /* Number of chars              */

    char reverse[4];
    reverse[0] = 27;
    reverse[1] = '[';
    reverse[2] = '7';
    reverse[3] = 'm';

    status = F_FALSE;

    if (termMade)
    {
	termVideo = False;
    }
    else
    {

    /*  Write to standard output  */

	nchars = write ( 1, &reverse[0], 4 );
	if ( nchars != 4 )
	{

	/*  Since we can't return an error code, print out an error
	 *  and give up
	 */
	    perror ( "SEMTRV" );
	    status = F_TRUE;
	}
    };

    /*  All done  */

    return ( status );

}

/*----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11CT ( ABOVE, BELOW )
 *       ----------------------------------
 *
 *       PARAMETERS:
 *
 *           logical above - INPUT : clear screen above scroll point
 *
 *           logical below - INPUT : clear screen below scroll point
 *
 *----------------------------------------------------------------------
 */

void sx11ct_ ( above, below )
F_logical *above, *below;

{
    int nchars;                         /* Number of chars              */
    int i, j;
    unsigned char *ptr, *att;

    char homecl[7];

    if (termMade)
    {
	if (*below == F_TRUE)
	{
	    for ( i=0; i < termHeight; i++)
	    {
		j = line_pointers[i];
		ptr = &term_screen_map[j];
		att = &term_screen_att[j];
		for ( j=0; j < chars_per_line; j++)
		{
		    *ptr++ = ' ';
		    *att++ = True;
		}
	    };
	    i = termWidth * termCharWidth;
	    j = termHeight * termCharHeight;
	    draw_term(0,0,i,j);
	    hide_cursor();
	    termXpos = 0;
	    termYpos = 0;
	    show_cursor();
	}
    }
    else
    {

    /*  Write to standard output  */

	homecl[0] = 27;
	homecl[1] = '[';
	homecl[2] = 'H';
	homecl[3] = 27;
	homecl[4] = '[';
	homecl[5] = '2';
	homecl[6] = 'J';
	nchars = write ( 1, &homecl[0], 7 );
	if ( nchars != 7 )
	{

	/*  Since we can't return an error code, print out an error
	 */
	    perror ( "SX11CT" );
	}
    }
}

/*----------------------------------------------------------------------
 *
 *       SUBROUTINE BUZZER
 *       -----------------
 *
 *       PARAMETERS:
 *
 *           NONE
 *
 *       Sound the bell!
 *
 *----------------------------------------------------------------------
 */

void buzzer_ ()

{
    int nchars;                         /* Number of chars              */

    char bell = 7;

    if (termMade)
    {
	(void) XRaiseWindow (termDisplay, termWin);
	(void) XFlush (termDisplay);
	(void) XBell (termDisplay,100);
    }
    else
    {

    /*  Write to standard output  */

	nchars = write ( 1, &bell, 1 );
	if ( nchars != 1 )
	{

	/*  Since we can't return an error code, print out an error
	 */
	    perror ( "BUZZER" );
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SHOW_MESSAGE( S )
 *       ----------------------------
 *
 *  Display error message on X terminal
 *
 *----------------------------------------------------------------------
 */

void show_message(s)
char *s;
{
     (void) semtcr_();
     (void) semtlf_();
     (void) fprintf(stderr,"semper-x11:%s\n",s);
     (void) csemtou(s);
     (void) semtcr_();
     (void) semtlf_();
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE EXDRAW ( X,Y, W,H, MEM, MX )
 *       ---------------------------------------
 *
 *  Redraw area of screen positioned at x,y size w,h from external
 *  memory area mem which is of row width mx
 *
 *----------------------------------------------------------------------
 */

void exdraw ( x,y, w,h, mem, mx )
int x,y, w,h, mx;
unsigned char *mem;

{
    int i, j;
    unsigned int wh, wd;
    u_medium s_buf_size, offset, off2;
    unsigned char *subMap, *fptr, *optr, *sptr;

    XImage *subImage;

    if ( frameIsIcon ||
	 ( frameVisual.class != PseudoColor ) || ( frameVisual.depth != 8 ) )
    {
/*      Ignore while unmapped or if not 8 bit PseudoColor visual        */
    }
    else
    {
	if ((x+w) > frameVirtWidth)  w = frameVirtWidth - x;
	if ((y+h) > frameVirtHeight) h = frameVirtHeight - y;

	s_buf_size = (u_medium) w * (u_medium )h;
	subMap = (unsigned char *) malloc( s_buf_size );
	if ( subMap == NULL )
	{
	    show_message("External redraw failed!! (No memory?)");
	    return;
	}
	sptr = subMap;

/* Copy data (do not use XSubImage as it is very slow) */

	offset = (y * frameVirtWidth) + x;
	off2 = 0;
	
	for (i = y; i < (y+h); i++)
	{
	    fptr = mem + off2;
	    off2 += mx;

	    optr =  XovBuffer + offset;
	    offset += frameVirtWidth;

	    for (j = 0; j < w; j++)
	    {
		XovMap [ 0 ] = XimCell [ *fptr++ ];
		*sptr++ = XovMap [ *optr++ & XovVisibilityMask ];
	    }
	}

	subImage = XCreateImage(termDisplay, frameVisual.visual,
				frameVisual.depth, ZPixmap, 0,
				(char *) subMap, w, h, 8, 0);

	wh = (unsigned) w;
	wd = (unsigned) h;
	(void)  XPutImage ( termDisplay, frameWin, frameGC, subImage,
			    0,0, x-frameXOffset,y-frameYOffset,wh,wd );

	XDestroyImage ( subImage );
	
	(void) XFlush(termDisplay);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE DRAW_DISP( X,Y, W,H )
 *       --------------------------------
 *
 *  Redraw area of screen positioned at x,y size w,h
 *
 *----------------------------------------------------------------------
 */

void draw_disp ( x,y, w,h )
int x,y, w,h;
{
    int i, j;
    unsigned int uw, uh;
    u_medium s_buf_size, offset;
    void *subMap;
    unsigned char  *fptr, *rptr, *gptr, *bptr, *optr, *cptr;
    unsigned short *sptr;
    unsigned long  *lptr;

    XImage *subImage;

    if ( frameIsIcon )
    {
/*      Ignore while unmapped   */
    }
    else
    {
	if ((x+w) > frameVirtWidth)  w = frameVirtWidth - x;
	if ((y+h) > frameVirtHeight) h = frameVirtHeight - y;

	s_buf_size = (u_medium) w * (u_medium )h * ( frameImageMemoryBits / 8 );
	subMap = (void *) malloc( s_buf_size );
	if ( subMap == NULL )
	{
	    s_buf_size = (u_medium) w * ( frameImageMemoryBits / 8 );
	    subMap = (void *) malloc( s_buf_size );
	    if (subMap == NULL)
	    {
		show_message("Redraw display failed!! (No memory?)");
		return;
	    };

	    uw = w;
	    uh = 1;

	    subImage = XCreateImage( termDisplay, frameVisual.visual,
				     frameVisual.depth, ZPixmap, 0,
				     (char *) subMap, uw, uh,
				     frameImageMemoryBits, 0 );

	    offset = ( y * frameVirtWidth ) + x;

	    for ( i = y; i < (y+h); i++ )
	    {
		optr =  XovBuffer + offset;

		if ( frameVisual.class == GrayScale ||
		     frameVisual.class == PseudoColor )
		{
		    fptr = frameBuffer [ 0 ] + offset;

		    if ( frameImageMemoryBits == 8 )
		    {
			cptr = (unsigned char *) subMap;

			for ( j = 0; j < w; j++ )
			{
			    XovMap [ 0 ] = XimCell [ *fptr++ ];
			    *cptr++ = XovMap [ *optr++ & XovVisibilityMask ];
			}
		    }

		    else
		    {
			sptr = (unsigned short *) subMap;

			for ( j = 0; j < w; j++ )
			{
			    XovMap [ 0 ] = XimCell [ *fptr++ ];
			    *sptr++ = XovMap [ *optr++ & XovVisibilityMask ];
			}
		    }
		}

		else if ( frameVisual.class == TrueColor )
		{
		    rptr = frameBuffer [ 0 ] + offset;
		    gptr = frameBuffer [ 1 ] + offset;
		    bptr = frameBuffer [ 2 ] + offset;

		    lptr = (unsigned long *) subMap;

		    for ( j = 0; j < w; j++ )
		    {
			XovRGB [ 0 ] = redMap   [ *rptr++ ] |
				       greenMap [ *gptr++ ] |
				       blueMap  [ *bptr++ ];
			*lptr++ = XovRGB [ *optr++ & XovVisibilityMask ];
		    }
		}

		(void) XPutImage ( termDisplay, frameWin, frameGC, subImage,
				   0, 0, x-frameXOffset, i-frameYOffset, uw, uh );

		offset += frameVirtWidth;
	    }
	}

	else
	{

/* Copy data (do not use XSubImage as it is very slow) */

	    uw = w;
	    uh = h;

	    subImage = XCreateImage( termDisplay, frameVisual.visual,
				     frameVisual.depth, ZPixmap, 0,
				     (char *) subMap, uw, uh,
				     frameImageMemoryBits, 0 );

	    offset = ( y * frameVirtWidth ) + x;

	    for ( i = y; i < (y+h); i++ )
	    {
		optr =  XovBuffer + offset;

		if ( frameVisual.class == GrayScale ||
		     frameVisual.class == PseudoColor )
		{
		    fptr = frameBuffer [ 0 ] + offset;

		    if ( frameImageMemoryBits == 8 )
		    {
			if ( i == y ) cptr = (unsigned char *) subMap;

			for ( j = 0; j < w; j++ )
			{
			    XovMap [ 0 ] = XimCell [ *fptr++ ];
			    *cptr++ = XovMap [ *optr++ & XovVisibilityMask ];
			}
		    }

		    else
		    {
			if ( i == y ) sptr = (unsigned short *) subMap;

			for ( j = 0; j < w; j++ )
			{
			    XovMap [ 0 ] = XimCell [ *fptr++ ];
			    *sptr++ = XovMap [ *optr++ & XovVisibilityMask ];
			}
		    }
		}

		else if ( frameVisual.class == TrueColor )
		{
		    rptr = frameBuffer [ 0 ] + offset;
		    gptr = frameBuffer [ 1 ] + offset;
		    bptr = frameBuffer [ 2 ] + offset;

		    if ( i == y ) lptr = (unsigned long *) subMap;

		    for ( j = 0; j < w; j++ )
		    {
			XovRGB [ 0 ] = redMap   [ *rptr++ ] |
				       greenMap [ *gptr++ ] |
				       blueMap  [ *bptr++ ];
			*lptr++ = XovRGB [ *optr++ & XovVisibilityMask ];
		    }
		}

		offset += frameVirtWidth;
	    }

	    (void)  XPutImage ( termDisplay, frameWin, frameGC, subImage,
				0, 0, x-frameXOffset, y-frameYOffset, uw, uh );
	}

	XDestroyImage ( subImage );
	
	(void) XFlush(termDisplay);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE EXDONE ( X,Y, W,H )
 *       ------------------------------
 *
 *  Refresh area of screen positioned at x,y size w,h (used after exdraw)
 *
 *----------------------------------------------------------------------
 */

void exdone ( x,y, w,h )
int x,y, w,h;

{
    draw_disp ( x,y, w,h );
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE DRAW_VIRT( X,Y, W,H )
 *       --------------------------------
 *
 *  Redraw area of screen positioned at frame pos x,y size w,h
 *
 *----------------------------------------------------------------------
 */

void draw_virt ( x, y, w, h )
int x, y, w, h;
{
    int xmax, ymax;

/*
    Truncate region to redraw
*/

    if ( x < frameXOffset )
    {
	w = w - ( frameXOffset - x );
	x = frameXOffset;
    }

    if ( y < frameYOffset )
    {
	h = h - ( frameYOffset + y );
	y = frameYOffset;
    }

    xmax = frameWidth + frameXOffset;

    if ( ( x + w ) > xmax ) w = xmax - x;

    ymax = frameHeight + frameYOffset;

    if ( ( y + h ) > ymax ) h = ymax - y;

/*
    Redraw region (if any)
*/

    if ( ( w > 0 ) && ( h > 0 ) ) draw_disp ( x, y, w, h );
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE MOVE_VIRT( MX,MY, RESIZED )
 *       -------------------------------
 *
 *  Move centre of visible area to (mx,my)
 *  If not resized then can use graphics copy to help redraw.
 *
 *----------------------------------------------------------------------
 */

void move_virt ( mx,my, resized )
int mx,my;
Bool resized;
{
    void sx11ce_();
    int myx, myy, oldx, oldy;

    myx = mx - (frameWidth/2);
    myy = my - (frameHeight/2);

    if ((myx+frameWidth)>frameVirtWidth) myx = frameVirtWidth - frameWidth;
    if (myx < 0) myx = 0;
    if ((myy+frameHeight)>frameVirtHeight) myy = frameVirtHeight - frameHeight;
    if (myy < 0) myy = 0;

    if ((myx != frameXOffset) || (myy != frameYOffset))
    {
	oldx = frameXOffset;
	oldy = frameYOffset;

	frameXOffset = myx;
	frameYOffset = myy;
/*
	Ensure repaint
*/
	if (resized)
	{
	    draw_virt(frameXOffset,frameYOffset,frameWidth,frameHeight);
	}
	else
	{
/*
	    Copy area and wait for graphics exposures (will there be some?)
*/
	    oldx = myx - oldx;
	    oldy = myy - oldy;
	    (void) XCopyArea(termDisplay,frameWin,frameWin,frameGC,
			     oldx,oldy,frameWidth,frameHeight,0,0);
	    frameExpose = False;
	    do
	    {
		wait_event();
	    } while (!frameExpose);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE FORCE_UPDATE ( )
 *      ---------------------------
 *
 *      Forces display update and clears stale flags
 *
 *----------------------------------------------------------------------
 */

void force_update()
{
    if (frameMade)
    {
	(void) sxrout();
	frameStale = False;
	frameCountI = 0;
	frameCountV = 0;
	(void) XFlush(termDisplay);
    };
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE DRAW_NOTE( X,Y, W,H, I )
 *       -----------------------------------
 *
 *  Note update needed for area of screen and type of draw just done
 *
 *----------------------------------------------------------------------
 */

void draw_note ( x,y, w,h, i )
int x,y, w,h;
Bool i;
{
    if (fsximm)
    {
	if (frameStale) force_update();
	draw_virt(x,y,w,h);
    }
    else
    {
	w += x-1;
	h += y-1;

	if (!frameStale)
	{
	    frameStale = True;
	    frameCountI = 0;
	    frameCountV = 0;
	};

/*
	Add rectangle to list
*/
	(void) sxrins(x,y,w,h);

	if (i)
	{
	    if (frameLimitI > 0)
	    {
		frameCountI++;
		if (frameCountI >= frameLimitI) force_update();
	    }
	}
	else
	{
	    if (frameLimitV > 0)
	    {
		frameCountV++;
		if (frameCountV >= frameLimitV) force_update();
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11GC ( ILIM, VLIM )
 *       --------------------------------
 *
 *----------------------------------------------------------------------
 */

void sx11gc_ ( ilim, vlim )
F_integer *ilim, *vlim;
{
    *ilim = (F_integer) frameLimitI;
    *vlim = (F_integer) frameLimitV;
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11SC ( ILIM, VLIM )
 *       --------------------------------
 *
 *----------------------------------------------------------------------
 */

void sx11sc_ ( ilim, vlim )
F_integer *ilim, *vlim;
{
    frameLimitI = (int) *ilim;
    frameLimitV = (int) *vlim;
}

/****************** X TERMINAL ROUTINES *******************/

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE QUEUE_STRING ( PTR, LENGTH, HOLD )
 *       ---------------------------------------------
 *
 *----------------------------------------------------------------------
 */

void queue_string ( ptr, length, hold )
unsigned char *ptr;
int length;
Bool *hold;

{
    F_integer intch;

    while ((*ptr != 0) && (length > 0))
    {
	length --;

	intch = (F_integer) *ptr++;
	if (intch == 3)
	{
	    (void) abreak ( );
	    intch = -1;
	}
	else if (intch == 1)
	{
	    intch = KBINS;
	}
	else if (intch == 5)
	{
	    intch = KBEND;
	}
	else if (intch == 8)
	{
	    intch = KBHOME;
	}
	else if (intch == 17)
	{
	    intch = -1;
	    *hold = False;
	}
	else if (intch == 18)
	{
	    intch = KBREFL;
	}
	else if (intch == 19)
	{
	    intch = -1;
	    *hold = True;
	}
	else if (intch == 21)
	{
	    intch = KBKILL;
	}
	else if (intch == 28)
	{
	    /*  Process kill */
	    intch = -1;
	    *hold = False;
	    (void) semend_F77();
	};

	if ( intch != -1 )
	{
	    (void) eqxxke_F77 ( &intch );
	};

    } /* end while */
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11DX
 *       -----------------
 *
 *----------------------------------------------------------------------
 */

void sx11dx_ ()

{
    int i;

    for ( i = 0; i < frameCount; i++ )
    {
	if ( frameBuffer [ i ] != NULL )
	{
	    (void) free( frameBuffer [ i ] );
	    frameBuffer [ i ] = NULL;
	}
    }

    if ( XovBuffer != NULL )
    {
	(void) free( XovBuffer );
	XovBuffer = NULL;
    }

    if ( frameFont != NULL )
    {
	(void) XFreeFont ( termDisplay, frameFont);
	frameFont = NULL;
    }

    if ( charBuffer != NULL )
    {
	(void) free(charBuffer);
	charBuffer = NULL;
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11EX
 *       -----------------
 *
 *----------------------------------------------------------------------
 */

void sx11ex_ ()

{
    if ( openedDisp )
    {
	if ( frameMade )
	{
	    (void) XFreeGC ( termDisplay, frameGC );

	    frameMade = False;
	    frameDying = True;
	    (void) XDestroyWindow ( termDisplay, frameWin );

	    sx11dx_( );
	}

	if ( menuMade )
	{
	    (void) XFreeGC ( termDisplay, menuGC );

	    menuMade = False;
	    menuDying = True;
	    (void) XDestroyWindow ( termDisplay, menuWin );
	}

	if ( termMade )
	{
	    (void) XFreeGC ( termDisplay, foreGC );
	    (void) XFreeGC ( termDisplay, backGC );

	    termMade = False;
	    termDying = True;
	    (void) XDestroyWindow ( termDisplay, termWin );
	}

	if (grayScalemap != NULL)
	{
	    (void) XFreeColormap ( termDisplay, grayScalemap );
	    grayScalemap = NULL;
	}

	if (pseudoColormap != NULL)
	{
	    (void) XFreeColormap ( termDisplay, pseudoColormap );
	    pseudoColormap = NULL;
	}

	if ( trueColormap != NULL )
	{
	    (void) XFreeColormap ( termDisplay, trueColormap );
	    trueColormap = NULL;
	}

	(void) XCloseDisplay ( termDisplay );

	openedDisp = False;
    }
}

void note_unknown(s)
char *s;
{
    (void) fprintf(stderr,"Warning: %s event on unknown window\n",s);
}

void note_destroyed(s)
char *s;
{
    (void) fprintf(stderr,"Fatal: %s window destroyed - Semper aborting\n",s);
}

/*
 *----------------------------------------------------------------------
 *
 *       SUBROUTINE SX11CE
 *       -----------------
 *
 *----------------------------------------------------------------------
 */

void sx11ce_ ()

{
    F_logical opened;
    F_integer button;

    unsigned char buffer[40];
    int bufsize = 40;
    int keycount;

    F_integer intch;                    /*  Integer character to queue  */

    int newHeight, i, j, x, y, w;
    unsigned char *ptr, *att;

    F_integer mx, my;

    Bool hold, show;

#define expo_max (20)

    int expo_cnt,expo_ind;
    int expo_x[expo_max],expo_y[expo_max];
    int expo_w[expo_max],expo_h[expo_max];

    KeySym keysym;

    int XPending(), XLookupString();

    Bool menu_event, term_event, frame_event;

    if ( openedDisp )
    {
	hold = False;
	expo_cnt = 0;
	while ( (XPending ( termDisplay ) != 0) || hold )
	{
	    (void) XNextEvent ( termDisplay, &report );

	    term_event = (termMade || termDying) &&
			 (report.xany.window == termWin);
	    menu_event = (menuMade || menuDying) &&
			 (report.xany.window == menuWin);
	    frame_event = (frameMade || frameDying) &&
			  (report.xany.window == frameWin);

	    switch (report.type)
	    {
/*
   Window expose event - repaint appropriate parts
*/
	  case Expose:
		if (term_event)
		{
		    draw_term(report.xexpose.x,report.xexpose.y,
			      report.xexpose.width,report.xexpose.height);
		    termExpose = True;
		}
		else if (frame_event)
		{
		    if (!frameIgnore)
		    draw_disp(report.xexpose.x + frameXOffset,
			      report.xexpose.y + frameYOffset,
			      report.xexpose.width,report.xexpose.height);
		    frameExpose = True;
		    frameIgnore = False;
		}
		else if (menu_event)
		{
		    draw_menu(report.xexpose.x,report.xexpose.y,
			      report.xexpose.width,report.xexpose.height);
		    menuExpose = True;
		}
		else note_unknown("expose");
		break;

	  case GraphicsExpose:
		if (term_event)
		{
/*
		Due to asynchronous nature of expose events, damaged
		region may have gone further upscreen by now, so smear
		to top of screen.
*/
		    {
			expo_ind = -1;
			i = 0;
			x = report.xgraphicsexpose.x;
			y = report.xgraphicsexpose.y;
			w = report.xgraphicsexpose.width;
/*
		Does this region intersect any others that have been stored?
*/
			while ((expo_ind < 0) && (i < expo_cnt))
			{
			    i++;
			    if ((x >= expo_x[i]) &&
				((x+w) <= (expo_x[i]+expo_w[i])))
			    {
/*
		Totally enclosed
*/
				if (y > expo_y[i]) expo_y[i] = y;
				expo_ind = i;
			    }
			    else if ((x <= expo_x[i]) &&
				((x+w) >= expo_x[i]))
			    {
/*
		Intersect at left
*/
				expo_x[i] = x;
				if (y > expo_y[i]) expo_y[i] = y;
/*
		New area encloses old ?
*/
				if ((x+w) >= (expo_x[i]+expo_w[i]))
				    expo_w[i] = w;
				else
				    expo_w[i] += expo_x[i] - x;
				expo_ind = i;
			    }
			    else if ((expo_x[i] <= x) &&
				((expo_x[i]+expo_w[i]) >= x))
			    {
/*
		Intersect at right - known not to be enclosed
				     therefore adjust width
*/
				expo_w[i] += x - expo_x[i];
				if (y > expo_y[i]) expo_y[i] = y;
				expo_ind = i;
			    }
			};
			if (expo_ind < 0)
			{
			    if (expo_cnt < expo_max)
			    {
				expo_cnt++;
				expo_x[expo_cnt] = report.xgraphicsexpose.x;
				expo_y[expo_cnt] = report.xgraphicsexpose.y;
				expo_w[expo_cnt] = report.xgraphicsexpose.width,
				expo_h[expo_cnt] = report.xgraphicsexpose.height;
			    }
			    else
			    {
(void) fprintf(stderr,"debug:expose overflow\n");
				draw_term(report.xgraphicsexpose.x,(int) 0,
					  report.xgraphicsexpose.width,
					  report.xgraphicsexpose.y +
					  report.xgraphicsexpose.height);
			    }
			}
		    }
		    termExpose = True;
		}
		else if (frame_event)
		{
		    if (!frameIgnore)
		    draw_disp(report.xgraphicsexpose.x + frameXOffset,
			      report.xgraphicsexpose.y + frameYOffset,
			      report.xgraphicsexpose.width,
			      report.xgraphicsexpose.height);
		    frameExpose = True;
		    frameIgnore = False;
		}
		else if (menu_event)
		{
		    draw_menu(report.xgraphicsexpose.x,
			      report.xgraphicsexpose.y,
			      report.xgraphicsexpose.width,
			      report.xgraphicsexpose.height);
		    menuExpose = True;
		}
		else note_unknown("graphics expose");
		break;

/*
	Button press/release - not worried about actual window
*/
	  case ButtonPress:
	  case ButtonRelease:
		button = (F_integer) report.xbutton.button;
		if (report.type == ButtonPress)
		{
		   opened = F_FALSE;
		}
		else
		{
		   opened = F_TRUE;
		};
		(void) eqxxse_F77 ( &button,&opened );
		break;

/*
   For key presses the actual window doesn't matter at present
   Note that as at present we do not use XRebindKeysym buffer
*/
	  case KeyPress:
		intch = -1;
		keycount = XLookupString ( &report, buffer, bufsize,
					   &keysym, NULL );
		if ((keysym == XK_Return)    ||
		    (keysym == XK_KP_Enter)  ||
		    (keysym == XK_Linefeed) )
		{
		    intch = KBRET;
		}
		else if ((keysym == XK_Tab) || (keysym == XK_KP_Tab))
		{
		    intch = KBTAB;
		}
		else if (keysym == XK_KP_Equal)
		{
		    intch = KEQUAL;
		}
		else if (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9)) ||
			 ((keysym >= XK_space) && (keysym <= XK_asciitilde)))
		{
		    queue_string ( &buffer[0], keycount, &hold );
		}
		else if ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R))
		{
		   /* Do nothing - it's a modifier */
		    ;
		}
		else if ((keysym == XK_BackSpace) ||
			 (keysym == XK_Delete))
		{
		    intch = KBDEL;
		}
		else if ((keysym == XK_Up) || (keysym == XK_Prior))
		{
		    intch = KBUP;
		}
		else if ((keysym == XK_Down) || (keysym == XK_Next))
		{
		    intch = KBDOWN;
		}
		else if (keysym == XK_Left)
		{
		    intch = KBLEFT;
		}
		else if (keysym == XK_Right)
		{
		    intch = KBRITE;
		}
		else if (keysym == XK_Insert)
		{
		    intch = KBINS;
		}
		else if ((keysym == XK_Home) || (keysym == XK_Begin))
		{
		    intch = KBHOME;
		}
		else if (keysym == XK_End)
		{
		    intch = KBEND;
		}
		else if (keysym == XK_Escape || keysym == XK_Clear)
		{
		    intch = KBKILL;
		}
		else if ((keysym >= XK_F1) && (keysym <= XK_F35))
		{
		    intch = KBFUNC + 1 + (F_integer) (keysym - XK_F1);
		}
		else if ((keysym >= XK_KP_F1) && (keysym <= XK_KP_F4))
		{
		    intch = KBFUNC + 1 + (F_integer) (keysym - XK_KP_F1);
		}
		else
		{
		    ptr = (unsigned char *) XKeysymToString(keysym);
		    if (ptr != NULL)
		    {
			keycount = (int) strlen ( (char *) ptr );
			queue_string ( ptr, keycount, &hold );
		    };
		};

		/*  Anything happened?  Queue if so  */

		if ( intch != -1 )
		{
		    (void) eqxxke_F77 ( &intch );
		};
		break;

	  case KeyRelease:
/*
   Shouldn't happen, but OpenLook seems to give you one with the Front key
*/
		break;

	  case FocusIn:
	  case FocusOut:
/*
   Shouldn't happen, but OpenLook seems to give you one with the Front key
*/
		break;
/*
   Keyboard remap - note event
*/
	  case MappingNotify:
		if (report.xmapping.request == MappingKeyboard)
		    (void) XRefreshKeyboardMapping(&report.xmapping);
		break;

	  case KeymapNotify:
		break;

/*
   Resize operation detected
*/
	  case ConfigureNotify:
		if (term_event)
		{
		    termWidth = report.xconfigure.width/termCharWidth;
		    newHeight = report.xconfigure.height/termCharHeight;
		    if (newHeight != termHeight)
		    {
/*
   New terminal height - clear lines or scroll ?
*/
			if (newHeight > termHeight)
			{
			    if (newHeight >= lines_per_page)
				newHeight = lines_per_page;

			    for ( i = termHeight + 1; i < newHeight; i++ )
			    {
				j = line_pointers[i];
				ptr = &term_screen_map[j];
				att = &term_screen_att[j];
				for ( j=0; j < chars_per_line; j++)
				{
				    *ptr++ = ' ';
				    *att++ = True;
				}
			    }
			}
			else
			{
			    show = termCurse;
			    hide_cursor();
			    i = termYpos + 1 - newHeight;
			    if ( i > 0) scroll_term( i, False );
			    if (show) show_cursor();
			};

			termHeight = newHeight;
		    }
		}
		else if (menu_event)
		{
		    menuWidth = report.xconfigure.width/menuCharWidth;
		    newHeight = report.xconfigure.height/menuCharHeight;
		    if (newHeight != menuHeight)
		    {
/*
   New menu height - clear lines or scroll ?
*/
			if (newHeight > menuHeight)
			{
			    if (newHeight >= lines_per_page)
				newHeight = lines_per_page;

			    for ( i = menuHeight + 1; i < newHeight; i++ )
			    {
				j = menu_pointers[i];
				ptr = &menu_screen_map[j];
				att = &menu_screen_att[j];
				for ( j=0; j < chars_per_line; j++)
				{
				    *ptr++ = ' ';
				    *att++ = 1;
				}
			    }
			};

			menuHeight = newHeight;
		    }
		}
		else if (frame_event)
		{
		    i = frameXOffset + (frameWidth/2);
		    j = frameYOffset + (frameHeight/2);

		    frameWidth = report.xconfigure.width;
		    frameHeight = report.xconfigure.height;

/*
	Recentre frame view on old centre if possible
*/
		    move_virt(i,j,True);

/*
	Let Semper know about change
*/
		    mx = (F_integer) frameWidth;
		    my = (F_integer) frameHeight;
/*		    (void) fsx11m_(&mx,&my); */
                    fsx11m_F77(&mx,&my);
		}
		else note_unknown("configure");
		break;

	  case UnmapNotify:
		if (term_event)
		{
		    termIsIcon = True;
		}
		else if (menu_event)
		{
		    menuIsIcon = True;
		}
		else if (frame_event)
		{
		    frameIsIcon = True;
		}
		else note_unknown("unmap");
		break;

	  case MapNotify:
		if (term_event)
		{
		    termIsIcon = False;
		}
		else if (menu_event)
		{
		    menuIsIcon = False;
		}
		else if (frame_event)
		{
		    frameIsIcon = False;
		}
		else note_unknown("map");
		break;

/*
	Window destroyed - did we expect this ?
*/
	  case DestroyNotify:
		if (term_event)
		{
		    if (!termDying)
		    {
			termDying = True;
			termMade = False;
			note_destroyed("terminal");
			sx11ex_();
/*			(void) semend_(); */
                        semend_F77 ();
		    };
		}
		else if (menu_event)
		{
		    if (!menuDying)
		    {
			menuDying = True;
			menuMade = False;
			note_destroyed("menu");
			sx11ex_();
/*			(void) semend_(); */
                        semend_F77 () ;
		    }
		    else menuDying = False;
		}
		else if (frame_event)
		{
		    if (!frameDying)
		    {
			frameDying = True;
			frameMade = False;
			note_destroyed("display");

			sx11dx_ ( );

			sx11ex_ ( );

/*			(void) semend_ ( ); */
                        semend_F77 ();
		    }
		    else frameDying = False;
		}
		else note_unknown("destroy");
		hold = False;
		break;

/*
   Pointer into window - tie position for pointer relative movements...
*/
	  case EnterNotify:
		if (term_event)
		{
		    termInFocus = True;
		    termLastX = report.xcrossing.x;
		    termLastY = report.xcrossing.y;
		}
		else if (menu_event)
		{
		    menuInFocus = True;
		    menuLastX = report.xcrossing.x;
		    menuLastY = report.xcrossing.y;
		}
		else if (frame_event)
		{
		    frameInFocus = True;
		    frameLastX = report.xcrossing.x;
		    frameLastY = report.xcrossing.y;
		}
		else note_unknown("enter");
		break;

/*
   Pointer out of window
*/
	  case LeaveNotify:
		if (term_event)
		{
		    i = termLastX;
		    j = termLastY;
		    termLastX = report.xcrossing.x;
		    termLastY = report.xcrossing.y;
		    termInFocus = False;
		}
		else if (menu_event)
		{
		    i = menuLastX;
		    j = menuLastY;
		    menuLastX = report.xcrossing.x;
		    menuLastY = report.xcrossing.y;
/*
		    Let UIF know
*/
		    mx = (F_integer) menuLastX;
		    my = (F_integer) menuLastY;
		    intch = 1;
/*		    (void) uixxrm_(&intch,&mx,&my); */
                    uixxrm_F77 (&intch,&mx,&my);
		    menuInFocus = False;
		}
		else if (frame_event)
		{
		    i = frameLastX;
		    j = frameLastY;
		    frameLastX = report.xcrossing.x;
		    frameLastY = report.xcrossing.y;
		    frameInFocus = False;
		}
		else
		{
		    i = report.xcrossing.x;
		    j = report.xcrossing.y;
		    note_unknown("leave");
		};
		mx = (F_integer) (report.xcrossing.x - i);
		my = (F_integer) (report.xcrossing.y - j);
/*
		Only note data if we haven't just warped the pointer
*/
		if (!warpingNow) (void) eqxxpe_F77 (&mx,&my);
		break;

	  case MotionNotify:
		if (term_event)
		{
		    i = termLastX;
		    j = termLastY;
		    termLastX = report.xmotion.x;
		    termLastY = report.xmotion.y;
		}
		else if (menu_event)
		{
		    i = menuLastX;
		    j = menuLastY;
		    menuLastX = report.xmotion.x;
		    menuLastY = report.xmotion.y;
/*
		    Let UIF know
*/
		    mx = (F_integer) menuLastX;
		    my = (F_integer) menuLastY;
		    intch = 1;
/*		    (void) uixxrm_(&intch,&mx,&my); */
                    uixxrm_F77 (&intch,&mx,&my);
		}
		else if (frame_event)
		{
		    i = frameLastX;
		    j = frameLastY;
		    frameLastX = report.xmotion.x;
		    frameLastY = report.xmotion.y;
		}
		else
		{
		    i = report.xmotion.x;
		    j = report.xmotion.y;
		    note_unknown("motion");
		};
		mx = (F_integer) (report.xmotion.x - i);
		my = (F_integer) (report.xmotion.y - j);
		if (!warpingNow) (void) eqxxpe_F77 (&mx,&my);
		break;

/*
   Others
*/
	  case CirculateNotify:
		break;

	  case GravityNotify:
		break;

	  case ClientMessage:
		/*(void) printf("ClientMessage of format %d on ",
				report.xclient.format);*/
		if (term_event)
		{
		/*(void) printf("terminal\n");*/
		}
		else if (menu_event)
		{
		/*(void) printf("menu\n");*/
		}
		else if (frame_event)
		{
		/*(void) printf("display\n");*/
		}
		else
		{
		/*(void) printf("unknown\n");*/
		    note_unknown("ClientMessage");
		};
		break;

	  case ReparentNotify:
		break;

	  case NoExpose:
		if (term_event)
		{
		    termExpose = True;
		}
		else if (frame_event)
		{
		    frameExpose = True;
		}
		else if (menu_event)
		{
		    menuExpose = True;
		}
		else note_unknown("noexpose");
		break;

	  default:
		if (term_event)
		{
(void) fprintf(stderr,
	  "term event type=%d serial=%ld\n",report.type, report.xany.serial );
		}
		else if (menu_event)
		{
(void) fprintf(stderr,
	  "menu event type=%d serial=%ld\n",report.type, report.xany.serial );
		}
		else if (frame_event)
		{
(void) fprintf(stderr,
	  "frame event type=%d serial=%ld\n",report.type, report.xany.serial );
		}
		else
		{
(void) fprintf(stderr,
	  "??? event type=%d serial=%ld\n",report.type, report.xany.serial );
		}
		break;
	    }
	};

/*
	Expose pending terminal redraws now
*/
	if (expo_cnt > 0)
	{
	    i = 0;
	    while (i < expo_cnt)
	    {
		i++;
		draw_term(expo_x[i],(int) 0,expo_w[i],expo_y[i]+expo_h[i]);
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION SX11IN ( BATCH )
 *       ---------------------------------
 *
 *       PARAMETERS:
 *
 *       logical batch : INPUT - true if running with no terminal
 *
 *----------------------------------------------------------------------
 */

F_logical sx11in_ ( batch )

F_logical *batch;

{
    XGCValues values;
    unsigned short fgc, bgc;
    unsigned char *ptr;
    register int i, j;
    Bool unseen;
    char *FontName;
    char *ColourName;
    XColor fg, bg;
    XColor color, rgb;
    XColor cell[256];
    unsigned long last_resort;
    int visualsMatched;
    XVisualInfo visualTemplate, *visualList;

    if ( *batch == F_FALSE )
    {
	termDisplayName[0] = ' ';

	if ( ! (termDisplay = XOpenDisplay(NULL) ) )
	{
	    (void) fprintf ( stderr, "Can't open display '%s'\n",
				     XDisplayName(NULL) );
	    return ( F_TRUE );
	}

	openedDisp = True;

	termScreen = DefaultScreen(termDisplay);

/* Get terminal font */

	FontName = getenv ( "SEMPER_FONT" );

	if ( ( termFont = XLoadQueryFont ( termDisplay, FontName ) ) == NULL )
	{
	    FontName = termFontName1;

	    if ( ( termFont = XLoadQueryFont ( termDisplay, FontName ) ) == NULL )
	    {
		FontName = termFontName2;

		if ( ( termFont = XLoadQueryFont ( termDisplay, FontName ) ) == NULL )
		{
		    (void) fprintf ( stderr, "Can't open fonts '%s' or '%s'\n",
					     termFontName1, termFontName2 );
		    return ( F_TRUE );
		}
	    }
	}

	if ( termFont->per_char == NULL ||
	     termFont->min_bounds.width == termFont->max_bounds.width)
	{
/*
   Get Font metrics and use them to determine window size required
*/
	   termXleft  = termFont->min_bounds.lbearing;
	   termCharWidth  = termFont->max_bounds.rbearing - termXleft;
	   termYupper = termFont->max_bounds.ascent;
	   termCharHeight = termFont->max_bounds.descent + termYupper;

/*
   Initialise menuchar height and width
*/
	   menuCharHeight = termCharHeight;
	   menuCharWidth  = termCharWidth;

/*
   Window increments are character cells
*/

	   termSizeHints.width_inc  = termCharWidth;
	   termSizeHints.height_inc = termCharHeight;

/*
   Window size range expected.
   Screen will always be maintained at the maximum size internally
   But may not be visible if the user shrinks it too far!
*/

	   termSizeHints.min_width  =  0;
	   termSizeHints.max_width  =  chars_per_line * termCharWidth;
	   termSizeHints.min_height =  0;
	   termSizeHints.max_height =  lines_per_page * termCharHeight;

	   termSizeHints.width  =  init_width  * termCharWidth;
	   termSizeHints.height =  init_height * termCharHeight;

	   termSizeHints.x = termCharWidth;
	   termSizeHints.y =
		DisplayHeight( termDisplay, termScreen ) - termSizeHints.height
							 - (2 * termCharHeight);

	   termSizeHints.flags  =
	      PMinSize | PMaxSize | PPosition | PSize | PResizeInc |
	      USSize;

	}
	else
	{
	    (void) fprintf ( stderr, "Font '%s' is not a true fixed font\n",
			     FontName );
	    return ( F_TRUE );
	}

	termInFocus = False;

/*
    Set up pointer to default colour map
*/

	defaultColormap = DefaultColormap ( termDisplay, termScreen );

/*
    Search for the standard terminal/menu colours (black, white, red, green,
    blue, cyan, magenta and yellow) in the default colour map
*/

	for ( menuColourCount = 0; menuColourCount < 8; menuColourCount++ )
	{
	    if ( XAllocNamedColor ( termDisplay, defaultColormap,
				    menuColourName [ menuColourCount ],
				    &color, &rgb ) == 0 ) break;

	    menuMap [ menuColourCount ] = color.pixel;
	}

/*
    Set up terminal foreground and background colours
*/

        fgc = menuMap [ 0 ];
        bgc = menuMap [ 1 ];

        ColourName = getenv ( "SEMPER_FG" );

        if ( ColourName != NULL )
        {
	    if ( XParseColor ( termDisplay, defaultColormap, ColourName, &fg ) )
            {
                if (XAllocColor ( termDisplay, defaultColormap, &fg ) )
                {
                    fgc = fg.pixel;
                }
            }
        }

        ColourName = getenv ( "SEMPER_BG" );

        if ( ColourName != NULL )
        {
            if ( XParseColor ( termDisplay, defaultColormap, ColourName, &bg ) )
            {
                if (XAllocColor ( termDisplay, defaultColormap, &bg ) )
                {
                    bgc = bg.pixel;
                }
            }
        }

/*
    Set up cursors
*/

	termCursor = XCreateFontCursor(termDisplay, XC_xterm);

	(void) XRecolorCursor(termDisplay, termCursor, &fg, &bg);

	arrowCursor = XCreateFontCursor(termDisplay, XC_left_ptr);

	fg.red   = 0;
	fg.green = 0;
	fg.blue  = 0;
	fg.flags = DoRed | DoGreen | DoBlue;

	bg.red   = 65535;
	bg.green = 65535;
	bg.blue  = 65535;
	bg.flags = DoRed | DoGreen | DoBlue;

/*  Red cursor colour because SG cursor lacks background surround  */

	if ( ! strcmp ( ServerVendor ( termDisplay ), "Silicon Graphics" ) )
	{
	    fg.red   = 65535;
	}

	(void) XRecolorCursor(termDisplay, arrowCursor, &fg, &bg);

	blankMap = XCreateBitmapFromData( termDisplay,
					  RootWindow( termDisplay, termScreen ),
					  blank_bits, 16, 16);

	blankCursor = XCreatePixmapCursor( termDisplay, blankMap, blankMap,
					   &fg, &bg, 8, 8);

	(void) XFreePixmap(termDisplay,blankMap);

/*
    Examine the visuals supported on the default screen
*/

	visualTemplate.screen = termScreen;

	visualList = XGetVisualInfo ( termDisplay, VisualScreenMask,
				      &visualTemplate, &visualsMatched );

/*
    Find the default visual
*/

	for ( i = 0; i < visualsMatched; i++ )
	{
	    if ( visualList [ i ].visual == DefaultVisual ( termDisplay, termScreen ) )
	    {
		defaultVisual = visualList [ i ];
	    }
	}

/*
    Find PseudoColor and TrueColor visuals (if any) with the greatest depth
    not exceeding the depth of the default visual
*/

	grayScaleVisual.visual = (Visual *) NULL;
	grayScaleVisual.depth = 0;

	pseudoColorVisual.visual = (Visual *) NULL;
	pseudoColorVisual.depth = 0;

	trueColorVisual.visual = (Visual *) NULL;
	trueColorVisual.depth = 0;

	for ( i = 0; i < visualsMatched; i++ )
	{
	    if ( visualList [ i ].class == GrayScale )
	    {
		if ( visualList [ i ].depth > grayScaleVisual.depth )
		{
		    grayScaleVisual = visualList [ i ];
		}
	    }

	    else if ( visualList [ i ].class == PseudoColor )
	    {
		if ( visualList [ i ].depth > pseudoColorVisual.depth )
		{
		    pseudoColorVisual = visualList [ i ];
		}
	    }

	    else if ( visualList [ i ].class == TrueColor )
	    {
		if ( visualList [ i ].depth > trueColorVisual.depth )
		{
		    trueColorVisual = visualList [ i ];
		}
	    }
	}

	(void) XFree ( visualList );

/*
    Choose a visual for the terminal window and if not using the default
    visual, create a new colour map and copy into it the contents of the
    default colour map
*/

	if ( pseudoColorVisual.visual != (Visual *) NULL )
	{
	    pseudoColormap =

		XCreateColormap ( termDisplay,
				  RootWindow ( termDisplay, termScreen ),
				  pseudoColorVisual.visual, AllocAll );

	    for ( j = 0; j < defaultVisual.colormap_size; )
	    {
		for ( i = 0; i < 256 && j < defaultVisual.colormap_size; i++, j++ )
		{
		    cell [ i ].pixel = j;
		}

		XQueryColors ( termDisplay, defaultColormap, cell, i );

		XStoreColors ( termDisplay, pseudoColormap, cell, i );
	    }

	    termVisual = pseudoColorVisual;

	    termColormap = pseudoColormap;
	}

	else if ( grayScaleVisual.visual != (Visual *) NULL )
	{
	    grayScalemap =

		XCreateColormap ( termDisplay,
				  RootWindow ( termDisplay, termScreen ),
				  grayScaleVisual.visual, AllocAll );

	    for ( j = 0; j < defaultVisual.colormap_size; )
	    {
		for ( i = 0; i < 256 && j < defaultVisual.colormap_size; i++, j++ )
		{
		    cell [ i ].pixel = j;
		}

		XQueryColors ( termDisplay, defaultColormap, cell, i );

		XStoreColors ( termDisplay, grayScalemap, cell, i );
	    }

	    termVisual = grayScaleVisual;

	    termColormap = grayScalemap;
	}

	else
	{
	    termVisual = defaultVisual;

	    termColormap = defaultColormap;
	}

/*
    Determine size of image look-up table according to chosen visual
*/

	if ( termVisual.depth < 7 )
	{
	    cellCount =  40;
	}

	else if ( termVisual.depth < 8 )
	{
	    cellCount =  104;
	}

	else if ( termVisual.depth > 8 )
	{
	    cellCount = 256;
	}

	else
	{
	    if ( strcmp ( ServerVendor ( termDisplay ), "Silicon Graphics" ) )
	    {
		cellCount = 232;
	    }

	    else
	    {
		cellCount = 192;
	    }
	}

	Xpixmax = cellCount - 1;

/*
    Create the terminal window
*/

	termAttributes.bit_gravity = NorthWestGravity;
	termAttributes.background_pixel = bgc;
	termAttributes.border_pixel     = menuMap [ 0 ];
	termAttributes.cursor = termCursor;
	termAttributes.colormap = termColormap;

	termWin = XCreateWindow ( termDisplay,
				  RootWindow ( termDisplay, termScreen ),
				  termSizeHints.x,     termSizeHints.y,
				  termSizeHints.width, termSizeHints.height,
				  1, termVisual.depth,
				  InputOutput, termVisual.visual,
				  CWBitGravity | CWBackPixel | CWBorderPixel |
				  CWCursor | CWColormap, &termAttributes );

/*        (void) XSetStandardProperties(termDisplay, termWin,
                                      "Semper 6 plus terminal",
                                      "S6term", None, NULL, 0, &termSizeHints ); */
	(void) XSetStandardProperties(termDisplay, termWin,
				      PACKAGE_STRING,
				      "S6term", None, NULL, 0, &termSizeHints );

	termWMHints.input         = True;
	termWMHints.initial_state = NormalState;
	termWMHints.flags         = InputHint | StateHint;

	(void) XSetWMHints(termDisplay, termWin, &termWMHints);

	termMade = True;

/*
   Initialise line pointers and clear screen
*/

	termIsIcon = True;              /* until mapped */
	termExpose = False;             /* one shot flag */
	termVideo  = True;              /* normal */
	termCurse  = False;             /* not showing at present */
	termXpos = 0;                   /* Cursor write position */
	termYpos = 4;
	termWidth = init_width;         /* May be reset on expose */
	termHeight = init_height;

/*
   Set default menu size here to allow restarts
*/
	menuHeight = init_height;
	menuWidth  = init_width;

	for ( i = 0; i < lines_per_page; i++ )
	    line_pointers[i] = i * chars_per_line;

	ptr = &term_screen_map[0];
	for ( i = 0; i < chars_per_page; i++ ) *ptr++ = ' ';

/*
	Put some useful info in the screen
*/
	ptr = &term_screen_map[0];
	(void) sprintf( (char *) ptr,
			"X Windows Version %d Protocol revision %d",
			ProtocolVersion(termDisplay),
			ProtocolRevision(termDisplay) );

	ptr = &term_screen_map[chars_per_line];
	(void) sprintf( (char *) ptr,
			"Server vendor:  %s",
			ServerVendor(termDisplay) );

	ptr = &term_screen_map[chars_per_line*2];
	(void) sprintf( (char *) ptr,
			"Vendor release: %d",
			VendorRelease(termDisplay) );

	ptr = &term_screen_att[0];
	for ( i = 0; i < chars_per_page; i++ ) *ptr++ = True;

/*
	Blank static space variable
*/
	for (i = 0; i < sps; i++ ) space[i] = ' ';

	(void) XSelectInput (termDisplay, termWin, termMask );

	values.font = termFont->fid;
	values.foreground = fgc;
	values.background = bgc;

	foreGC = XCreateGC ( termDisplay, termWin,
			     GCForeground | GCBackground | GCFont, &values );

	values.foreground = bgc;
	values.background = fgc;

	backGC = XCreateGC ( termDisplay, termWin,
			     GCForeground | GCBackground | GCFont, &values );

/*
    Now map the window and wait for it to appear
*/

	(void) XMapWindow(termDisplay, termWin);

	do
	{
	    wait_event();
	} while (!termExpose);

/*
	XSetInputFocus ( termDisplay, termWin, RevertToNone, CurrentTime );
	(void) XFlush(termDisplay);
*/
    }

    return  ( F_FALSE );
}

/****************** FRAME ACCESS ROUTINES *******************/

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION FSBNRO ( ROW, FIRST, STEP, LAST, IX, IY)
 *       LOGICAL FUNCTION FSINRO ( ROW, FIRST, STEP, LAST, IX, IY)
 *       LOGICAL FUNCTION FSFNRO ( ROW, FIRST, STEP, LAST, IX, IY)
 *       ---------------------------------------------------------
 *
 *       PARAMETERS:
 *
 *       byte    (FSBNRO)
 *       integer (FSINRO) row : INPUT - source data to write
 *       real    (FSFNRO)
 *
 *       integer first : INPUT - index into row of first pixel
 *       integer step  : INPUT - spacing between pixels in row
 *       integer last  : INPUT - index into row of last pixel
 *
 *       integer ix,iy : INPUT - The position to write at.
 *
 *       Each Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical fsbnro_ (frow, first, last, step, ix, iy, ifr)
unsigned char *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
{
    u_medium ind, cnt, stp;
    unsigned char *ptr, p;
    unsigned char *row, *pts;
    int f, w;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;

/* Get start index (from 0) */

    ind = (u_medium) *first;
    ind--;
    row += ind;

/* Get modified end position */

    cnt = (u_medium) *last;
    cnt -= ind;

    stp = (u_medium) *step;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;
    pts = ptr;

    if (stp == 1)
    {
	/* Straight copy */
	for (ind = cnt; ind > 0; ind--)
	{
	    p = *row++;
	    if (p > Xpixmax) *ptr++ = Xpixmax;
	    else             *ptr++ = p;
	}
    }
    else
    {
	/* Subsampling copy */
	for (ind = 0; ind < cnt; ind += stp)
	{
	    p = *row;
	    if (p > Xpixmax) *ptr++ = Xpixmax;
	    else             *ptr++ = p;
	    row += stp;
	}
    }

    w = (ptr - pts);
    draw_note( (int) *ix, (int) *iy, (int) w, (int) 1, True );

    return (F_FALSE);

}

F_logical fsinro_ (frow, first, last, step, ix, iy, ifr)
F_integer *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
{
    u_medium ind, cnt, stp;
    unsigned char *ptr, *pts;
    F_integer *row;
    int f, p, pmax, w;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;
    pmax = (int) Xpixmax;

/* Get start index (from 0) */

    ind = (u_medium) *first;
    ind--;
    row += ind;

/* Get modified end position */

    cnt = (u_medium) *last;
    cnt -= ind;

    stp = (u_medium) *step;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;
    pts = ptr;

    if (stp == 1)
    {
	/* Straight copy */
	for (ind = cnt; ind > 0; ind--)
	{
	    p = (int) *row++;
	    if      (p < 0)    *ptr++ = 0;
	    else if (p > pmax) *ptr++ = Xpixmax;
	    else               *ptr++ = (unsigned char) p;
	}
    }
    else
    {
	/* Subsampling copy */
	for (ind = 0; ind < cnt; ind += stp)
	{
	    p = (int) *row;
	    if      (p < 0)    *ptr++ = 0;
	    else if (p > pmax) *ptr++ = Xpixmax;
	    else               *ptr++ = (unsigned char) p;
	    row += stp;
	}
    }

    w = (ptr - pts);
    draw_note( (int) *ix, (int) *iy, (int) w, (int) 1, True );

    return (F_FALSE);

}

F_logical fsfnro_(frow, first, last, step, ix, iy, ifr)
F_real *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
{
    u_medium ind, cnt, stp;
    unsigned char *ptr, *pts;
    F_real *row;
    int f, w;
    float p, pmax;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    pmax = (float) Xpixmax;
    row = frow;

/* Get start index (from 0) */

    ind = (u_medium) *first;
    ind--;
    row += ind;

/* Get modified end position */

    cnt = (u_medium) *last;
    cnt -= ind;

    stp = (u_medium) *step;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;
    pts = ptr;

    if (stp == 1)
    {
	/* Straight copy */
	for (ind = cnt; ind > 0; ind--)
	{
	    p = (float) *row++;
	    if      (p < 0.0)  *ptr++ = 0;
	    else if (p > pmax) *ptr++ = Xpixmax;
	    else               *ptr++ = (unsigned char) (p + 0.5);
	}
    }
    else
    {
	/* Subsampling copy */
	for (ind = 0; ind < cnt; ind += stp)
	{
	    p = (float) *row;
	    if      (p < 0.0)  *ptr++ = 0;
	    else if (p > pmax) *ptr++ = Xpixmax;
	    else               *ptr++ = (unsigned char) (p + 0.5);
	    row += stp;
	}
    }

    w = (ptr - pts);
    draw_note( (int) *ix, (int) *iy, (int) w, (int) 1, True );

    return (F_FALSE);

}

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION FSBSRO ( ROW, FIRST, STEP, LAST, IX, IY, BL, WH)
 *       LOGICAL FUNCTION FSISRO ( ROW, FIRST, STEP, LAST, IX, IY, BL, WH)
 *       LOGICAL FUNCTION FSFSRO ( ROW, FIRST, STEP, LAST, IX, IY, BL, WH)
 *       -----------------------------------------------------------------
 *
 *       PARAMETERS:
 *
 *       byte    (FSBSRO)
 *       integer (FSISRO) row : INPUT - source data to write
 *       real    (FSFSRO)
 *
 *       integer first : INPUT - index into row of first pixel
 *       integer step  : INPUT - spacing between pixels in row
 *       integer last  : INPUT - index into row of last pixel
 *
 *       integer ix,iy : INPUT - The position to write at.
 *
 *       real bl : INPUT - black pixel level
 *       real wh : INPUT - black pixel level
 *
 *       Each Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical fsbsro_ (frow, first, last, step, ix, iy, ifr, bl, wh)
unsigned char *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
F_real *bl, *wh;
{
    u_medium ind, cnt, stp;
    unsigned char *ptr, *pts;
    unsigned char *row;
    float p, pmax, scale, black, white;
    int f, w;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;

    black = (float) *bl;
    white = (float) *wh;
    pmax = (float) (Xpixmax);

    scale = pmax / (white - black);

/* Get start index (from 0) */

    ind = (u_medium) *first;
    ind--;
    row += ind;

/* Get modified end position */

    cnt = (u_medium) *last;
    cnt -= ind;

    stp = (u_medium) *step;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;
    pts = ptr;

    if (*bl < *wh)
    {
	if (stp == 1)
	{
	    /* Straight scaled copy */
	    for (ind = cnt; ind > 0; ind--)
	    {
		p = (float) *row++;
		if      (p < black) *ptr++ = 0;
		else if (p > white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
	    }
	}
	else
	{
	    /* Subsampling scaled copy */
	    for (ind = 0; ind < cnt; ind += stp)
	    {
		p = (float) *row;
		if      (p < black) *ptr++ = 0;
		else if (p > white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
		row += stp;
	    }
	}
    }
    else
    {
	if (stp == 1)
	{
	    /* Straight scaled copy */
	    for (ind = cnt; ind > 0; ind--)
	    {
		p = (float) *row++;
		if      (p > black) *ptr++ = 0;
		else if (p < white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
	    }
	}
	else
	{
	    /* Subsampling scaled copy */
	    for (ind = 0; ind < cnt; ind += stp)
	    {
		p = (float) *row;
		if      (p > black) *ptr++ = 0;
		else if (p < white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
		row += stp;
	    }
	}
    }

    w = (ptr - pts);
    draw_note( (int) *ix, (int) *iy, (int) w, (int) 1, True );

    return (F_FALSE);

}

F_logical fsisro_ (frow, first, last, step, ix, iy, ifr, bl, wh)
F_integer *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
F_real *bl, *wh;
{
    u_medium ind, cnt, stp;
    unsigned char *ptr, *pts;
    F_integer *row;
    float p, pmax, scale, black, white;
    int f, w;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;

    black = (float) *bl;
    white = (float) *wh;
    pmax = (float) (Xpixmax);

    scale = pmax / (white - black);

/* Get start index (from 0) */

    ind = (u_medium) *first;
    ind--;
    row += ind;

/* Get modified end position */

    cnt = (u_medium) *last;
    cnt -= ind;

    stp = (u_medium) *step;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;
    pts = ptr;

    if (*bl < *wh)
    {
	if (stp == 1)
	{
	    /* Straight scaled copy */
	    for (ind = cnt; ind > 0; ind--)
	    {
		p = (float) *row++;
		if      (p < black) *ptr++ = 0;
		else if (p > white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
	    }
	}
	else
	{
	    /* Subsampling scaled copy */
	    for (ind = 0; ind < cnt; ind += stp)
	    {
		p = (float) *row;
		if      (p < black) *ptr++ = 0;
		else if (p > white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
		row += stp;
	    }
	}
    }
    else
    {
	if (stp == 1)
	{
	    /* Straight scaled copy */
	    for (ind = cnt; ind > 0; ind--)
	    {
		p = (float) *row++;
		if      (p > black) *ptr++ = 0;
		else if (p < white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
	    }
	}
	else
	{
	    /* Subsampling scaled copy */
	    for (ind = 0; ind < cnt; ind += stp)
	    {
		p = (float) *row;
		if      (p > black) *ptr++ = 0;
		else if (p < white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
		row += stp;
	    }
	}
    }

    w = (ptr - pts);
    draw_note( (int) *ix, (int) *iy, (int) w, (int) 1, True );

    return (F_FALSE);

}

F_logical fsfsro_ (frow, first, last, step, ix, iy, ifr, bl, wh)
F_real *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
F_real *bl, *wh;
{
    u_medium ind, cnt, stp;
    unsigned char *ptr, *pts;
    F_real *row;
    float p, pmax, scale, black, white;
    int f, w;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;
    black = (float) *bl;
    white = (float) *wh;
    pmax = (float) (Xpixmax);

    scale = pmax / (white - black);

/* Get start index (from 0) */

    ind = (u_medium) *first;
    ind--;
    row += ind;

/* Get modified end position */

    cnt = (u_medium) *last;
    cnt -= ind;

    stp = (u_medium) *step;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;
    pts = ptr;

    if (*bl < *wh)
    {
	if (stp == 1)
	{
	    /* Straight scaled copy */
	    for (ind = cnt; ind > 0; ind--)
	    {
		p = (float) *row++;
		if      (p < black) *ptr++ = 0;
		else if (p > white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
	    }
	}
	else
	{
	    /* Subsampling scaled copy */
	    for (ind = 0; ind < cnt; ind += stp)
	    {
		p = (float) *row;
		if      (p < black) *ptr++ = 0;
		else if (p > white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
		row += stp;
	    }
	}
    }
    else
    {
	if (stp == 1)
	{
	    /* Straight scaled copy */
	    for (ind = cnt; ind > 0; ind--)
	    {
		p = (float) *row++;
		if      (p > black) *ptr++ = 0;
		else if (p < white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
	    }
	}
	else
	{
	    /* Subsampling scaled copy */
	    for (ind = 0; ind < cnt; ind += stp)
	    {
		p = (float) *row;
		if      (p > black) *ptr++ = 0;
		else if (p < white) *ptr++ = Xpixmax;
		else                *ptr++ = (unsigned char)
					     (scale * (p - black) + 0.5);
		row += stp;
	    }
	}
    }

    w = (ptr - pts);
    draw_note( (int) *ix, (int) *iy, (int) w, (int) 1, True );

    return (F_FALSE);

}

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION FSBNRI ( ROW, N, IX, IY)
 *       LOGICAL FUNCTION FSINRI ( ROW, N, IX, IY)
 *       LOGICAL FUNCTION FSFNRI ( ROW, N, IX, IY)
 *       LOGICAL FUNCTION FSCNRI ( ROW, N, IX, IY)
 *       -----------------------------------------
 *
 *       PARAMETERS:
 *
 *       byte    (FSBNRI)
 *       integer (FSINRI) row : OUTPUT - destination for data
 *       real    (FSFNRI)
 *       complex (FSCNRI)
 *
 *       integer n : INPUT - number of pixels to read
 *
 *       integer ix,iy : INPUT - The position to read from.
 *
 *       Each Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical fsbnri_ (frow, n, ix, iy, ifr)
unsigned char *frow;
F_integer *n, *ix, *iy, *ifr;
{
    u_medium ind;
    unsigned char *ptr, *row;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    row = frow;
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Straight copy */

    for (ind = (u_medium) *n; ind > 0; ind--) *row++ = *ptr++;

    return (F_FALSE);

}

F_logical fsinri_ (frow, n, ix, iy, ifr)
F_integer *frow;
F_integer *n, *ix, *iy, *ifr;
{
    u_medium ind;
    unsigned char *ptr;
    F_integer *row;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

/* Get pointer to data */

    row = frow;
    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Straight copy */

    for (ind = (u_medium) *n; ind > 0; ind--)
	*row++ = (F_integer) *ptr++;

    return (F_FALSE);

}

F_logical fsfnri_ (frow, n, ix, iy, ifr)
F_real *frow;
F_integer *n, *ix, *iy, *ifr;
{
    u_medium ind;
    unsigned char *ptr;
    F_real *row;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

/* Get pointer to data */

    row = frow;
    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Straight copy */

    for (ind = (u_medium) *n; ind > 0; ind--)
	*row++ = (F_real) *ptr++;

    return (F_FALSE);

}

F_logical fscnri_ (frow, n, ix, iy, ifr)
F_real *frow;
F_integer *n, *ix, *iy, *ifr;
{
    u_medium ind;
    unsigned char *ptr;
    F_real *row;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

/* Get pointer to data */

    row = frow;
    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Straight copy, skipping complex part */

    for (ind = (u_medium) *n; ind > 0; ind--, row++)
	*row++ = (F_real) *ptr++;

    return (F_FALSE);

}

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION FSBSRI ( ROW, N, IX, IY, WHITE, BLACK)
 *       LOGICAL FUNCTION FSISRI ( ROW, N, IX, IY, WHITE, BLACK)
 *       LOGICAL FUNCTION FSFSRI ( ROW, N, IX, IY, WHITE, BLACK)
 *       LOGICAL FUNCTION FSCSRI ( ROW, N, IX, IY, WHITE, BLACK)
 *       -------------------------------------------------------
 *
 *       PARAMETERS:
 *
 *       byte    (FSBSRI)
 *       integer (FSISRI) row : OUTPUT - destination for data
 *       real    (FSFSRI)
 *       complex (FSCSRI)
 *
 *       integer n : INPUT - number of pixels to read
 *
 *       integer ix,iy : INPUT - The position to read from.
 *
 *       real black,white : INPUT - Scaling values
 *
 *       Each Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical fsbsri_ (frow, n, ix, iy, ifr, black, white)
unsigned char *frow;
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
    u_medium ind, xlen;
    unsigned char *ptr, *map;
    unsigned char *row;
    F_real bl, wh, scale, pmax, v;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;

    bl = *black;
    wh = *white;

    if ((!bytValid) || (bl != bytMin) || (wh != bytMax))
    {
	/* Need to recalculate byte lookup table */

	pmax = (F_real) Xpixmax;
	scale = (wh - bl)/pmax;
	map = bytMap;
	xlen = (u_medium) cellCount;

	for (ind = 0; ind < xlen; ind++)
	{
	    v = (scale * (F_real) ind) + bl;
	    if      (v < 0.0)   *map++ = 0;
	    else if (v > 255.0) *map++ = 255;
	    else                *map++ = (unsigned char) (v + 0.5);
	}

	bytMin = bl;
	bytMax = wh;
	bytValid = True;
    }

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Copy via lookup table */

    for (ind = (u_medium) *n; ind > 0; ind--)
	*row++ = bytMap[*ptr++];

    return (F_FALSE);

}

F_logical fsisri_ (frow, n, ix, iy, ifr, black, white)
F_integer *frow;
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
    u_medium ind, xlen;
    unsigned char *ptr;
    F_integer *row;
    F_integer *map, imax;
    F_real bl, wh, scale, pmax, v;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;
    bl = *black;
    wh = *white;

    if ((!intValid) || (bl != intMin) || (wh != intMax))
    {
	/* Need to recalculate integer lookup table */

	imax = 32767;
	pmax = (F_real) imax;
	scale = (wh - bl)/(F_real) Xpixmax;
	map = intMap;
	xlen = (u_medium) cellCount;

	for (ind = 0; ind < xlen; ind++)
	{
	    v = (scale * (F_real) ind) + bl;
	    if      (v < -32768.0) *map++ = -32768;
	    else if (v >  32767.0) *map++ =  32767;
	    else if (v < 0.0 )     *map++ = (F_integer) (v - 0.5);
	    else                   *map++ = (F_integer) (v + 0.5);
	}

	intMin = bl;
	intMax = wh;
	intValid = True;
    }

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Copy via lookup table */

    for (ind = (u_medium) *n; ind > 0; ind--)
	*row++ = intMap[*ptr++];

    return (F_FALSE);

}

F_logical fsfsri_ (frow, n, ix, iy, ifr, black, white)
F_real *frow;
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
    u_medium ind, xlen;
    unsigned char *ptr;
    F_real *map, *row;
    F_real bl, wh, scale;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;

    bl = *black;
    wh = *white;

    if ((!fpValid) || (bl != fpMin) || (wh != fpMax))
    {
	/* Need to recalculate real lookup table */

	scale = (wh - bl)/(F_real) Xpixmax;
	map = fpMap;
	xlen = (u_medium) cellCount;

	for (ind = 0; ind < xlen; ind++)
	    *map++ = (scale * (F_real) ind) + bl;

	fpMin = bl;
	fpMax = wh;
	fpValid = True;
    }

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Copy via lookup table */

    for (ind = (u_medium) *n; ind > 0; ind--)
	*row++ = fpMap[*ptr++];

    return (F_FALSE);

}

F_logical fscsri_ (frow, n, ix, iy, ifr, black, white)
F_real *frow;
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
    u_medium ind, xlen;
    unsigned char *ptr;
    F_real *map, *row;
    F_real bl, wh, scale;
    int f;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	return ( F_TRUE );
    }

    f = (int) *ifr - 1;

    row = frow;

    bl = *black;
    wh = *white;

    if ((!fpValid) || (bl != fpMin) || (wh != fpMax))
    {
	/* Need to recalculate real lookup table */

	scale = (wh - bl)/(F_real) Xpixmax;
	map = fpMap;
	xlen = (u_medium) cellCount;

	for (ind = 0; ind < xlen; ind++)
	    *map++ = (scale * (F_real) ind) + bl;

	fpMin = bl;
	fpMax = wh;
	fpValid = True;
    }

/* Get pointer to data */

    ptr = frameBuffer [ f ];
    ind = (u_medium) *ix + (((u_medium) *iy) * frameVirtWidth);
    ptr += ind;

/* Copy via lookup table */

    for (ind = (u_medium) *n; ind > 0; ind--, row++)
	*row++ = fpMap[*ptr++];

    return (F_FALSE);

}

/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION FSLQ61 ( LUTLEN, LUTMAX )
 *       ------------------------------------------
 *
 *       PARAMETERS:
 *
 *       integer lutlen : OUTPUT - The length of LUT Semper should use.
 *
 *       integer lutmax : OUTPUT - The maximum value for a LUT entry
 *                        Semper should use (from 0).
 *
 *       Returns the number of LUT entries available.  Currently always
 *       assumes that a 7 bit LUT will be available.
 *
 *       Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical fslq61_ ( lutlen, lutmax )

F_integer *lutlen, *lutmax;

{

    /*  Return the data  */

    *lutlen = (F_integer) cellCount;
    *lutmax = (F_integer) 255;

    return  ( F_FALSE );
}
/*
 *----------------------------------------------------------------------
 *
 *       LOGICAL FUNCTION FSAS61 ( NFRS, NFX, NFY, NMX, NMY, NCHX, NCHY,
 *                                 NGP, IWO, IERROR )
 *       ---------------------------------------------------------------
 *
 *       PARAMETERS:
 *
 *       integer nfrs : INPUT/OUTPUT - Defines and returns the number of
 *                      frames to be created.
 *
 *       integer nfx : INPUT/OUTPUT - Defines and returns the horizontal
 *                     size of the frames in pixels
 *
 *       integer nfy : INPUT/OUTPUT - Defines and returns the verti *al
 *                     size of the frames in pixels
 *
 *       integer nmx : INPUT/OUTPUT - Defines and returns the horizontal
 *                     size of the monitor signal in pixels
 *
 *       integer nmy : INPUT/OUTPUT - Defines and returns the horizontal
 *                     size of the monitor signal in pixels
 *
 *       integer nchx : INPUT/OUTPUT - Defines and returns the character
 *                      cell x size in pixels
 *
 *       integer nchy : INPUT/OUTPUT - Defines and returns the character
 *                      cell y size in pixels
 *
 *       integer ngp : INPUT/OUTPUT - Defines and returns the grey-pixel
 *                     block size
 *
 *       integer iwo : INPUT/OUTPUT - Defines and returns the
 *                     "write-only" flag
 *
 *       integer ierror : OUTPUT - the error code, set on error
 *
 *       Creates a window of the requested size.  If too many frames are
 *       requested, then only the maximum number are created, but no error
 *       is generated.  If no size is supplied, defaults are used.  Sets up
 *       all necessary parameters for the window, including loading colour map,
 *       loading a font etc
 *
 *       Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *       code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsas61_ ( nfrs, nfx, nfy, nmx, nmy, nchx, nchy, ngp, iwo, ierror )

F_integer *nfrs, *nfx, *nfy, *nmx, *nmy, *nchx, *nchy, *ngp, *iwo, *ierror;

{
    int sxrupd();

    unsigned int root_width, root_height;
    char *FontName;

    XGCValues values;

    F_integer px,py, cells, saved, depth;

    XColor cell[256];

    XImage *subImage;

    int x, y, n, nf, i, j, monx, mony, best, bclass, index, start, step, grey;
    u_medium f_buf_size, ind, maxind, maps;
    unsigned long red, green, blue, rgb;
    float ramp;
    unsigned char *ptr;
    char *cha;
    unsigned char icha;

    if ( ! termMade )
    {
	show_message ( "Display not supported in batch mode" );
	*ierror = (F_integer) 40;
	return ( F_TRUE );
    }

    if ( *nfrs )
    {
	if ( ! ( *nfrs == 1 || *nfrs == 3 ) )
	{
	    show_message ( "Illegal number of frames requested" );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}

	nf = *nfrs;
    }

    else
    {
	nf = 1;
    }

/*
    See if there is suitable GrayScale/PseudoColor or TrueColor visual
*/

    if ( nf == 1 )
    {
	if ( termVisual.class != GrayScale && termVisual.class != PseudoColor )
	{
	    show_message ( "No support for GrayScale or PseudoColor visuals" );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}

	if ( termVisual.depth < 6 )
	{
	    show_message (
		"Insufficient depth for visual (at least 6 planes needed)" );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}

	frameVisual = termVisual;

	frameColormap = termColormap;
    }

    else
    {
	if ( trueColorVisual.visual == (Visual *) NULL )
	{
	    show_message ( "No support for TrueColor visual" );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}

	if ( trueColorVisual.depth < 24 ) //!= 24 )
	{
	    show_message (
    "Insufficient depth for TrueColor visual (24 bit visual needed)" );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}

	trueColormap =

	    XCreateColormap ( termDisplay,
			      RootWindow ( termDisplay, termScreen ),
			      trueColorVisual.visual, AllocNone );

	frameVisual = trueColorVisual;

	frameColormap = trueColormap;
    }

/*
    Set up overlay look-up table and default overlay colours
*/

    for ( i = 0; i < 8; i++ )
    {
	XovRed   [ i ] = XovInitRed   [ i ];
	XovGreen [ i ] = XovInitGreen [ i ];
	XovBlue  [ i ] = XovInitBlue  [ i ];
    }

/*
    Assign overlays for graphics, rubberbanding and cursor
*/

    XovGraphics   = 0;
    XovRubberband = 6;
    XovCursor     = 7;

/*
    Make all overlays visible
*/

    XovVisibilityMask = 255;

/*
    Set up necessary data structures for selected visual
*/

    if ( frameVisual.class == GrayScale || frameVisual.class == PseudoColor )
    {
/*
    Set up depth value for XCreateImage
*/

	if ( frameVisual.depth > 8 ) frameImageMemoryBits = 16;
	else                         frameImageMemoryBits = 8;

/*
    Set up mapping for colour indices for image and overlays
    (Assign colour indices starting away from Black and skipping
     any of the static menu colours)
*/

	if ( menuMap [ 0 ] < frameVisual.colormap_size / 2 )
	{
	    if ( frameVisual.depth > 8 )
	    {
		start = frameVisual.colormap_size / 2 + 127;
	    }

	    else
	    {
		start = frameVisual.colormap_size - 1;
	    }

	    step = -1;
	}

	else
	{
	    start = 0;

	    step = 1;
	}

	for ( n = 0, index = start; n < cellCount; index+= step )
	{
	    for ( j = 0; j < menuColourCount; j++ )
	    {
		if ( menuMap [ j ] == index ) break;
	    }

	    if ( j == menuColourCount ) XimCell [ n++ ] = index;
	}

	for ( ; n < 256; n++ ) XimCell [ n ] = XimCell [ cellCount - 1 ];

	for ( n = 0; n < 8; index+= step )
	{
	    for ( j = 0; j < menuColourCount; j++ )
	    {
		if ( menuMap [ j ] == index ) break;
	    }

	    if ( j == menuColourCount )
	    {
		for ( i = XovBitMask [ n ]; i < 2 * XovBitMask [ n ]; i++ )
		{
		    XovMap [ i ] = index;
		}

		XovCell [ n++ ] = index;
	    }
	}
    }

    else if ( frameVisual.class == TrueColor )
    {
/*
    Set up depth value for XCreateImage
*/

	frameImageMemoryBits = 32;

/*
    Set up shift values for packing RGB data
*/

	red   = frameVisual.red_mask;
	green = frameVisual.green_mask;
	blue  = frameVisual.blue_mask;

	for ( i = 0; i < 24; i++ )
	{
	    if ( ( red   >> i ) == 255 ) frameTrueColorRedShift   = i ;
	    if ( ( green >> i ) == 255 ) frameTrueColorGreenShift = i ;
	    if ( ( blue  >> i ) == 255 ) frameTrueColorBlueShift  = i ;
	}
    }

/*
    Allow previous frame to go away
*/

    while (frameDying)
    {
	wait_event();
    }

/* Get frame font */

    FontName = frameFontName1;
    frameFont = XLoadQueryFont ( termDisplay, FontName );
    if ( frameFont == NULL )
    {
	FontName = frameFontName2;
	frameFont = XLoadQueryFont ( termDisplay, FontName );
	if ( frameFont == NULL )
	{
	    (void) sprintf ( message, "Can't open fonts '%s' or '%s'",
				      frameFontName1, frameFontName2 );
	    show_message ( message );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}
    }

    if ( frameFont->per_char == NULL ||
	 frameFont->min_bounds.width == frameFont->max_bounds.width)
    {
/*
   Get Font metrics
*/
	frameXleft      = frameFont->min_bounds.lbearing;
	frameCharWidth  = frameFont->max_bounds.rbearing - frameXleft;
	*nchx           = (F_integer) frameCharWidth;
	frameYupper     = frameFont->max_bounds.ascent + 1;
	frameCharHeight = frameFont->max_bounds.descent + frameYupper;
	*nchy           = (F_integer) frameCharHeight;
    }
    else
    {
	(void) sprintf ( message, "Font '%s' is not a true fixed font",
				  FontName );
	show_message ( message );
	*ierror = (F_integer) 40;
	return ( F_TRUE );
    }

    x = (int) *nfx;
    y = (int) *nfy;

    if ( x <= 0 )
    {
       x = 768;
       if ( y <= 0 ) y = 512;
    }
    else if ( y <= 0 ) y = x;

    if ( x > 16384 ) x = 16384;
    if ( y > 16384 ) y = 16384;

    (void) sxrsiz ( x, y );

    monx = (int) *nmx;
    mony = (int) *nmy;

    if (monx <= 0) monx = x;
    if (mony <= 0) mony = y;

    root_width = DisplayWidth( termDisplay, termScreen );
    root_height = DisplayHeight( termDisplay, termScreen );

    i = (3 * root_width) / 4;
    j = (3 * root_height) / 4;

    if (i < monx) monx = i;
    if (j < mony) mony = j;

    f_buf_size = (u_medium) x * (u_medium) y;

    for ( i = 0; i < nf; i++ )
    {
	frameBuffer [ i ] = (unsigned char *) malloc( f_buf_size );

	if ( frameBuffer [ i ] == NULL )
	{
	    while ( --i >= 0 )
	    {
		free ( frameBuffer [ i ] );
		frameBuffer [ i ] = NULL;
	    }

	    show_message ( "Can't allocate enough memory for frame buffer" );
	    *ierror = (F_integer) 40;
	    return ( F_TRUE );
	}

	else
	{
	    ptr = frameBuffer [ i ];
	    for ( ind = 0; ind < f_buf_size; ind++ ) *ptr++ = 0;
	}
    }

    XovBuffer = (unsigned char *) malloc( f_buf_size );

    if ( XovBuffer == NULL )
    {
	for ( i = 0; i < n; i++ )
	{
	    free ( frameBuffer [ i ] );
	    frameBuffer [ i ] = NULL;
	}

	show_message ( "Can't allocate enough memory for overlay buffer" );
	*ierror = (F_integer) 40;
	return ( F_TRUE );
    }

    else
    {
	ptr = XovBuffer;
	for ( ind = 0; ind < f_buf_size; ind++ ) *ptr++ = 0;
    }

/*
    Default for all colour maps is grey-scale ramp
*/

    frameLUT = 1;

    ramp = 255.0 / ( (float) ( cellCount - 1 ) );

    for ( j = 0; j < LUTNUM; j++ )
    {
	for ( i = 0; i < cellCount; i++ )
	{
	    XimRed   [ j ] [ i ] =
	    XimGreen [ j ] [ i ] =
	    XimBlue  [ j ] [ i ] = ramp * ( (float) i ) + 0.5;
	}
    }

    if ( frameVisual.class == GrayScale || frameVisual.class == PseudoColor )
    {
	for ( i = 0; i < cellCount; i++ )
	{
	    cell [ i ].pixel = XimCell [ i ];

	    cell [ i ].red   = (unsigned short) XimRed   [ 0 ] [ i ] << 8;
	    cell [ i ].green = (unsigned short) XimGreen [ 0 ] [ i ] << 8;
	    cell [ i ].blue  = (unsigned short) XimBlue  [ 0 ] [ i ] << 8;

	    cell [ i ].flags = DoRed | DoBlue | DoGreen;
	}

	XStoreColors ( termDisplay, frameColormap, cell, cellCount );

	for ( j = 0; j < 8; j++ )
	{
	    cell [ j ].pixel = XovCell [ j ];

	    if ( frameVisual.class == GrayScale )
	    {
		grey = ( 30 * XovRed   [ j ] +
			 59 * XovGreen [ j ] +
			 11 * XovBlue  [ j ] + 50 ) / 100;

		cell [ j ].red   =
		cell [ j ].green =
		cell [ j ].blue  = (unsigned short) grey << 8;
	    }

	    else
	    {
		cell [ j ].red   = (unsigned short) XovRed   [ j ] << 8;
		cell [ j ].green = (unsigned short) XovGreen [ j ] << 8;
		cell [ j ].blue  = (unsigned short) XovBlue  [ j ] << 8;
	    }

	    cell [ j ].flags = DoRed | DoBlue | DoGreen;
	}

	XStoreColors ( termDisplay, frameColormap, cell, 8 );
    }

    else if ( frameVisual.class == TrueColor )
    {
	for ( i = 0; i < cellCount; i++ )
	{
	    redMap   [ i ] = (unsigned long) XimRed   [ 0 ] [ i ] << frameTrueColorRedShift;
	    greenMap [ i ] = (unsigned long) XimGreen [ 0 ] [ i ] << frameTrueColorGreenShift;
	    blueMap  [ i ] = (unsigned long) XimBlue  [ 0 ] [ i ] << frameTrueColorBlueShift;
	}

	for ( ; i < 256; i++ )
	{
	    redMap   [ i ] = redMap   [ cellCount - 1 ];
	    greenMap [ i ] = greenMap [ cellCount - 1 ];
	    blueMap  [ i ] = blueMap  [ cellCount - 1 ];
	}

	for ( j = 0; j < 8; j++ )
	{
	    red   = (unsigned long) XovRed   [ j ] << frameTrueColorRedShift;
	    green = (unsigned long) XovGreen [ j ] << frameTrueColorGreenShift;
	    blue  = (unsigned long) XovBlue  [ j ] << frameTrueColorBlueShift;

	    rgb = red | green | blue;

	    for ( i = XovBitMask [ j ]; i < 2 * XovBitMask [ j ]; i++ )
	    {
		XovRGB [ i ] = rgb;
	    }
	}
    }

/*
    Window increments are pixels
*/

    frameSizeHints.width_inc  = 1;
    frameSizeHints.height_inc = 1;

/*
   Window size range expected.
   Screen will always be maintained at the maximum size internally
   But may not be visible if the user shrinks it too far!
*/

    frameSizeHints.min_width  =  0;
    frameSizeHints.max_width  =  x;
    frameSizeHints.width  =  monx;

    frameSizeHints.min_height =  0;
    frameSizeHints.max_height =  y;
    frameSizeHints.height =  mony;

    px = (F_integer) (root_width - monx);
    py = (F_integer) (root_height - mony);

    (void) fsx11p_F77 (&px, &py);

    if (px < 0) px = 0;
    frameSizeHints.x = (unsigned int) px;

    if (py < 0) py = 0;
    frameSizeHints.y = (unsigned int) py;

    frameSizeHints.flags  =
	PMinSize | PMaxSize | PPosition | PSize | PResizeInc |
	USSize;

    frameAttributes.bit_gravity = NorthWestGravity;

    if ( frameVisual.class == TrueColor )
    {
	frameAttributes.background_pixel = 0;
	frameAttributes.border_pixel     = frameVisual.red_mask ||
					   frameVisual.green_mask ||
					   frameVisual.blue_mask;
    }

    else
    {
	frameAttributes.background_pixel = XimCell [ 0 ];
	frameAttributes.border_pixel     = XimCell [ cellCount - 1 ];
    }

    frameAttributes.cursor = arrowCursor;
    frameAttributes.colormap = frameColormap;

    frameWin = XCreateWindow( termDisplay,
			      RootWindow ( termDisplay, termScreen ),
			      frameSizeHints.x,     frameSizeHints.y,
			      frameSizeHints.width, frameSizeHints.height,
			      1, frameVisual.depth,
			      InputOutput, frameVisual.visual,
			      CWBitGravity | CWBackPixel | CWBorderPixel |
			      CWCursor | CWColormap, &frameAttributes );

/*    (void) XSetStandardProperties(termDisplay, frameWin,
                                  "Semper 6 plus display",
                                  "S6disp", None, NULL, 0, &frameSizeHints ); */

    (void) XSetStandardProperties(termDisplay, frameWin,
				  PACKAGE_STRING,
				  "S6disp", None, NULL, 0, &frameSizeHints );

    frameWMHints.input         = True;
    frameWMHints.initial_state = NormalState;
    frameWMHints.flags         = InputHint | StateHint;

    (void) XSetWMHints(termDisplay, frameWin, &termWMHints);

    (void) XSelectInput (termDisplay, frameWin, frameMask );

    values.font = frameFont->fid;
    values.foreground = XimCell [ cellCount - 1 ];
    values.background = XimCell [ 0 ];

    frameGC = XCreateGC ( termDisplay, frameWin,
			  GCForeground | GCBackground | GCFont, &values );

    (void) XMapWindow ( termDisplay, frameWin );

    frameIsIcon = True;
    frameExpose = False;
    frameMade = True;
    frameInFocus = False;

    frameCount = nf;
    *nfrs = (F_integer) nf;

    frameVirtWidth = x;
    *nfx = (F_integer) x;

    frameVirtHeight = y;
    *nfy = (F_integer) y;

    *nmx = (F_integer) monx;
    *nmy = (F_integer) mony;

    frameXOffset = (x-monx)/2;
    frameYOffset = (y-mony)/2;

/* Temporarily guess at monitor size */

    frameWidth = monx;
    frameHeight = mony;

    frameDying = False;

    *ngp = (F_integer) 1;
    *iwo = F_FALSE;

    frameStale = False;
    frameLimitI = sxrupd();
    frameLimitV = 0;

    do
    {
	wait_event();
    } while (!frameExpose);

/*
    Read definitions via a pixmap to get a drawable
*/

    x = frameCharWidth * 128;
    charPixmap = XCreatePixmap(termDisplay, frameWin, x, frameCharHeight, 8 );

    values.foreground = 0;
    values.background = 0;
    values.font = frameFont->fid;

    pixmapGC = XCreateGC ( termDisplay, charPixmap,
			  (GCForeground | GCBackground | GCFont), &values );

    (void) XFillRectangle(termDisplay, charPixmap, pixmapGC, 0, 0,
			  x, frameCharHeight);

    (void) XSetForeground(termDisplay, pixmapGC, (unsigned long) 1);

    for ( icha = 0; icha < 128; icha++ )
    {
	(void) XDrawImageString(termDisplay, charPixmap, pixmapGC,
				frameXleft + (frameCharWidth*icha),
				frameYupper, &icha, 1);
    }

    subImage = XGetImage(termDisplay, charPixmap, 0, 0, x, frameCharHeight,
			 AllPlanes, ZPixmap);

    if (subImage == NULL)
    {
	show_message ( "Can't read back character set" );
    }
    else
    {
	f_buf_size = (u_medium) frameCharHeight *
		     (u_medium) frameCharWidth;
	f_buf_size *= 128;

	charBuffer = (unsigned char *) malloc( f_buf_size );
	if ( charBuffer == NULL )
	{
	    show_message ( "Can't allocate enough memory for character set" );
	}
	else
	{
	    cha = subImage->data;
	    ptr = charBuffer;
	    x = frameCharHeight * frameCharWidth;
	    maps = 128 * (u_medium) x;

/*          Copy data with value set to True where foreground  */

	    for (ind=0;ind<maps;ind++) *ptr++ = (*cha++ == 1);
	}

	XDestroyImage ( subImage );
    }

    if (charBuffer == NULL)
    {
	show_message ( "No text annotation will be performed" );
    }

    XFreePixmap ( termDisplay, charPixmap );

    XFreeGC ( termDisplay, pixmapGC );

    return ( F_FALSE );
}

/*
 *      F_logical fsxdcu ( iop, &ierror )
 *
 *      PARAMETERS:
 *
 *      integer iop : INPUT - the operation required:
 *                              1 = draw, 2 = undraw
 *
 *      integer *ierror : OUTPUT - the error code, set on error
 *
 *      Draws the current Semper cursor.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE
 *      with error code set to 40.
 *
 */

F_logical fsxdcu ( iop, ierror )

int iop;
F_integer *ierror;

{
    int delta,x1,x2,y1,y2;
    F_logical status;

    F_integer home,p1,p2;
    F_integer oviop = 3;

#define small_cursor (3)
#define middle_cursor (10)
#define large_cursor (40)

    fsximm = True;
    status = F_TRUE;

    if (fsxsee)
    {
	if (fsxsct > 0)
	{
	    delta = 1;
	    if (fsxsct == 1)
	    {
		delta = small_cursor;
	    }
	    else if (fsxsct == 2)
	    {
		delta = middle_cursor;
	    }
	    else if (fsxsct == 3)
	    {
		delta = large_cursor;
	    }
	    else if (fsxsct == 4)
	    {
		delta = (frameHeight > frameWidth) ? frameHeight : frameWidth;
	    }

	    x1 = fsxcrx - delta;
	    if (x1 < 0) x1 = 0;
	    x2 = fsxcrx + delta;
	    if (x2 >= frameVirtWidth) x2 = frameVirtWidth - 1;
	    y1 = fsxcry - delta;
	    if (y1 < 0) y1 = 0;
	    y2 = fsxcry + delta;
	    if (y2 >= frameVirtHeight) y2 = frameVirtHeight - 1;

	    home = (F_integer) fsxcry;

	    if (fsxcrx > x1)
	    {
		p1 = (F_integer) x1;
		p2 = (F_integer) (fsxcrx - 1);

		if ( iop == 1) status = semdln_F77(&p1,&home,&p2,&home,&oviop);
		else           status = semuln_F77(&p1,&home,&p2,&home,&oviop);

		if (status == F_TRUE)
		{
		    *ierror = 40;
		    return (F_TRUE);
		}
	    }
	    if (fsxcrx < x2)
	    {
		p1 = (F_integer) x2;
		p2 = (F_integer) (fsxcrx + 1);

		if ( iop == 1) status = semdln_F77(&p1,&home,&p2,&home,&oviop);
		else           status = semuln_F77(&p1,&home,&p2,&home,&oviop);

		if (status == F_TRUE)
		{
		    *ierror = 40;
		    return (F_TRUE);
		}
	    }

	    home = (F_integer) fsxcrx;

	    if (fsxcry > y1)
	    {
		p1 = (F_integer) y1;
		p2 = (F_integer) (fsxcry - 1);

		if ( iop == 1) status = semdln_F77(&home,&p1,&home,&p2,&oviop);
		else           status = semuln_F77(&home,&p1,&home,&p2,&oviop);

		if (status == F_TRUE)
		{
		    *ierror = 40;
		    return (F_TRUE);
		}
	    }
	    if (fsxcry < y2)
	    {
		p1 = (F_integer) y2;
		p2 = (F_integer) (fsxcry + 1);

		if ( iop == 1) status = semdln_F77(&home,&p1,&home,&p2,&oviop);
		else           status = semuln_F77(&home,&p1,&home,&p2,&oviop);

		if (status == F_TRUE)
		{
		    *ierror = 40;
		    return (F_TRUE);
		}
	    }

	    if (frameMade) force_update();
	}
	status = F_FALSE;
    }
    fsximm = False;
    return (status);

}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSCC61(IOP,FRAME,IX,IY,IERROR)
 *      -----------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer iop : INPUT - operation required:
 *                              1 = switch on and display
 *                              2 = move
 *                              3 = switch off
 *
 *      integer frame : INPUT - the frame number to use
 *
 *      integer ix,iy : INPUT - the position to display the cursor
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Controls the display and position of the current Semper cursor.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fscc61_ ( iop, frame, ix, iy, ierror )

F_integer *iop, *frame, *ix, *iy, *ierror;

{
    F_logical status;

    int myiop, myix, myiy;

    Window root, child;
    int root_x, root_y, win_x, win_y;
    unsigned int keys_buttons;
    u_medium mymask;

    Bool okay;
    int result;

    status = F_FALSE;

    myiop = (int) *iop;
    myix = (int) *ix;
    myiy = (int) *iy;

    if (frameMade)
    {
	if (myiop == 1)
	{
	    (void) XRaiseWindow (termDisplay, frameWin);
	    if (~fsxsee)
	    {
		fsxvis = False;
/*
	Hide workstation cursor (make blank) and bound to window
*/
		okay = False;
/*
	If pointer not in display then turn off movement events while
	we warp!
*/
		warpingNow = !frameInFocus;
		mymask = frameMask &
			 ~ (PointerMotionHintMask | KeyPressMask |
			    ExposureMask | StructureNotifyMask);
		do
		{
		    result = XGrabPointer ( termDisplay, frameWin, False,
				mymask,
				GrabModeAsync, GrabModeAsync, frameWin,
				blankCursor, CurrentTime );
		    if ( result != GrabNotViewable && result != AlreadyGrabbed )
		    {
			okay = True;
		    }
		    else
		    {
			wait_event();
		    }
		} while (!okay);

		while (!frameInFocus)
		{
		    wait_event();
		}

		if ( result != GrabSuccess )
		{
		    *ierror = (F_integer) 40;
		    return (F_TRUE);
		};

		okay = False;
		do
		{
		    result = XGrabKeyboard ( termDisplay, frameWin, False,
					     GrabModeAsync, GrabModeAsync,
					     CurrentTime );
		    if ( result != GrabNotViewable && result != AlreadyGrabbed )
		    {
			okay = True;
		    }
		    else
		    {
			wait_event();
		    }
		} while (!okay);

		if ( result != GrabSuccess )
		{
/*
		    Ungrab the pointer to prevent a disaster!
*/
		    (void) XUngrabPointer(termDisplay,CurrentTime);
		    *ierror = (F_integer) 40;
		    return (F_TRUE);
		};


		fsxcrx = -1;
		fsxcry = -1;
		fsxsee = True;
	    };
	    myiop = 2;
	};

	if (myiop != 3 || ~fsxsee)
	{
	    if (fsxvis)
	    {
		if ( (myiop != 2) || (myix != fsxcrx) || (myiy != fsxcry) )
		{
/*
		    Undraw cursor at FSXCRX, FSXCRY if new position different
		    or not needed subsequently
*/
		    status = fsxdcu(2, ierror);
		    if (status == F_TRUE) return (F_TRUE);
		    fsxvis = False;
		}
	    };

	    if ((fsxcrx != myix) || (fsxcry != myiy))
	    {
		fsxcrx = myix;
		fsxcry = myiy;
		if (myiop == 2)
		{
/*
    Get up to date if possible - if warping from another window
    this will ignore movements
*/
		    sx11ce_();
/*
    Correct for view position
*/
		    myix -= frameXOffset;
		    myiy -= frameYOffset;
/*
    Is the new position any different from the last one ??
*/

		    if ((frameLastX != myix) || (frameLastY != myiy))
		    {
/*
    Now really ignore Pointer movements
*/
			warpingNow = True;
			frameMask = frameMask &
				    ~(PointerMotionMask|PointerMotionHintMask);
			(void) XSelectInput (termDisplay, frameWin, frameMask);

			(void) XWarpPointer (termDisplay, None, frameWin,
					     0, 0, 0, 0, myix, myiy);

/*
    Find out where it went!
*/

			if ( XQueryPointer (termDisplay, frameWin, &root,
					    &child, &root_x, &root_y,
					    &win_x, &win_y,
					    &keys_buttons) )
			{
			    frameLastX = win_x;
			    frameLastY = win_y;
			}
			else
			{
			    frameLastX = myix;
			    frameLastY = myiy;
			};

			sx11ce_();

/*
    and restore movement!
*/
			frameMask = frameMask | PointerMotionMask;
			(void) XSelectInput (termDisplay, frameWin, frameMask);

			warpingNow = False;

		    }
/*
		    Draw at FSXCRX, FSXCRY
*/
		    status = fsxdcu(1, ierror);
		    if (status == F_TRUE) return (F_TRUE);
		    fsxvis = True;
		}
	    };

	    if (myiop == 3)
	    {
/*
		Restore and unbound workstation cursor
*/
		(void) XUngrabKeyboard(termDisplay,CurrentTime);
		(void) XUngrabPointer(termDisplay,CurrentTime);
		sx11ce_();
		fsxsee = False;
	    };
	}
    }
    if (status == F_TRUE) *ierror = (F_integer) 40;

    fsximm = False;
    return ( status );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSCT61 ( ITYPE, IERROR )
 *      -----------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer itype : INPUT - the cursor type required
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Sets the current Semper cursor type.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsct61_ ( itype, ierror )

F_integer *itype, *ierror;

{
    F_logical status;
    int i;

    status = F_FALSE;

    i = (int) *itype;

    if (frameMade)
    {
	if (fsxsee)
	{
	    if (fsxvis) status = fsxdcu ( 2, ierror );
	}
    }

    if ( i < 0 || i > 4 ) i = 1;
    fsxsct = i;

    if (frameMade)
    {
	if (fsxsee)
	{
	    if (fsxvis)
	    {
		if ( fsxdcu ( 1, ierror ) == F_TRUE )
		    status = F_TRUE;
	    }
	}
    }

    fsximm = False;
    return ( status );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSDE61 ( IERROR )
 *      ----------------------------------
 *
 *      PARAMETERS:
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsde61_ ( ierror )

F_integer *ierror;

{
    if ( frameMade )
    {
	(void) XFreeGC ( termDisplay, frameGC );

	frameDying = True;
	(void) XDestroyWindow ( termDisplay, frameWin );
	frameMade = False;

	if ( trueColormap != NULL )
	{
	    (void) XFreeColormap ( termDisplay, trueColormap );
	    trueColormap = NULL;
	}
    }

    sx11dx_ ( );

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNTION FSER61 (IOP, NX, NY, IX, IY, IFR, IERROR )
 *      -----------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer iop : INPUT - The erasing operation to be carried out
 *
 *      integer nx,ny : INPUT - The size of the "to erase" area
 *
 *      integer ix,iy : INPUT - The top left position of the "to erase" area
 *
 *      integer ifr : INPUT - The frame to be erased
 *
 *      integer ierror : OUTPUT - The error code, set on error
 *
 *      Clears an area of a frame down to zero.  Will clear image,
 *      overlay or both depending on iop.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fser61_ ( iop, nx, ny, ix, iy, ifr, ierror )

F_integer *iop, *nx, *ny, *ix, *iy, *ifr, *ierror;

{
    unsigned char *ptr, *ovp;
    unsigned char oval;
    u_medium offset, x, y, f;

    int xc, yc, i, j;

    if ( *ifr < 1 || *ifr > frameCount )
    {
	show_message ( "Bad frame number" );
	*ierror = (F_integer) 40;
	return ( F_TRUE );
    }

    xc = (int) *nx;
    yc = (int) *ny;
    f  = (int) *ifr - 1;

    if ((xc != 0) && (yc != 0) && frameMade)
    {
	x = (u_medium) *ix;
	y = (u_medium) *iy;
	offset = x + (y * (u_medium) frameVirtWidth);

	oval = ~XovBitMask [ XovGraphics ];

	if (*iop == 1)
	{
	    /* Erase image */
	    for (i = 0; i < yc; i++)
	    {
		ptr = frameBuffer [ f ] + offset;
		offset += frameVirtWidth;
		for (j = 0; j < xc; j++) *ptr++ = 0;
	    }
	}
	else if (*iop == 2)
	{
	    /* Erase overlay */
	    for (i = 0; i < yc; i++)
	    {
		ovp = XovBuffer + offset;
		offset += frameVirtWidth;
		for (j = 0; j < xc; j++) *ovp++ &= oval;
	    }
	}
	else if (*iop == 3)
	{
	    /* Erase both */
	    for (i = 0; i < yc; i++)
	    {
		ptr = frameBuffer [ f ] + offset;
		ovp =  XovBuffer + offset;
		offset += frameVirtWidth;
		for (j = 0; j < xc; j++)
		{
		    *ptr++ = 0;
		    *ovp++ &= oval;
		}
	    }
	}
	draw_note ((int) *ix, (int) *iy, xc, yc, True );
	force_update();
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSEX61 ( IERROR )
 *      ----------------------------------
 *
 *      PARAMETERS:
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Performs session termination requirements
 *
 *      Function returns FALSE if sucessful, otherwise TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsex61_ ( ierror )

F_integer *ierror;

{
    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSFL61 ( IOP, IERROR )
 *      ---------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer iop : INPUT - Flushing operation required.
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsfl61_ ( iop, ierror )

F_integer *iop, *ierror;

{
    if (frameMade) force_update();
    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION X11DLN ( IX2, IY2, IX1, IY1, IVALUE )
 *      ------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer ix2 : INPUT - Defines the x coordinate of the end point
 *                    of the line to be drawn
 *
 *      integer iy2 : INPUT - Defines the y coordinate of the end point
 *                    of the line to be drawn
 *
 *      integer ix1 : INPUT - Defines the x coordinate of the start
 *                    point of the line to be drawn
 *
 *      integer iy1 : INPUT - Defines the y coordinate of the start
 *                    point of the line to be drawn
 *
 *      integer ivalue : INPUT - the value to write (non-zero)
 *
 *      Draws a line in the overlay of the given frame between the given
 *      positions.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical x11dln_ ( ix2, iy2, ix1, iy1, iop )

F_integer *ix2, *iy2, *ix1, *iy1, *iop;
{
    int x, y, x1, x2, y1, y2, dx, dy, cx, cy, frac;
    u_medium offset;
    unsigned char oval;
    unsigned char *ptr;
    Bool noted;

    if (!frameMade) return (F_TRUE);

    noted = False;
    x2 = (int) *ix2;
    y2 = (int) *iy2;
    x1 = (int) *ix1;
    y1 = (int) *iy1;

    switch ( *iop )
    {
	case 1:  oval = XovBitMask [ XovGraphics ];
		 break;

	case 2:  oval = XovBitMask [ XovRubberband ];
		 break;

	case 3:  oval = XovBitMask [ XovCursor ];
		 break;

	default: oval = 0;
    }

    dx = x2 - x1;
    dy = y2 - y1;

    cx = dx < 0 ? -1 : 1;
    cy = dy < 0 ? -1 : 1;

/*
    Three cases treated seperately; Horizontal, Vertical, Diagonal
*/

    if (x1 == x2)
    {
	/* Vertical */

	if ( dy < 0 )
	{
	    dy = -dy;
	    y = y2;
	}
	else y = y1;

	offset = (u_medium) y * (u_medium) frameVirtWidth;
	offset += (u_medium) x1;
	ptr = XovBuffer + offset;

	for (y = 0; y <= dy; y++)
	{
	    *ptr |= oval;
	    ptr += frameVirtWidth;;
	}
    }
    else if (y1 == y2)
    {
	/* Horizontal */

	if ( dx < 0 )
	{
	    dx = -dx;
	    x = x2;
	}
	else x = x1;

	offset = (u_medium) y1 * (u_medium) frameVirtWidth;
	offset += (u_medium) x;
	ptr = XovBuffer + offset;

	for (x = 0; x <= dx; x++) *ptr++ |= oval;
    }
    else
    {
	/* Diagonal */

	noted = True;
	if (dx < 0) dx = -dx;
	if (dy < 0) dy = -dy;

	offset = (u_medium) y1 * (u_medium) frameVirtWidth;
	offset += (u_medium) x1;
	ptr = XovBuffer + offset;

	/* Test slope */

	if (dx >= dy)
	{
	    /* ABS(SLOPE) <= 1; Loop on X */

	    x2 = 0;
	    frac = dx >> 1;
	    y = cy < 0 ? -frameVirtWidth : frameVirtWidth;
	    for (x=0; x<=dx; x++)
	    {
		*ptr |= oval;
		x2++;
		frac += dy;
		if (frac >= dx)
		{
		    if (cx > 0)
		    {
			draw_note ( x1, y1, x2, 1, False );
			x1 += x2;
		    }
		    else
		    {
			x1 -= x2;
			draw_note ( x1+1, y1, x2, 1, False );
		    }
		    x2 = 0;
		    y1 += cy;
		    frac -= dx;
		    ptr += (y + cx);
		}
		else
		{
		    ptr += cx;
		}
	    }

	    if (x2 != 0)
	    {
		if (cx < 0) x1 = x1 - x2 + 1;
		draw_note ( x1, y1, x2, 1, False );
	    }
	}
	else
	{
	    /* ABS(SLOPE) > 1; Loop on Y */

	    y2 = 0;
	    frac = dy >> 1;
	    x = cy < 0 ? -frameVirtWidth : frameVirtWidth;

	    for (y=0; y<=dy; y++)
	    {
		*ptr |= oval;
		y2++;
		frac += dx;
		if (frac >= dy)
		{
		    if (cy > 0)
		    {
			draw_note ( x1, y1, 1, y2, False );
			y1 += y2;
		    }
		    else
		    {
			y1 -= y2;
			draw_note ( x1, y1+1, 1, y2, False );
		    }
		    y2 = 0;
		    x1 += cx;
		    frac -= dy;
		    ptr += (x + cx);
		}
		else
		{
		    ptr += x;
		}
	    }
	    if (y2 != 0)
	    {
		if (cy < 0) y1 = y1 - y2 + 1;
		draw_note ( x1, y1, 1, y2, False );
	    }
	}
    }

    if (!noted)
    {
	if (x1 > x2)
	{
	    x  = x1;
	    x1 = x2;
	    x2 = x;
	}

	if (y1 > y2)
	{
	    y  = y1;
	    y1 = y2;
	    y2 = y;
	}

	x2 -= x1;
	x2++;
	y2 -= y1;
	y2++;
	draw_note ( x1, y1, x2, y2, False );
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION X11ULN ( IX2, IY2, IX1, IY1, IVALUE )
 *      ------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer ix2 : INPUT - Defines the x coordinate of the end point
 *                    of the line to be undrawn
 *
 *      integer iy2 : INPUT - Defines the y coordinate of the end point
 *                    of the line to be undrawn
 *
 *      integer ix1 : INPUT - Defines the x coordinate of the start
 *                    point of the line to be undrawn
 *
 *      integer iy1 : INPUT - Defines the y coordinate of the start
 *                    point of the line to be undrawn
 *
 *      integer ivalue : INPUT - the value to unwrite (non-zero)
 *
 *      Undraws a line in the overlay of the given frame between the given
 *      positions.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE.
 *
 *----------------------------------------------------------------------
 */

F_logical x11uln_ ( ix2, iy2, ix1, iy1, iop )

F_integer *ix2, *iy2, *ix1, *iy1, *iop;

{
    int x, y, x1, x2, y1, y2, dx, dy, cx, cy, frac;
    u_medium offset;
    unsigned char oval;
    unsigned char *ptr;
    Bool noted;

    if (!frameMade) return (F_TRUE);

    noted = False;
    x2 = (int) *ix2;
    y2 = (int) *iy2;
    x1 = (int) *ix1;
    y1 = (int) *iy1;


    switch ( *iop )
    {
	case 1:  oval = ~XovBitMask [ XovGraphics ];
		 break;

	case 2:  oval = ~XovBitMask [ XovRubberband ];
		 break;

	case 3:  oval = ~XovBitMask [ XovCursor ];
		 break;

	default: oval = 255;
    }

    dx = x2 - x1;
    dy = y2 - y1;

    cx = dx < 0 ? -1 : 1;
    cy = dy < 0 ? -1 : 1;

/*
    Three cases treated seperately; Horizontal, Vertical, Diagonal
*/

    if (x1 == x2)
    {
	/* Vertical */

	if ( dy < 0 )
	{
	    dy = -dy;
	    y = y2;
	}
	else y = y1;

	offset = (u_medium) y * (u_medium) frameVirtWidth;
	offset += (u_medium) x1;
	ptr = XovBuffer + offset;

	for (y = 0; y <= dy; y++)
	{
	    *ptr &= oval;
	    ptr += frameVirtWidth;;
	}
    }
    else if (y1 == y2)
    {
	/* Horizontal */

	if ( dx < 0 )
	{
	    dx = -dx;
	    x = x2;
	}
	else x = x1;

	offset = (u_medium) y1 * (u_medium) frameVirtWidth;
	offset += (u_medium) x;
	ptr = XovBuffer + offset;

	for (x = 0; x <= dx; x++) *ptr++ &= oval;
    }
    else
    {
	/* Diagonal */

	noted = True;
	if (dx < 0) dx = -dx;
	if (dy < 0) dy = -dy;

	offset = (u_medium) y1 * (u_medium) frameVirtWidth;
	offset += (u_medium) x1;
	ptr = XovBuffer + offset;

	/* Test slope */

	if (dx >= dy)
	{
	    /* ABS(SLOPE) <= 1; Loop on X */

	    x2 = 0;
	    frac = dx >> 1;
	    y = cy < 0 ? -frameVirtWidth : frameVirtWidth;
	    for (x=0; x<=dx; x++)
	    {
		*ptr &= oval;
		x2++;
		frac += dy;
		if (frac >= dx)
		{
		    if (cx > 0)
		    {
			draw_note ( x1, y1, x2, 1, False );
			x1 += x2;
		    }
		    else
		    {
			x1 -= x2;
			draw_note ( x1+1, y1, x2, 1, False );
		    }
		    x2 = 0;
		    y1 += cy;
		    frac -= dx;
		    ptr += (y + cx);
		}
		else
		{
		    ptr += cx;
		}
	    }

	    if (x2 != 0)
	    {
		if (cx < 0) x1 = x1 - x2 + 1;
		draw_note ( x1, y1, x2, 1, False );
	    }
	}
	else
	{
	    /* ABS(SLOPE) > 1; Loop on Y */

	    y2 = 0;
	    frac = dy >> 1;
	    x = cy < 0 ? -frameVirtWidth : frameVirtWidth;

	    for (y=0; y<=dy; y++)
	    {
		*ptr &= oval;
		y2++;
		frac += dx;
		if (frac >= dy)
		{
		    if (cy > 0)
		    {
			draw_note ( x1, y1, 1, y2, False );
			y1 += y2;
		    }
		    else
		    {
			y1 -= y2;
			draw_note ( x1, y1+1, 1, y2, False );
		    }
		    y2 = 0;
		    x1 += cx;
		    frac -= dy;
		    ptr += (x + cx);
		}
		else
		{
		    ptr += x;
		}
	    }
	    if (y2 != 0)
	    {
		if (cy < 0) y1 = y1 - y2 + 1;
		draw_note ( x1, y1, 1, y2, False );
	    }
	}
    }

    if (!noted)
    {
	if (x1 > x2)
	{
	    x  = x1;
	    x1 = x2;
	    x2 = x;
	}

	if (y1 > y2)
	{
	    y  = y1;
	    y1 = y2;
	    y2 = y;
	}

	x2 -= x1;
	x2++;
	y2 -= y1;
	y2++;
	draw_note ( x1, y1, x2, y2, False );
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE WRITE_LUT ( LUTN )
 *      -----------------------------
 *
 *----------------------------------------------------------------------
 */

void write_lut ( lutn )

int lutn;

{
    XColor cell[256];
    int grey;
    int i, j;

    if ( lutn < 1 || lutn > LUTNUM ) return;

    if ( ! frameMade ) return;

    j = lutn - 1;

    if ( frameVisual.class == GrayScale || frameVisual.class == PseudoColor )
    {
	for ( i = 0; i < cellCount; i++)
	{
	    cell [ i ].pixel = XimCell [ i ];

	    if ( frameVisual.class == GrayScale )
	    {
		grey = ( 30 * XimRed   [ j ] [ i ] +
			 59 * XimGreen [ j ] [ i ] +
			 11 * XimBlue  [ j ] [ i ] + 50 ) / 100;

		cell [ i ].red   =
		cell [ i ].green =
		cell [ i ].blue  = (unsigned short) grey << 8;
	    }

	    else
	    {
		cell [ i ].red   = (unsigned short) XimRed   [ j ] [ i ] << 8;
		cell [ i ].green = (unsigned short) XimGreen [ j ] [ i ] << 8;
		cell [ i ].blue  = (unsigned short) XimBlue  [ j ] [ i ] << 8;
	    }

	    cell [ i ].flags = DoRed | DoBlue | DoGreen;
	}

	XStoreColors ( termDisplay, frameColormap, cell, cellCount );

	XFlush ( termDisplay );
    }

    else if ( frameVisual.class == TrueColor )
    {
	for ( i = 0; i < cellCount; i++ )
	{
	    redMap   [ i ] = (unsigned long) XimRed   [ j ] [ i ] << frameTrueColorRedShift;
	    greenMap [ i ] = (unsigned long) XimGreen [ j ] [ i ] << frameTrueColorGreenShift;
	    blueMap  [ i ] = (unsigned long) XimBlue  [ j ] [ i ] << frameTrueColorBlueShift;
	}

	for ( ; i < 256; i++ )
	{
	    redMap   [ i ] = redMap   [ cellCount - 1 ];
	    greenMap [ i ] = greenMap [ cellCount - 1 ];
	    blueMap  [ i ] = blueMap  [ cellCount - 1 ];
	}

	draw_note ( 0, 0, frameVirtWidth, frameVirtHeight, True );
	force_update ( );
    }
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSLU61 ( IOP, LUTN, MODE, LUTBUF, IERROR )
 *      -----------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer iop : INPUT - Lookup table operation required.
 *
 *      integer lutn : INPUT - The lookup table the operation is to be
 *                     carried out on
 *
 *      integer mode : INPUT/OUTPUT - The mode of the lookup table
 *
 *      integer lutbuf : INPUT/OUTPUT - The buffer which contains/is to
 *                       contain the lookup table information.
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Read or writes Semper lut data from and to internal lut data.
 *      On writing, if the current frame is using the lut which is
 *      being updated, then the colour map for the frame window is
 *      updated.  If a read is requested for a lut which is not loaded,
 *      and the mode is non-zero, then the hardware colourmap is read
 *      back as the LUT.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fslu61_ ( iop, lutn, mode, lutbuf, ierror )

F_integer *iop, *lutn, *mode, *lutbuf, *ierror;

{
    int i, j, mylutn, grey;
    F_integer *lutr, *lutg, *lutb;

    mylutn = (int) *lutn;
    if (mylutn >= LUTNUM) mylutn = LUTNUM;

    j = mylutn - 1;

    if ( *iop == 1 )
    {

	if ( frameMade )
	{
	    for ( i = 0; i < cellCount; i++ ) *lutbuf++ = XimRed [ j ] [ i ];

	    if ( *mode != 1 )
	    {
		for ( i = 0; i < cellCount; i++ ) *lutbuf++ = XimGreen [ j ] [ i ];

		for ( i = 0; i < cellCount; i++ ) *lutbuf++ = XimBlue  [ j ] [ i ];
	    }
	}

	else
	{
	    for ( i = 0; i < cellCount; i++ ) *lutbuf++ = 0;

	    if ( *mode != 1 )
	    {
		for ( i = 0; i < cellCount; i++ ) *lutbuf++ = 0;

		for ( i = 0; i < cellCount; i++ ) *lutbuf++ = 0;
	    }
	}
    }

    else if ( *iop == 2 )
    {

	if ( frameMade )
	{

	    for ( i = 0; i < cellCount; i++ ) XimRed [ j ] [ i ] = *lutbuf++;

	    if ( *mode == 1 )
	    {

		for ( i = 0; i < cellCount; i++ ) XimGreen [ j ] [ i ] = XimRed [ j ] [ i ];

		for ( i = 0; i < cellCount; i++ ) XimBlue  [ j ] [ i ] = XimRed [ j ] [ i ];
	    }

	    else
	    {

		for ( i = 0; i < cellCount; i++ ) XimGreen [ j ] [ i ] = *lutbuf++;

		for ( i = 0; i < cellCount; i++ ) XimBlue  [ j ] [ i ] = *lutbuf++;
	    }

	    if ( *lutn == frameLUT ) write_lut( (int) frameLUT );
	}
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSOI61 ( BUFFER, N, X, Y, FRAME, ICOL, IERROR )
 *      ----------------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer buffer(*) : OUTPUT - The row of data recovered
 *
 *      integer n : INPUT - The number of pixels within the row to
 *                  recover
 *
 *      integer x, y : INPUT - The start position of the row within
 *                     the frame
 *
 *      integer frame : INPUT - The frame from which the row it to be
 *                      recovered
 *
 *      integer icol : INPUT - Flag indicating vertical or horizontal
 *                     recovery for the row
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Recovers a row of pixels from the overlay of the given frame.
 *      Can be recovered either horizontally (true row) or vertically
 *      (column).  The data are returned as zero or one if the overlay
 *      bit is respectively unset or set.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsoi61_ ( buff, n, ix, iy, frame, icol, ierror )

F_integer *buff, *n, *ix, *iy, *frame, *icol, *ierror;

{
    unsigned char *ovp, mask;
    u_medium offset, np;
    F_integer *buffer;

    buffer = buff;

    np = (u_medium) *n;

    if (np > 0)
    {
	offset = (u_medium) *iy * (u_medium) frameVirtWidth;
	offset += (u_medium) *ix;
	ovp = XovBuffer + offset;
	mask = XovBitMask [ XovGraphics ];

	if (*icol == 0)
	{
	    for (;np>0;np--) *buffer++ = ( *ovp++ & mask ) ? 1 : 0;
	}
	else
	{
	    for (;np>0;np--,ovp += frameVirtWidth)
	    {
		*buffer++ = ( *ovp & mask ) ? 1 : 0;
	    }
	}
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSOO61 ( BUFFER, N, X, Y, FRAME, ICOL, IERROR )
 *      ----------------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer buffer(*) : OUTPUT - The row of data recovered
 *
 *      integer n : INPUT - The number of pixels within the row to
 *                  recover
 *
 *      integer x, y : INPUT - The start position of the row within
 *                     the frame
 *
 *      integer frame : INPUT - The frame from which the row it to be
 *                      recovered
 *
 *      integer icol : INPUT - Flag indicating vertical or horizontal
 *                     recovery for the row
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Recovers a row of pixels from the overlay of the given frame.
 *      Can be recovered either horizontally (true row) or vertically
 *      (column).  The data are returned as zero or one if the overlay
 *      bit is respectively unset or set.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      code set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsoo61_ ( buff, n, ix, iy, frame, icol, ierror )

F_integer *buff, *n, *ix, *iy, *frame, *icol, *ierror;

{
    unsigned char *ovp;
    u_medium offset, np;
    F_integer *buffer;
    int nx, ny;
    unsigned char on, off;

    np = (u_medium) *n;

    if (np > 0)
    {
	buffer = buff;

	on  =  XovBitMask [ XovGraphics ];
	off = ~XovBitMask [ XovGraphics ];

	offset = (u_medium) *iy * (u_medium) frameVirtWidth;
	offset += (u_medium) *ix;
	ovp = XovBuffer + offset;

	if (*icol == 0)
	{
	    for (;np>0;np--)
	    {
		if ( *buffer++ ) *ovp++ |= on;
		else             *ovp++ &= off;
	    }
	    nx = *n;
	    ny =  1;
	}
	else
	{
	    for (;np>0;np--)
	    {
		if ( *buffer++ ) *ovp |= on;
		else             *ovp &= off;

		ovp += frameVirtWidth;
	    }
	    nx =  1;
	    ny = *n;
	}
	draw_note ((int) *ix, (int) *iy, nx, ny, True );
    }

    return ( F_FALSE );
}

F_logical fsov61_ ( IOP, RED, GREEN, BLUE, VISIB, GOVER, ROVER, COVER, IERROR )
/*===========================================================================*/

F_integer *IOP;
F_integer RED [ ];
F_integer GREEN [ ];
F_integer BLUE [ ];
F_integer VISIB [ ];
F_integer *GOVER;
F_integer *ROVER;
F_integer *COVER;
F_integer *IERROR;
{
    int opcode;
    int mask = 0;
    int grey;
    int i, j;
    unsigned long red, green, blue, rgb;
    int colour_change = FALSE;
    int visibility_change;
    XColor cell;

/*
 *  Check display window exists and fault bad data arguments
*/
    if ( ! frameMade ||
	 *IOP < 1 || *IOP > 2 )
    {
	*IERROR = 40; return ( F_TRUE );
    }

    opcode = *IOP;
/*
 *  Switch on specified opcode
*/
    switch ( opcode )
    {
/*
 *  Return overlay set-up
*/
	case 1:
	{
/*
 *  Return overlay colours and visibility
*/
	    for ( i = 0; i < 8; i++ )
	    {
		RED   [ i ] = XovRed   [ i ];
		GREEN [ i ] = XovGreen [ i ];
		BLUE  [ i ] = XovBlue  [ i ];

		VISIB [ i ] = XovVisibilityMask & XovBitMask [ i ] ? 1 : 0;
	    }
/*
 *  Return graphics/rubberband/cursor overlay planes
*/
	    *GOVER = XovGraphics   + 1;
	    *ROVER = XovRubberband + 1;
	    *COVER = XovCursor     + 1;
	}
	break;
/*
 *  Change overlay set-up
*/
	case 2:
	{
/*
 *  Fault any bad colour components
*/
	    for ( i = 0; i < 8; i++ )
	    {
		if ( RED   [ i ] < 0 || RED   [ i ] > 255 ||
		     GREEN [ i ] < 0 || GREEN [ i ] > 255 ||
		     BLUE  [ i ] < 0 || BLUE  [ i ] > 255 )
		{
		    *IERROR = 40; return ( F_TRUE );
		}
	    }
/*
 *  Fault any bad overlay plane numbers
*/
	    if ( *GOVER < 1 || *GOVER > 8 ||
		 *ROVER < 1 || *ROVER > 8 ||
		 *COVER < 1 || *COVER > 8 ||
		 *GOVER == *ROVER || *ROVER == *COVER || *COVER == *GOVER )
	    {
		*IERROR = 40; return ( F_TRUE );
	    }
/*
 *  Examine new overlay colours and visibility
*/
	    for ( j = 0; j < 8; j++ )
	    {
/*
 *  See if colour has changed
*/
		if ( RED   [ j ] != XovRed   [ j ] ||
		     GREEN [ j ] != XovGreen [ j ] ||
		     BLUE  [ j ] != XovBlue  [ j ] )
		{
/*
 *  Store new colour in colour table
*/
		    XovRed   [ j ] = RED   [ j ];
		    XovGreen [ j ] = GREEN [ j ];
		    XovBlue  [ j ] = BLUE  [ j ];
/*
 *  Update the hardware colour map
*/
		    if ( frameVisual.class == GrayScale ||
			 frameVisual.class == PseudoColor )
		    {
			cell.pixel = XovCell [ j ];

			if ( frameVisual.class == GrayScale )
			{
			    grey = ( 30 * XovRed   [ j ] +
				     59 * XovGreen [ j ] +
				     11 * XovBlue  [ j ] + 50 ) / 100;

			    cell.red   =
			    cell.green =
			    cell.blue  = (unsigned short) grey << 8;
			}

			else
			{
			    cell.red   = (unsigned short) XovRed   [ j ] << 8;
			    cell.green = (unsigned short) XovGreen [ j ] << 8;
			    cell.blue  = (unsigned short) XovBlue  [ j ] << 8;
			}

			cell.flags = DoRed | DoBlue | DoGreen;

			XStoreColor ( termDisplay, frameColormap, &cell );

			XFlush ( termDisplay );
		    }
/*
 *  Update red/green/blue colour maps
*/
		    else if ( frameVisual.class == TrueColor )
		    {
			red   = (unsigned long) XovRed   [ j ];
			green = (unsigned long) XovGreen [ j ];
			blue  = (unsigned long) XovBlue  [ j ];

			red   <<= frameTrueColorRedShift;
			green <<= frameTrueColorGreenShift;
			blue  <<= frameTrueColorBlueShift;

			rgb = red | green | blue;

			for ( i = XovBitMask [ j ]; i < 2 * XovBitMask [ j ]; i++ )
			{
			    XovRGB [ i ] = rgb;
			}
		    }
/*
 *  Note the change in colour
*/
		    colour_change = TRUE;
		}
/*
 *  Build up mask from overlay visibility
*/
		if ( VISIB [ j ] != 0 ) mask |= XovBitMask [ j ];
	    }
/*
 *  See if overlay visibility has changed
*/
	    visibility_change = ( mask != XovVisibilityMask );
/*
 *  Store new overlay visibility mask
*/
	    XovVisibilityMask = mask;
/*
 *  Record the new settings for the graphics/rubberband/cursor overlays
*/
	    XovGraphics   = *GOVER - 1;
	    XovRubberband = *ROVER - 1;
	    XovCursor     = *COVER - 1;
/*
 *  If necessary, redraw the display window to reflect the changes made
*/
	    if ( ( colour_change && frameVisual.class == TrueColor ) ||
		 visibility_change )
	    {
		draw_note ( 0, 0, frameVirtWidth, frameVirtHeight, True );
		force_update ( );
	    }
	}
    }

    return ( F_FALSE );

/*  Copyright (C) 1994:  Synoptics Ltd,  All Rights Reserved  */
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION X11DTX ( IBUF, N, IX, IY, BACK, INVERT)
 *      --------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer ibuf : INPUT - The text to be drawn, in internal form
 *
 *      integer n : INPUT - The length of the text string, in characters
 *
 *      integer ix : INPUT - The x coordinate of the centre of the text
 *
 *      integer iy : INPUT - The y coordinate of the centre of the text
 *
 *      logical back : INPUT - Paint in background if TRUE
 *
 *      logical normal : INPUT - Use normal video if TRUE else inverse
 *                       (Not implemented at present)
 *
 *      Draws text in the overlay at the given position.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE
 *
 *----------------------------------------------------------------------
 */

F_logical x11dtx_ ( tbuf, n, ix, iy, back, normal )

F_integer *tbuf, *n, *ix, *iy;
F_logical *back, *normal;

{
    unsigned char on, off;
    unsigned char *ovp, *keep, *hold;
    u_medium offset, nc, wc, ws, ind;
    F_integer *iptr, *ibuf;
    int rows,cols,ch;

    if ( charBuffer == NULL ) return ( F_FALSE );

    ibuf = tbuf;

    if (!frameMade) return (F_TRUE);

    nc = (u_medium) *n;
    wc =  nc * (u_medium) frameCharWidth;
    ws = 128 * (u_medium) frameCharWidth;

    if (nc > 0)
    {
	on  =  XovBitMask [ XovGraphics ];
	off = ~XovBitMask [ XovGraphics ];

	offset = (u_medium) *iy * (u_medium) frameVirtWidth;
	offset += (u_medium) *ix;
	ovp = XovBuffer + offset;
	keep = ovp;
	offset = 0;
/*
	Loop over all rows of the character cells
*/
	for (rows = 0; rows < frameCharHeight; rows++)
	{
/*
	Loop over all characters
*/
	    iptr = ibuf;
	    for (ind = 0; ind < nc; ind++)
	    {
		ch = (int) *iptr++;
		ch &= 127;              /* Mask to 0-127 */
		hold  = charBuffer + offset;
		hold += (u_medium) ch * (u_medium) frameCharWidth;
/*
	Loop over each column of the cell
*/
		if (*back == F_TRUE)
		{
		    for (cols = 0; cols < frameCharWidth; cols++)
		    {
			if ( *hold++ ) *ovp++ |= on;
			else           *ovp++ &= off;
		    }
		}
		else
		{
		    for (cols = 0; cols < frameCharWidth; cols++)
		    {
			if ( *hold++ ) *ovp++ |= on;
			else            ovp++;
		    }
		}
	    };
/*
	Next row of charset
*/
	    offset += ws;
	    keep += frameVirtWidth;
	    ovp = keep;
	};

	draw_note ( (int) *ix, (int) *iy,
		    (int) wc, (int) frameCharHeight, False );
    }

    return ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSVQ61(FRAME,ZOOM,XMIN,XMAX,YMIN,YMAX,IERROR)
 *      --------------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer frame : OUTPUT - The current frame
 *
 *      integer zoom : OUTPUT - The zoom factor to be applied
 *
 *      integer iblank : INPUT - The "blank outside area" flag
 *
 *      integer xmin : OUTPUT - The leftmost pixel visible
 *
 *      integer xmax : OUTPUT - The rightmost pixel visible
 *
 *      integer ymin : OUTPUT - The topmost pixel visible
 *
 *      integer ymax : OUTPUT - The bottommost pixel visible
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Returns viewing conditions.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsvq61_ ( frame, zoom, xmin, xmax, ymin, ymax, ierror )

F_integer *frame, *zoom, *xmin, *xmax, *ymin, *ymax, *ierror;

{
    F_logical status;

    if ( frameMade )
    {
	sx11ce_();
	*frame = (F_integer) 1;
	*zoom  = (F_integer) 1;
	*xmin  = (F_integer) frameXOffset;
	*ymin  = (F_integer) frameYOffset;
	*xmax  = (F_integer) (frameWidth + frameXOffset - 1);
	*ymax  = (F_integer) (frameHeight + frameYOffset - 1);
	status = F_FALSE;
    }
    else
    {
	*ierror = (F_integer) 40;
	status = F_TRUE;
    };

    return ( status );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION FSVW61 ( LUTN, IZOOM, IBLANK, NX, NY, IX, IY,
 *                                MX, MY, IFR, IERROR )
 *      -----------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer lutn : INPUT - The lookup table to be activated
 *
 *      integer izoom : INPUT - The zoom factor to be applied
 *
 *      integer iblank : INPUT - The "blank outside area" flag
 *
 *      integer nx, ny : INPUT - The size of the frame area
 *
 *      integer ix, iy : INPUT - The top left position of the frame
 *
 *      integer mx, my : INPUT - The pixel of the frame to be centred
 *                               on the screen
 *
 *      integer ifr : INPUT - The frame to be viewed
 *
 *      integer ierror : OUTPUT - the error code, set on error
 *
 *      Establishes viewing conditions.  Causes, if necessary, the appropriate
 *      lut to be activated into window.
 *
 *      Function returns F_FALSE if sucessful, otherwise F_TRUE with error
 *      set to 40.
 *
 *----------------------------------------------------------------------
 */

F_logical fsvw61_ ( lutn, izoom, iblank, nx, ny, ix, iy, mx, my, ifr, ierror )

F_integer *lutn, *izoom, *iblank, *nx, *ny, *ix, *iy, *mx, *my, *ifr, *ierror;

{
    F_logical status;
    F_integer mode, iop, lutbuf[3 * 256];

#define semlut_F77 F77_FUNC(semlut,SEMLUT)
    F_logical semlut_F77 ();

    status = F_FALSE;

    if ( frameMade )
    {
/*
    Handle LUT changes
*/
	if (*lutn != frameLUT)
	{
	/* Need to load a different LUT */
	    if ((int) *lutn >= LUTNUM)
	    {
		/* Need to get lut from Semper */

		iop = (F_integer) 1;
		status = semlut_F77 ( &iop, lutn, &mode, lutbuf );
		if (status == F_FALSE)
		{
		    iop = (F_integer) 2;
		    status = fslu61_ ( &iop, lutn, &mode, lutbuf, ierror );
		}
	    }
	    if (status == F_FALSE)
	    {
		frameLUT = *lutn;
		write_lut( (int) frameLUT );
	    }
	}
/*
	Change view centre
*/
	move_virt((int) *mx, (int) *my, False);

	(void) XRaiseWindow (termDisplay, frameWin);
	(void) XFlush(termDisplay);
	sx11ce_();
    }

    return ( status );

}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXPO
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Opens the pointer device.
 *
 *----------------------------------------------------------------------
 */

void eqxxpo_ ()

{
	termMask = termMask & ~PointerMotionHintMask;
	if (termMade)
	    (void) XSelectInput (termDisplay, termWin, termMask );
	frameMask = frameMask & ~PointerMotionHintMask;
	if (frameMade)
	    (void) XSelectInput (termDisplay, frameWin, frameMask );
	menuMask = menuMask & ~PointerMotionHintMask;
	if (menuMade)
	    (void) XSelectInput (termDisplay, menuWin, menuMask );
}
/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXPC
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Closes the pointer device.
 *
 *----------------------------------------------------------------------
 */

void eqxxpc_ ()

{
	termMask = termMask | PointerMotionHintMask;
	if (termMade)
	    (void) XSelectInput (termDisplay, termWin, termMask );
	frameMask = frameMask | PointerMotionHintMask;
	if (frameMade)
	    (void) XSelectInput (termDisplay, frameWin, frameMask );
	menuMask = menuMask | PointerMotionHintMask;
	if (menuMade)
	    (void) XSelectInput (termDisplay, menuWin, menuMask );
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXKO
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Opens the keyboard queue.
 *
 *----------------------------------------------------------------------
 */

void eqxxko_ ()

{
}
/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXKC
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Closes the keyboard queue.
 *
 *----------------------------------------------------------------------
 */

void eqxxkc_ ()

{
}
/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE EQXXPK
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Polls the keyboard.  If anything is available to be read from
 *      the tty window, it reads and decodes such input.
 *
 *----------------------------------------------------------------------
 */

void eqxxpk_ ()

{
    sx11ce_ ();
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE TERSIZ
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Returns the size of the tty window.
 *
 *----------------------------------------------------------------------
 */

void tersiz_ ( xsize, ysize )

F_integer *xsize, *ysize;

{
    if (termMade)
    {
	*xsize = (F_integer) termWidth;
	*ysize = (F_integer) termHeight;
    }
    else
    {
	*xsize = (F_integer) 79;
	*ysize = (F_integer) 24;
    }
}

/****************** UIF SUPPORT *******************/

static int uifsvx, uifsvy;

void mapMenu()

{
    unsigned char *ptr;
    int i;
    XGCValues values;

    if (!menuMade)
    {
	menuVisual = termVisual;

	menuColormap = termColormap;

	menuSizeHints.width  =  menuWidth  * menuCharWidth;
	menuSizeHints.height =  menuHeight * menuCharHeight;

	menuAttributes.bit_gravity = NorthWestGravity;
	menuAttributes.background_pixel = menuMap [ 0 ];
	menuAttributes.border_pixel     = menuMap [ 1 ];
	menuAttributes.cursor = arrowCursor;
	menuAttributes.colormap = menuColormap;

	menuWin = XCreateWindow ( termDisplay,
				  RootWindow ( termDisplay, termScreen ),
				  menuSizeHints.x,     menuSizeHints.y,
				  menuSizeHints.width, menuSizeHints.height,
				  1, menuVisual.depth,
				  InputOutput, menuVisual.visual,
				  CWBitGravity | CWBackPixel | CWBorderPixel |
				  CWColormap | CWCursor, &menuAttributes );

	(void) XSetStandardProperties (termDisplay, menuWin,
				       "Semper 6 plus menus",
				       "S6menu", None, NULL, 0,
				       &menuSizeHints );

	menuWMHints.input         = True;
	menuWMHints.initial_state = NormalState;
	menuWMHints.flags         = InputHint | StateHint;

	(void) XSetWMHints(termDisplay, menuWin, &menuWMHints);

	values.font = menuFont->fid;
	values.foreground = menuMap [ 1 ];
	values.background = menuMap [ 0 ];

	menuGC = XCreateGC ( termDisplay, menuWin,
			    (GCForeground | GCBackground | GCFont), &values );

/*
   Initialise line pointers and clear screen
*/

	menuIsIcon = True;              /* until mapped */
	menuExpose = False;             /* one shot flag */
	menuAtt    = 1;                 /* normal */
	menuXpos   = 0;                 /* Cursor write position */
	menuYpos   = 0;

	for ( i = 0; i < lines_per_page; i++ )
		menu_pointers[i] = i * chars_per_line;

	ptr = &menu_screen_map[0];
	for ( i = 0; i < chars_per_page; i++ ) *ptr++ = ' ';

	ptr = &menu_screen_att[0];
	for ( i = 0; i < chars_per_page; i++ ) *ptr++ = menuAtt;

	(void) XSelectInput (termDisplay, menuWin, menuMask );

/*
    Now map the window
*/

	(void) XMapWindow(termDisplay, menuWin);

	menuInFocus = False;

	menuMade = True;
    }
}


/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE DHOCQU ( X, Y )
 *      --------------------------
 *
 *      PARAMETERS:
 *
 *      integer x : OUTPUT - horizontal character position of pointer
 *
 *      integer y : OUTPUT - vertical character position of pointer
 *
 *----------------------------------------------------------------------
 */

void dhocqu_ ( x, y )

F_integer *x, *y;

{
    *x = (F_integer) (menuLastX / menuCharWidth);
    *y = (F_integer) (menuLastY / menuCharHeight);
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE DHOCMO ( X, Y )
 *      --------------------------
 *
 *      PARAMETERS:
 *
 *      integer x : INPUT - horizontal character position of pointer
 *
 *      integer y : INPUT - vertical character position of pointer
 *
 *----------------------------------------------------------------------
 */

void dhocmo_ ( x, y )

F_integer *x, *y;

{
    int ax, ay, ox, oy, result;
    Bool okay;
    u_medium mymask;

    ax = ((int) *x) * menuCharWidth;
    ax += (menuCharWidth>>1);
    ay = ((int) *y) * menuCharHeight;
    ay += (menuCharHeight>>1);

    (void) XRaiseWindow (termDisplay, menuWin);
    okay = False;
    mymask = menuMask &
	     ~ (PointerMotionHintMask | KeyPressMask |
		ExposureMask | StructureNotifyMask);
    do
    {
	result = XGrabPointer ( termDisplay, menuWin, False, mymask,
				GrabModeAsync, GrabModeAsync, menuWin,
				None, CurrentTime );
	if ( result != GrabNotViewable && result != AlreadyGrabbed )
	{
	    okay = True;
	}
	else
	{
	    wait_event();
	}
    } while (!okay);

/*
    Did we get the pointer ?
*/

    if ( result == GrabSuccess )
    {
	okay = False;
	do
	{
	    result = XGrabKeyboard ( termDisplay, menuWin, False,
				     GrabModeAsync, GrabModeAsync,
				     CurrentTime );
	    if ( result != GrabNotViewable && result != AlreadyGrabbed )
	    {
		okay = True;
	    }
	    else
	    {
		wait_event();
	    }
	} while (!okay);

	if ( result == GrabSuccess )
	{

	    ox = menuLastX;
	    oy = menuLastY;
	    sx11ce_();
/*
	Note recent changes
*/
	    ox = menuLastX - ox;
	    oy = menuLastY - oy;
/*
	Add in recent changes
*/
	    ax = ax + ox;
	    ay = ay + oy;
	    (void) XWarpPointer( termDisplay, None, menuWin, 0,0, 0,0, ax,ay );

	    menuLastX = ax;
	    menuLastY = ay;

	    (void) XFlush ( termDisplay );
	    sx11ce_();

	    (void) XUngrabKeyboard(termDisplay,CurrentTime);
	};

	(void) XUngrabPointer(termDisplay,CurrentTime);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE DHOCSA
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Save the current cursor state.
 *
 *----------------------------------------------------------------------
 */

void dhocsa_ ()

{
    uifsvx = menuLastX;
    uifsvy = menuLastY;
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE DHOCRE
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Restores the saved cursor state.
 *
 *----------------------------------------------------------------------
 */

void dhocre_ ()

{
    F_integer cd, x, y;

    x = (F_integer) (uifsvx / menuCharWidth);
    y = (F_integer) (uifsvy / menuCharHeight);

    dhocmo_(&x, &y);

/*
    Ensure UIF knows...
*/
    cd = 1;
    x = (F_integer) uifsvx;
    y = (F_integer) uifsvy;
    uixxrm_F77 (&cd,&x,&y);
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE DHOCON
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Turn on normal cursor
 *
 *----------------------------------------------------------------------
 */

void dhocon_ ()

{
    (void) XDefineCursor(termDisplay, menuWin, arrowCursor);
    (void) XFlush(termDisplay);
}

/*
 *----------------------------------------------------------------------
 *
 *      SUBROUTINE DHOCOF
 *      -----------------
 *
 *      PARAMETERS:
 *
 *      None.
 *
 *      Turn on busy (off) cursor
 *
 *----------------------------------------------------------------------
 */

void dhocof_ ()

{
    (void) XDefineCursor(termDisplay, menuWin, blankCursor);
    (void) XFlush(termDisplay);
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION DHODSI ( XMIN, YMIN, XSIZE, YSIZE, MXSCA,
 *                                MYSCA, XOFF, YOFF, NCOLS )
 *      ----------------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer xmin, ymin : OUTPUT - The minimum coordinates available
 *                                    on the device.
 *
 *      integer xsize, ysize : OUTPUT - The size of the device.
 *
 *      integer mxsca, mysca : OUTPUT - The mouse to screen scale.
 *
 *      integer xoff, yoff : OUTPUT - Offset to be added to device
 *                                    coordinates for this device
 *
 *      integer ncols : OUTPUT - The number of colours supported.
 *
 *      Returns the minimum coordiates, size, mouse scale, device
 *      offset and number of colours of the host display device.
 *
 *      Function returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical dhodsi_ ( xmin,ymin,xsize,ysize,mxsca,mysca,xoff,yoff,ncols )

F_integer *xmin,*ymin,*xsize,*ysize,*mxsca,*mysca,*xoff,*yoff,*ncols;

{

/*
    Set the device dimensions for caller
*/
    *xsize = (F_integer) init_width;
    *ysize = (F_integer) init_height;

    *xmin = 0;
    *ymin = 0;
/*
    And the device offset
*/
    *xoff = 0;
    *yoff = 0;
/*
    And the mouse scaling factor
*/
    *mxsca = (F_integer) menuCharWidth;
    *mysca = (F_integer) menuCharHeight;
/*
    And the number of colours
*/
    *ncols = (F_integer) menuColourCount;

    return  ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION DHODST ( DUMMY )
 *      ---------------------------------
 *
 *       PARAMETERS:
 *
 *       integer dummy : INPUT - Dummy argument.
 *
 *       Initialises the host display device.
 *
 *       Function returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical dhodst_ ( dummy )

F_integer *dummy;

{
    char *FontName;

    if (!menuLoaded)
    {

	/* Get menu font */

	FontName = menuFontName1;
	menuFont = XLoadQueryFont ( termDisplay, FontName );
	if ( menuFont == NULL )
	{
	    FontName = menuFontName2;
	    menuFont = XLoadQueryFont ( termDisplay, FontName );
	    if ( menuFont == NULL )
	    {
	    (void) fprintf ( stderr, "Can't open fonts '%s' or '%s'\n",
				     menuFontName1, menuFontName2 );
		return ( F_TRUE );
	    }
	}

	if ( menuFont->per_char == NULL ||
	     menuFont->min_bounds.width == menuFont->max_bounds.width)
	{
/*
   Get Font metrics and use them to determine window size required
*/
	    menuXleft  = menuFont->min_bounds.lbearing;
	    menuCharWidth  = menuFont->max_bounds.rbearing - menuXleft;
	    menuYupper = menuFont->max_bounds.ascent;
	    menuCharHeight = menuFont->max_bounds.descent + menuYupper;

/*
   Window increments are character cells
*/
	    menuSizeHints.width_inc  = menuCharWidth;
	    menuSizeHints.height_inc = menuCharHeight;

/*
   Window size range expected.
   Screen will always be maintained at the maximum size internally
   But may not be visible if the user shrinks it too far!
*/

	    menuSizeHints.min_width  =  0;
	    menuSizeHints.max_width  =  chars_per_line * menuCharWidth;
	    menuSizeHints.min_height =  0;
	    menuSizeHints.max_height =  lines_per_page * menuCharHeight;

	    menuSizeHints.x = 0;
	    menuSizeHints.y = 0;

	    menuSizeHints.flags  =
		PMinSize | PMaxSize | PPosition | PSize | PResizeInc |
		USSize;

	}
	else
	{
	(void) fprintf ( stderr, "Font '%s' is not a true fixed font\n",
			 FontName );
	    return ( F_TRUE );
	}

	menuLoaded = True;
	menuInFocus = False;
    }

    mapMenu();

/*
    Window mapped - wait for it to appear
*/

    dhocon_();
    do
    {
	wait_event();
    } while (!menuExpose);

    return  ( F_FALSE );
}

void unmapMenu()
{
    if ( menuMade )
    {
	(void) XFreeGC ( termDisplay, menuGC );

	menuDying = True;
	(void) XDestroyWindow ( termDisplay, menuWin );
	menuMade = False;

	do
	{
	    wait_event();
	} while (menuDying);
    }
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION DHODSH ( DUMMY )
 *      ---------------------------------
 *
 *       PARAMETERS:
 *
 *       integer dummy : INPUT - Dummy argument.
 *
 *       Terminates use of the host display device.
 *
 *       Function returns TRUE in case of error, otherwise FALSE.
 *
 *----------------------------------------------------------------------
 */

F_logical dhodsh_ ( dummy )

F_integer *dummy;

{
    unmapMenu();
    return  ( F_FALSE );
}

/*
 *----------------------------------------------------------------------
 *
 *      LOGICAL FUNCTION DHOSCR ( XPOS, YPOS, XSIZE, YSIZE )
 *      ----------------------------------------------------
 *
 *      PARAMETERS:
 *
 *      integer xpos, ypos : INPUT - the position (device coordinates)
 *                                   of the top left of the scrolling area.
 *
 *      integer xsize, ysize : INPUT - the size of the scrolling area.
 *
 *      Sets scrolling to be limited to an area of the host display
 *      device
 *
 *----------------------------------------------------------------------
 */

F_logical dhoscr_ ( xpos, ypos, xsize, ysize )
F_integer *xpos, *ypos, *xsize, *ysize;

{
    if ( menuMade )
    {
	/*  Resize menu window to end on ypos ?? */

	menuHeight = (int) *ypos;

	unmapMenu();
	mapMenu();

	dhocon_();

	do
	{
	    wait_event();
	} while (!menuExpose);
    };

    return  ( F_FALSE );
}

/* Include the buffer routines at the end */
#include "fsbuff.h"

