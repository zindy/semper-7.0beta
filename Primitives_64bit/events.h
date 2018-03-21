/*-----------------------------------------------------------------------
 *  The following #defines MUST match those defined in the FORTRAN
 *  include file 'events'
 */

/*
  Standard sources
*/

#define MANYS   0
#define MBREAK (MANYS+1)

/*  We cannot use MKEY as this clashes the a 4SIGHT queue device.  */

#define SEMPER_MKEY (MBREAK+1)
#define MPOINT (SEMPER_MKEY+1)
#define MBUT   (MPOINT+1)

/*
  Standard arguments

  QRUN = Allow events
  QWAIT = Disable events but maintain queue activity
  QOPEN = Flush queue and allow events
  QCLOSE = Disable events with no queue activity

*/

#define QRUN    0
#define QWAIT  (QRUN+1)			
#define QOPEN  (QWAIT+1)
#define QCLOSE (QOPEN+1)

/*
  QTAKE = Read and remove queue head entry
  QLOOK = Read but keep queue entry, point to next entry
  QSNAP = Examine current source status (e.g. mouse position)
*/

#define QTAKE  0
#define QLOOK (QTAKE+1)
#define QSNAP (QLOOK+1)

/*

  ECHNON = No hardware echo.
  ECHDSP = Hardware echo on the display.
  ECHHST = Hardware echo on the host (VDU?).
  ECHALL = Hardware echo on both.
*/

#define ECHNON 0
#define ECHDSP (ECHNON+1)
#define ECHHST (ECHDSP+1)
#define ECHALL (ECHHST+1)

/*

  Operations allowed when modifying queue data

  OSETL = Set pointer lock
  OSETG = Set pointer gearing
  OSETS = Set pointer sensitivity
*/

#define OSETL 0
#define OSETG (OSETL+1)
#define OSETS (OSETG+1)

/*  Copyright (C) 1988,1990:  Synoptics Ltd,  All Rights Reserved  */
