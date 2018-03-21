void sx11cu ( xp, yp )
F_integer *xp, *yp;

{
	sx11cu_ ( xp, yp ) ;
}
void sx11cr ( xp, yp )
F_integer *xp, *yp;

{
	sx11cr_ ( xp, yp ) ;
}

void sx11mc ( fgc, bgc )
F_integer *fgc, *bgc;

{
	sx11mc_ ( fgc, bgc ) ;
}

F_logical mentou ( astring, string_length )

char *astring;
medium string_length;

{
	return mentou_ ( astring, string_length );
}

F_logical semtou ( astring, string_length )                                 

char *astring;
medium string_length;

{
	return semtou_ ( astring, string_length ) ;                                
}


F_logical sx11vo ( astring, string_length )

char *astring;
medium string_length;

{
	return sx11vo_ ( astring, string_length );
}

F_logical semtbs ( steps )

F_integer *steps;

{
	return semtbs_ ( steps ) ;
}

F_logical semtcr ()

{
	return semtcr_ () ;
}

F_logical semtcu ()

{
	return semtcu_ () ;
}

F_logical semtfl ()

{
	return semtfl_ () ;
}

F_logical semtlf ()

{
	return semtlf_ () ;
}

F_logical semtnv ()

{
	return semtnv_ () ;
}

F_logical semtrv ()

{
	return semtrv_ () ;
}

void sx11ct ( above, below )
F_logical *above, *below;

{
	sx11ct_ ( above, below );
}

void buzzer ()
{
	buzzer_ ();
}


void sx11gc ( ilim, vlim )
F_integer *ilim, *vlim;
{
	sx11gc_ ( ilim, vlim ) ;
}

void sx11sc ( ilim, vlim )
F_integer *ilim, *vlim;
{
	sx11sc_ ( ilim, vlim ) ;
}

void sx11dx ()
{
	sx11dx_ () ;
}

void sx11ex ()
{
	sx11ex_ () ;
}

void sx11ce ()
{
	sx11ce_ () ;
}

F_logical sx11in ( batch )

F_logical *batch;
{
	return sx11in_ ( batch ) ;
}

F_logical fslq61 ( lutlen, lutmax )

F_integer *lutlen, *lutmax;

{
	return fslq61_ ( lutlen, lutmax ) ;
}

F_logical fsas61 ( nfrs, nfx, nfy, nmx, nmy, nchx, nchy, ngp, iwo, ierror )

F_integer *nfrs, *nfx, *nfy, *nmx, *nmy, *nchx, *nchy, *ngp, *iwo, *ierror;

{
	return fsas61_ ( nfrs, nfx, nfy, nmx, nmy, nchx, nchy, ngp, iwo, ierror ) ;
}

F_logical fscc61 ( iop, frame, ix, iy, ierror )

F_integer *iop, *frame, *ix, *iy, *ierror;
{
	return fscc61_ ( iop, frame, ix, iy, ierror ) ;
}

F_logical fsct61 ( itype, ierror )

F_integer *itype, *ierror;

{
	return fsct61_ ( itype, ierror ) ;
}

F_logical fsde61 ( ierror )
F_integer *ierror;
{
	return fsde61_ ( ierror ) ;
}

F_logical fser61 ( iop, nx, ny, ix, iy, ifr, ierror )
F_integer *iop, *nx, *ny, *ix, *iy, *ifr, *ierror;
{
	return fser61_ ( iop, nx, ny, ix, iy, ifr, ierror ) ;
}

F_logical fsex61 ( ierror )

F_integer *ierror;
{
	return fsex61_ ( ierror ) ;
}

F_logical fsfl61 ( iop, ierror )

F_integer *iop, *ierror;
{
	return fsfl61_ ( iop, ierror ) ;
}

F_logical x11dln ( ix2, iy2, ix1, iy1, iop )

F_integer *ix2, *iy2, *ix1, *iy1, *iop;
{
	return x11dln_ ( ix2, iy2, ix1, iy1, iop ) ;
}

F_logical x11uln ( ix2, iy2, ix1, iy1, iop )

F_integer *ix2, *iy2, *ix1, *iy1, *iop;

{
	return x11uln_ ( ix2, iy2, ix1, iy1, iop ) ;
}

F_logical fslu61 ( iop, lutn, mode, lutbuf, ierror )
F_integer *iop, *lutn, *mode, *lutbuf, *ierror;
{
	return fslu61_ ( iop, lutn, mode, lutbuf, ierror ) ;
}

F_logical fsoi61 ( buff, n, ix, iy, frame, icol, ierror )                                                                   

F_integer *buff, *n, *ix, *iy, *frame, *icol, *ierror;

{
	return fsoi61_ ( buff, n, ix, iy, frame, icol, ierror ) ;
}

F_logical fsoo61 ( buff, n, ix, iy, frame, icol, ierror )                                                                   

F_integer *buff, *n, *ix, *iy, *frame, *icol, *ierror;

{
	return fsoo61_ ( buff, n, ix, iy, frame, icol, ierror ) ;
}

F_logical fsov61 ( IOP, RED, GREEN, BLUE, VISIB, GOVER, ROVER, COVER, IERROR )
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
	return fsov61_ ( IOP, RED, GREEN, BLUE, VISIB, GOVER, ROVER, COVER, IERROR ) ;
}

F_logical x11dtx ( tbuf, n, ix, iy, back, normal )

F_integer *tbuf, *n, *ix, *iy;
F_logical *back, *normal;

{
	return x11dtx_ ( tbuf, n, ix, iy, back, normal );
}

F_logical fsvq61 ( frame, zoom, xmin, xmax, ymin, ymax, ierror )

F_integer *frame, *zoom, *xmin, *xmax, *ymin, *ymax, *ierror;

{
	return fsvq61_ ( frame, zoom, xmin, xmax, ymin, ymax, ierror ) ;
}

F_logical fsvw61 ( lutn, izoom, iblank, nx, ny, ix, iy, mx, my, ifr, ierror )

F_integer *lutn, *izoom, *iblank, *nx, *ny, *ix, *iy, *mx, *my, *ifr, *ierror;

{
	return fsvw61_ ( lutn, izoom, iblank, nx, ny, ix, iy, mx, my, ifr, ierror ) ;
}

void eqxxpo ()

{
	eqxxpo_ ();
}

void eqxxpc ()

{
	eqxxpc_ ();
}
	
void eqxxko ()

{
	eqxxko_ ();
}
	
void eqxxkc ()

{
	eqxxkc_ ();
}

void eqxxpk ()

{
	eqxxpk_ ();
}

void tersiz ( xsize, ysize )

F_integer *xsize, *ysize;

{
	tersiz_ ( xsize, ysize ) ;
}

void dhocqu ( x, y )

F_integer *x, *y;

{
	dhocqu_ ( x, y ) ;
}

void dhocmo ( x, y )

F_integer *x, *y;

{
	dhocmo_ ( x, y );
}

void dhocsa ()

{
    dhocsa_ () ;
}

void dhocre ()

{
	dhocre_ ();
}

void dhocon ()

{
	dhocon_ ();
}

void dhocof ()

{
	dhocof_ ();
}

F_logical dhodsi ( xmin,ymin,xsize,ysize,mxsca,mysca,xoff,yoff,ncols )

F_integer *xmin,*ymin,*xsize,*ysize,*mxsca,*mysca,*xoff,*yoff,*ncols;

{
	return dhodsi_ ( xmin,ymin,xsize,ysize,mxsca,mysca,xoff,yoff,ncols ) ;
}

F_logical dhodst ( dummy )

F_integer *dummy;

{
	return dhodst_ ( dummy ) ;
}

F_logical dhodsh ( dummy )

F_integer *dummy;

{
	return dhodsh_ ( dummy );
}

F_logical dhoscr ( xpos, ypos, xsize, ysize )
F_integer *xpos, *ypos, *xsize, *ysize;

{
	return dhoscr_ ( xpos, ypos, xsize, ysize ) ;
}

F_logical fscnri (frow, n, ix, iy, ifr)                                                                                 
F_real *frow;
F_integer *n, *ix, *iy, *ifr;
{
        return fscnri_ (frow, n, ix, iy, ifr) ;
}

F_logical fsbnro (frow, first, last, step, ix, iy, ifr)
unsigned char *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
{
        return fsbnro_ (frow, first, last, step, ix, iy, ifr) ;
}

F_logical fsinro (frow, first, last, step, ix, iy, ifr)                                                                 
F_integer *frow;                                                                                                        
F_integer *first, *last, *step, *ix, *iy, *ifr;
{
        return fsinro_ (frow, first, last, step, ix, iy, ifr) ;
}

F_logical fsfnro (frow, first, last, step, ix, iy, ifr)                                                                 
F_real *frow;                                                                                                           
F_integer *first, *last, *step, *ix, *iy, *ifr;
{
        return fsfnro_ (frow, first, last, step, ix, iy, ifr) ;
}

F_logical fsbsro (frow, first, last, step, ix, iy, ifr, bl, wh)                                                         
unsigned char *frow;
F_integer *first, *last, *step, *ix, *iy, *ifr;
F_real *bl, *wh;
{
        return fsbsro_ (frow, first, last, step, ix, iy, ifr, bl, wh) ;
}

F_logical fsisro (frow, first, last, step, ix, iy, ifr, bl, wh)                                                         
F_integer *frow;                                                                                                        
F_integer *first, *last, *step, *ix, *iy, *ifr;
F_real *bl, *wh;
{
        return fsisro_ (frow, first, last, step, ix, iy, ifr, bl, wh) ;
}

F_logical fsfsro (frow, first, last, step, ix, iy, ifr, bl, wh)                                                         
F_real *frow;                                                                                                           
F_integer *first, *last, *step, *ix, *iy, *ifr;
F_real *bl, *wh;
{
        return fsfsro_ (frow, first, last, step, ix, iy, ifr, bl, wh) ;
}

F_logical fsbnri (frow, n, ix, iy, ifr)                                                                                 
unsigned char *frow;
F_integer *n, *ix, *iy, *ifr;
{
        return fsbnri_ (frow, n, ix, iy, ifr) ;
}

F_logical fsinri (frow, n, ix, iy, ifr)                                                                                 
F_integer *frow;                                                                                                        
F_integer *n, *ix, *iy, *ifr;
{
        return fsinri_ (frow, n, ix, iy, ifr) ;
}

F_logical fsfnri (frow, n, ix, iy, ifr)                                                                                 
F_real *frow;                                                                                                           
F_integer *n, *ix, *iy, *ifr;
{
        return fsfnri_ (frow, n, ix, iy, ifr) ;
} 

F_logical fsbsri (frow, n, ix, iy, ifr, black, white)                                                                   
unsigned char *frow;
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
        return fsbsri_ (frow, n, ix, iy, ifr, black, white) ;
}

F_logical fsisri (frow, n, ix, iy, ifr, black, white)                                                                   
F_integer *frow;                                                                                                        
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
        return fsisri_ (frow, n, ix, iy, ifr, black, white) ;
}

F_logical fsfsri (frow, n, ix, iy, ifr, black, 
white)                                                                   
F_real *frow;                                                                                                           
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
        return fsfsri_ (frow, n, ix, iy, ifr, black, white) ;
}

F_logical fscsri (frow, n, ix, iy, ifr, black, white)                                                                   
F_real *frow;                                                                                                           
F_integer *n, *ix, *iy, *ifr;
F_real *black, *white;
{
        return fscsri_ (frow, n, ix, iy, ifr, black, white) ;
}


