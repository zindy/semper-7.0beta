C Semper X11 subroutine FSIN61 - session display initialisation
C
      LOGICAL FUNCTION FSIN61 ( IERROR )
      INTEGER IERROR
C
      INCLUDE 'COMMON'
C
      INTEGER KSTYLE,KXTYPE
      COMMON /XSTYLE/ KSTYLE,KXTYPE
C
      KXTYPE = 2
      FSIN61 = .FALSE.
      IDUMMY = IERROR
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper X11 subroutine FSLN61 - draw line in overlay plane
C
      LOGICAL FUNCTION FSLN61(IXEND,IYEND,IXST,IYST,IFR,IERROR)
      INTEGER IXEND,IYEND,IXST,IYST,IFR,IERROR
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMDLN,SEMDIA
C
      IF (SEMDLN(IXEND,IYEND,IXST,IYST,1)) THEN
         FSLN61 = .TRUE.
         IERROR = 40
         IF (SEMDIA('FSLN61: Error during line draw',NDIERR)) GOTO 10
         GOTO 10
      ELSE
         FSLN61 = .FALSE.
      ENDIF
C
      IDUMMY = IERROR
      IDUMMY = IFR
   10 RETURN
C
C Copyright (C) 1989 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper X11 subroutine FSRB61 - draw rubber banded line
C
      LOGICAL FUNCTION FSRB61(IOP,IFR,IXEND,IYEND,IXST,IYST,IERROR)
      INTEGER IOP,IFR,IXEND,IYEND,IXST,IYST,IERROR
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMDLN,SEMULN,SEMDIA,FSFL61
C
      IF (IOP .EQ. 1) THEN
         IF (SEMDLN(IXEND,IYEND,IXST,IYST,2)) GOTO 20
      ELSE
         IF (SEMULN(IXEND,IYEND,IXST,IYST,2)) GOTO 20
      ENDIF
      FSRB61 = FSFL61(1,IERROR)
C
   10 RETURN
C
   20 FSRB61 = .TRUE.
      IERROR = 40
      IF (SEMDIA('FSRB61: Error during line drawing',NDIERR)) GOTO 10
      IDUMMY = IFR
      GOTO 10
C
C Copyright (C) 1990,1991 Synoptics Ltd,  All Rights Reserved
C
      END
C
C SEMDLN - draw line
C
      LOGICAL FUNCTION SEMDLN(IX1,IY1,IX2,IY2,VALUE)
C
      INTEGER IX1,IY1,IX2,IY2,VALUE
C
      INCLUDE 'COMMON'
C
C Draws a line from (IX1,IY1) to (IX2,IY2) with value given by value.
C
      LOGICAL CLIP,X11DLN
C
      INTEGER CLIX2,CLIY2,CLIX1,CLIY1
C
      CLIX2 = IX2
      CLIY2 = IY2
      CLIX1 = IX1
      CLIY1 = IY1
C
C Clip line start and end points and draw if visible
C
      IF (CLIP(CLIX2,CLIY2,CLIX1,CLIY1)) THEN
         SEMDLN = X11DLN(CLIX1,CLIY1,CLIX2,CLIY2,VALUE)
      ELSE
         SEMDLN = .FALSE.
      ENDIF
      RETURN
C
C Copyright (C) 1989,1991  Synoptics Ltd,  All Rights Reserved
C
      END
C
C SEMULN - undraw line
C
      LOGICAL FUNCTION SEMULN(IX1,IY1,IX2,IY2,VALUE)
C
      INTEGER IX1,IY1,IX2,IY2,VALUE
C
      INCLUDE 'COMMON'
C
C UnDraws a line from (IX1,IY1) to (IX2,IY2) with value given by value.
C
      LOGICAL CLIP,X11ULN
C
      INTEGER CLIX2,CLIY2,CLIX1,CLIY1
C
      CLIX2 = IX2
      CLIY2 = IY2
      CLIX1 = IX1
      CLIY1 = IY1
C
C Clip line start and end points and draw if visible
C
      IF (CLIP(CLIX2,CLIY2,CLIX1,CLIY1)) THEN
         SEMULN = X11ULN(CLIX1,CLIY1,CLIX2,CLIY2,VALUE)
      ELSE
         SEMULN = .FALSE.
      ENDIF
      RETURN
C
C Copyright (C) 1989,1991  Synoptics Ltd,  All Rights Reserved
C
      END
C
C ICLIP - return clip region
C
      INTEGER FUNCTION ICLIP(X0, Y0)
C
C Calculate number of point as per Cohen-Sutherland
C
C    0 + 4 + 2 + 0  !  0 + 4 + 0 + 0  !  0 + 4 + 0 + 1
C    -------------------------------------------------
C    0 + 0 + 2 + 0  !  0 + 0 + 0 + 0  !  0 + 0 + 0 + 1
C    -------------------------------------------------
C    8 + 0 + 2 + 0  !  8 + 0 + 0 + 0  !  8 + 0 + 0 + 1
C
      INTEGER X0, Y0, IRES
      INCLUDE 'COMMON'
C
      IF (Y0 .LT. 0) THEN
          IRES = 8
      ELSE IF (Y0 .GE. FRSI2(1)) THEN
          IRES = 4
      ELSE
          IRES = 0
      ENDIF
C
      IF (X0 .LT. 0) THEN
          IRES = IRES + 2
      ELSE IF (X0 .GE. FRSIZ(1)) THEN
          IRES = IRES + 1
      ENDIF
C
      ICLIP = IRES
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C CLIP - clip region (for line draw etc.)
C
      LOGICAL FUNCTION CLIP(X0, Y0, X1, Y1)
C
C Clip the line specified in X0,Y0 and X1,Y1 to the current
C window limits
C
      INTEGER X0, Y0, X1, Y1, T, IR0, IR1, IBDR
C
      INCLUDE 'COMMON'
C
      INTEGER ICLIP
      REAL K
C
   10 CONTINUE
C
      IF (ICLIP(X0, Y0) .EQ. 0) THEN
         IF (ICLIP(X1, Y1) .EQ. 0) THEN
C
C Both points are now on viewed screen
C
            CLIP = .TRUE.
            RETURN
         ELSE
C
C Swap start and end points
C
            T = X0
            X0 = X1
            X1 = T
            T = Y0
            Y0 = Y1
            Y1 = T
         ENDIF
      ENDIF
C
      IR0 = ICLIP(X0, Y0)
      IR1 = ICLIP(X1, Y1)
      IF (IAND(IR0, IR1) .NE. 0) THEN
C
C Line is off screen
C
         CLIP = .FALSE.
         RETURN
      ENDIF
      IF (IAND(IR0, 1) .NE. 0) THEN
         IBDR = FRSI2(1) - 1
      ELSE
         IF (IAND(IR0, 2) .EQ. 0) GOTO 20
         IBDR = 0
      ENDIF
C
      K = REAL(Y1 - Y0) / REAL(X1 - X0)
      Y0 = INT(K * REAL(IBDR - X0) + Y0)
      X0 = IBDR
      GOTO 10
C
   20 IF (IAND(IR0, 4) .NE. 0) THEN
         IBDR = FRSIZ(1) - 1
      ELSE
         IF (IAND(IR0, 8) .EQ. 0) GOTO 10
         IBDR = 0
      ENDIF
C
      K = REAL(X1 - X0) / REAL(Y1 - Y0)
      X0 = INT(K * REAL(IBDR - Y0) + X0)
      Y0 = IBDR
      GOTO 10
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper X11 subroutine FSRI61 - input row of pixels
C
      LOGICAL FUNCTION FSRI61(ROW,N,IFORM,IXST,IYST,IFR,
     +                        BLACK,WHITE,ICOL,IERROR)
C
C   recover n pixels of row from frame ifr, inverse scaling into the
C   range black to white; other details as for fsro61
C
      INTEGER N,IFORM,IXST,IYST,IFR,ICOL,IERROR
      REAL ROW(*),BLACK,WHITE
C
      LOGICAL FSBNRI,FSBSRI,FSINRI,FSISRI,FSFNRI,FSFSRI,FSCNRI,FSCSRI
      LOGICAL SEMDIA
C
      INCLUDE 'COMMON'
C
      REAL SCALE
C
      LOGICAL NOSCAL
C
C This code checks that the pixels requested for read are within the
C framestore boundary. It produces an error message if they are not.
C
      IF ((IXST .LT. 0) .OR. ((IXST+N) .GT. FRSIZ(1))
     +   .OR. (IYST .GT. (FRSI2(1)-1)) .OR. (IYST .LT. 0)) THEN
C
         IF (SEMDIA('Trying to read pixels outside frame boundary',
     +             NDIERR)) GOTO 10
         GOTO 20
      ENDIF
C
C Prepare scaling
C
      SCALE = (WHITE-BLACK)/REAL(LUTLEN-1)
      NOSCAL = (((SCALE .GE. 0.999999) .AND. (SCALE .LE. 1.000001))
     +    .AND. ((BLACK .GE. -0.000001).AND. (BLACK .LE. +0.000001)))
C
C Fetch data from display
C
      IF (IFORM.EQ.NFMBYT) THEN
         IF (NOSCAL) THEN
            IF (FSBNRI(ROW,N,IXST,IYST,IFR)) GOTO 20
         ELSE
            IF (FSBSRI(ROW,N,IXST,IYST,IFR,BLACK,WHITE)) GOTO 20
         ENDIF
      ELSE IF (IFORM.EQ.NFMINT) THEN
         IF (NOSCAL) THEN
            IF (FSINRI(ROW,N,IXST,IYST,IFR)) GOTO 20
         ELSE
            IF (FSISRI(ROW,N,IXST,IYST,IFR,BLACK,WHITE)) GOTO 20
         ENDIF
      ELSE IF (IFORM.EQ.NFMFP) THEN
         IF (NOSCAL) THEN
            IF (FSFNRI(ROW,N,IXST,IYST,IFR)) GOTO 20
         ELSE
            IF (FSFSRI(ROW,N,IXST,IYST,IFR,BLACK,WHITE)) GOTO 20
         ENDIF
      ELSE
         IF (NOSCAL) THEN
            IF (FSCNRI(ROW,N,IXST,IYST,IFR)) GOTO 20
         ELSE
            IF (FSCSRI(ROW,N,IXST,IYST,IFR,BLACK,WHITE)) GOTO 20
         ENDIF
      ENDIF
C
      FSRI61 = .FALSE.
C
   10 RETURN
C
   20 IERROR = 40
      FSRI61 = .TRUE.
      IDUMMY = ICOL
      GOTO 10
C
C Copyright (C):  Synoptics Limited 1988,1989
C
      END
C
C Semper X11 subroutine FSRO61 - output row of pixels
C
      LOGICAL FUNCTION FSRO61(ROW,N1,N,ISAM,IFORM,IXST,IYST,IFR,
     +                        BLACK,WHITE,ICOL,IERROR)
C
C   output pixels n1 to n sampling isam of row to frame ifr, rightwards
C   from ixst,iyst, scaled so that black appears black and white white;
C   if icol non-zero, output column downwards instead;
C   (icol not actioned at present)
C   row type acc to iform
C
      INTEGER N1,N,ISAM,IFORM,IXST,IYST,IFR,ICOL,IERROR
      INTEGER ROW(*)
      REAL BLACK,WHITE
C
      INCLUDE 'COMMON'
C
      LOGICAL FSBNRO,FSBSRO,FSINRO,FSISRO,FSFNRO,FSFSRO,SEMDIA
C
      INTEGER STEP,FIRST,LAST
      INTEGER CIXST,CIXFIN,CFIRST,CLAST,IXFIN,IXSIZE
C
      REAL BMNUS,BPLUS,WMNUS,WPLUS,X
      PARAMETER(BMNUS=-0.0002,BPLUS=+0.0002)
      LOGICAL NOSCAL
C
C Decode iform and decode what to do
C
      STEP = ISAM
      FIRST = N1
      LAST = N
C
      IF (IFORM .EQ. NFMCOM) THEN
         STEP = ISAM+ISAM
         FIRST = N1+N1-1
         LAST = N+N-1
      ENDIF
C
      IXFIN = IXST+(LAST-FIRST)/STEP
C
      CIXST = IXST
      CIXFIN = IXFIN
      CFIRST = FIRST
      CLAST = LAST
C
      IXSIZE = FRSIZ(1)-1
      IF ((IYST .LT. 0) .OR. (IYST .GT. (FRSI2(1)-1))
     +   .OR. (CIXFIN .LT. 0) .OR. (CIXST .GT. IXSIZE)) GOTO 10
C
      IF (CIXST .LT. 0) THEN
         CIXST = 0
         CFIRST = LAST - (CIXFIN*STEP)
      ENDIF
C
      IF (CIXFIN .GT. IXSIZE) THEN
         CIXFIN = IXSIZE
         CLAST = ((IXSIZE-CIXST)*STEP) + CFIRST
      ENDIF
C
C Set NOSCAL according to whether scaling is required
C
      X = REAL(LUTLEN-1)
      WMNUS = X - .0002
      WPLUS = X + .0002
      NOSCAL = (((BLACK .GT. BMNUS) .AND. (BLACK .LT. BPLUS))
     +    .AND. ((WHITE .GT. WMNUS) .AND. (WHITE .LT. WPLUS)))
C
C Output the row via the appropriate routine
C
      IF (IFORM .EQ. NFMBYT) THEN
         IF (NOSCAL) THEN
            IF (FSBNRO(ROW,CFIRST,CLAST,STEP,CIXST,IYST,IFR)) GOTO 30
         ELSE
            IF (FSBSRO(ROW,CFIRST,CLAST,STEP,CIXST,IYST,IFR,
     +                 BLACK,WHITE)) GOTO 30
         ENDIF
      ELSE IF (IFORM .EQ. NFMINT) THEN
         IF (NOSCAL) THEN
            IF (FSINRO(ROW,CFIRST,CLAST,STEP,CIXST,IYST,IFR)) GOTO 30
         ELSE
            IF (FSISRO(ROW,CFIRST,CLAST,STEP,CIXST,IYST,IFR,
     +                 BLACK,WHITE)) GOTO 30
         ENDIF
      ELSE IF (IFORM .EQ. NFMFP .OR. IFORM .EQ. NFMCOM) THEN
         IF (NOSCAL) THEN
            IF (FSFNRO(ROW,CFIRST,CLAST,STEP,CIXST,IYST,IFR)) GOTO 30
         ELSE
            IF (FSFSRO(ROW,CFIRST,CLAST,STEP,CIXST,IYST,IFR,
     +                 BLACK,WHITE)) GOTO 30
         ENDIF
      ENDIF
C
   10 FSRO61 = .FALSE.
   20 RETURN
C
C Error
C
   30 IERROR = 40
      IF (SEMDIA('FSRO61: error during row output',NDIERR)) GOTO 40
C
   40 FSRO61 = .TRUE.
      IDUMMY = ICOL
      GOTO 20
C
C Copyright (C) 1989 Synoptics Limited
C
      END
C
C Semper X11 subroutine FSLN61 - draw text characters
C
      LOGICAL FUNCTION FSTX61(STRING,CHARS,X,Y,IFR,IERROR)
      INTEGER STRING(*),CHARS,X,Y,IFR,IERROR
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMDIA,X11DTX
      INTEGER IX,IY
C
      FSTX61 = .FALSE.
      IF (CHARS .GT. 0) THEN
C
C Centre X
C
         IX = X - (CHSIZ(1) * CHARS)/2
C
C Centre Y
C
         IY = Y - (CHSI2(1)/2)
         IF (X11DTX(STRING,CHARS,IX,IY,.TRUE.,.TRUE.)) THEN
            FSTX61 = .TRUE.
            IERROR = 40
            IF (SEMDIA('Error during text output',NDIERR)) GOTO 10
         ENDIF
      ENDIF
C
   10 RETURN
      IDUMMY=IFR
C
C Copyright (C) 1989 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper X11 subroutine FSX11P
C
      SUBROUTINE FSX11P(POSX,POSY)
      INTEGER POSX,POSY
C
C Overrides values of POSX and POSY with the values given in the Semper
C variables POS and PO2 if they are set.
C
      INTEGER NPOS,NPO2
      PARAMETER (NPOS=26219,NPO2=26232)
C
      INTEGER IVAL
      LOGICAL VARSET
C
      IF (VARSET(NPOS)) THEN
         POSX = IVAL(NPOS)
         IF (VARSET(NPO2)) POSY = IVAL(NPO2)
      ENDIF
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper X11 subroutine FSX11M
C
      SUBROUTINE FSX11M(SIZX,SIZY)
      INTEGER SIZX,SIZY
C
C Stores the values SIZX and SIZY as the monitor size of
C        the display device
C
      INCLUDE 'COMMON'
C
      MONSIZ(1) = SIZX
      MONSI2(1) = SIZY
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper X11 processing routine X11SET
C
      SUBROUTINE X11SET
C
      INTEGER NILIM,NVLIM,NVERIF
      PARAMETER (NILIM=14889,NVLIM=-3690,NVERIF=-3419)
C
      INTEGER IVAL
      LOGICAL OPT,SEMCON,VARSET
C
      INCLUDE 'COMMON'
C
      INTEGER I,V
      LOGICAL LI,LV
C
      LI = VARSET(NILIM)
      LV = VARSET(NVLIM)
C
      IF (LI .OR. LV) THEN
         CALL SX11GC(I,V)
         IF (LI) I = IVAL(NILIM)
         IF (LV) V = IVAL(NVLIM)
         CALL SX11SC(I,V)
         IF (OPT(NVERIF)) THEN
            RECORD = 'Image refresh'
            IF (I .LE. 0) THEN
               RECORD(15:) = 'on flush'
            ELSE
               WRITE(RECORD(15:),10) I
   10          FORMAT('every',i5,' operations')
            ENDIF
            IF (SEMCON(RECORD(1:40))) GOTO 20
            RECORD = 'Graphic refresh'
            IF (V .LE. 0) THEN
               RECORD(17:) = 'on flush'
            ELSE
               WRITE(RECORD(17:),10) V
            ENDIF
            IF (SEMCON(RECORD(1:40))) GOTO 20
         ENDIF
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1989,1991:  Synoptics Ltd,  All Rights Reserved
C
      END
