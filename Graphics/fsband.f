C Semper 6 system module FSBAND
C
      LOGICAL FUNCTION FSBAND(IOP,X,Y,N,CLOSED)
C
      INTEGER IOP,N
      REAL    X(*),Y(*)
      LOGICAL CLOSED
C
C Draws/undraws open/closed curve of N lines whose end points are
C passed in arrays X and Y in graphics coordinates.  If CLOSED is
C .TRUE., a line is output between the first and last point to close
C curve.  The curve is drawn if IOP = 1 and undrawn if IOP = 2.
C FSBAND provides basic support for rubberbanding.  The transformation
C from graphics to display coordinates must have been previously set up
C in COMMON via a call to FSINIT.  FSINIT will also have determined, in
C the case of picture coordinates being selected, whether output to
C the real or imaginary part of a complex display picture has been
C requested by means of one of the options RE or IM.
C
      LOGICAL FSQMON,FSEDGE,FSFLUS
C
      INCLUDE 'COMMON'
C
      INTEGER FRAME,ZOOM,I
      REAL XCLIP(2),YCLIP(2)
C
      FSBAND=.TRUE.
C
C Fetch current frame and monitor limits
C
      IF (FSQMON(FRAME,ZOOM,XCLIP(1),XCLIP(2),YCLIP(1),YCLIP(2)))
     +   GOTO 20
C
C Draw/undraw first (N-2) lines (all end points connect with next line)
C
      DO 10 I=1,N-2
         IF (FSEDGE(IOP,FRAME,X(I),Y(I),X(I+1),Y(I+1),XCLIP,YCLIP,
     +              .TRUE.)) GOTO 20
   10 CONTINUE
C
C Draw next line (end point joins on to closing line (if any))
C
      IF (N.GT.1) THEN
         IF (FSEDGE(IOP,FRAME,X(N-1),Y(N-1),X(N),Y(N),XCLIP,YCLIP,
     +              CLOSED)) GOTO 20
      ENDIF
C
C Draw closing line (if any) (end point always joins to first line)
C
      IF (CLOSED.AND.N.GT.0) THEN
         IF (FSEDGE(IOP,FRAME,X(N),Y(N),X(1),Y(1),XCLIP,YCLIP,
     +              .TRUE.)) GOTO 20
      ENDIF
C
C Flush out contents of graphics buffer (if any)
C
      IF (FSFLUS()) GOTO 20
C
      FSBAND=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
