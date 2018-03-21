C Semper 6 system module FSDRAG
C
      LOGICAL FUNCTION FSDRAG(IOP,X,Y,DX,DY,N,CLOSED)
C
      INTEGER IOP,N
      REAL    X,Y,DX(*),DY(*)
      LOGICAL CLOSED
C
C Draws/undraws open/closed curve of N lines whose end points are
C specified in arrays DX and DY as offsets in graphics coordinates
C relative to the anchor position (X,Y).  If CLOSED is .TRUE., a line
C is output between the first and last point to close curve.  The curve
C is drawn if IOP = 1 and undrawn if IOP = 2.  FSDRAG provides basic
C support for rubberbanding.  The transformation from graphics to
C display coordinates must have been previously set up in COMMON via a
C call to FSINIT.  FSINIT will also have determined, in the case of
C picture coordinates being selected, whether output to the real or
C imaginary part of a complex display picture has been requested by
C means of one of the options RE or IM.
C
      LOGICAL FSQMON,FSEDGE,FSFLUS
C
      INCLUDE 'COMMON'
C
      INTEGER FRAME,ZOOM,I
      REAL XCLIP(2),YCLIP(2)
C
      FSDRAG=.TRUE.
C
C Fetch current frame and monitor limits
C
      IF (FSQMON(FRAME,ZOOM,XCLIP(1),XCLIP(2),YCLIP(1),YCLIP(2)))
     +   GOTO 20
C
C Draw/undraw first (N-2) lines (all end points connect with next line)
C
      DO 10 I=1,N-2
         IF (FSEDGE(IOP,FRAME,X+DX(I),Y+DY(I),X+DX(I+1),Y+DY(I+1),
     +              XCLIP,YCLIP,.TRUE.)) GOTO 20
   10 CONTINUE
C
C Draw next line (end point joins on to closing line (if any))
C
      IF (N.GT.1) THEN
         IF (FSEDGE(IOP,FRAME,X+DX(N-1),Y+DY(N-1),X+DX(N),Y+DY(N),
     +              XCLIP,YCLIP,CLOSED)) GOTO 20
      ENDIF
C
C Draw closing line (if any) (end point always joins to first line)
C
      IF (CLOSED.AND.N.GT.0) THEN
         IF (FSEDGE(IOP,FRAME,X+DX(N),Y+DY(N),X+DX(1),Y+DY(1),
     +              XCLIP,YCLIP,.TRUE.)) GOTO 20
      ENDIF
C
C Flush out contents of graphics buffer (if any)
C
      IF (FSFLUS()) GOTO 20
C
      FSDRAG=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
