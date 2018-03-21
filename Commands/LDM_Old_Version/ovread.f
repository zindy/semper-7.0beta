C Semper 6 processing module OVREAD
C
      SUBROUTINE OVREAD
C
C Reads overlay from a display partition, the frame or (default) a
C displayed picture and outputs the result as a byte picture with zeros
C where the overlay is off and ones where the overlay is on.
C Two dimensional rectangular subregions are supported.
C
C Syntax:  Ovread :OVREAD $1= frame partition picture $2=sel to=$2 +
C                         size= si2= position= po2= +
C                         left right top bottom
C
      LOGICAL SEMOPN,SEMROW,FSINIT,FSOVRW,FSOPTN,FSREGN,FSAMPL,FSELEC
      INTEGER IVALPN
C
      INTEGER N,MODE,NCOL,NROW,NPIC,I,J
      REAL X,DX,Y,DY,XMIN,XMAX,YMIN,YMAX
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT)
      EQUIVALENCE (IB1(1),RB1(1))
C
C Packed name
C
      INTEGER NTO
      PARAMETER (NTO=-601)
C
C Establish graphics mode
C
      IF (FSOPTN(MODE,N)) GOTO 30
C
C Initialise graphics
C
      IF (FSINIT(MODE,N)) GOTO 30
C
C Establish subregion limits in graphics coordinates
C
      IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 30
C
C Establish sampling grid
C
      IF (FSAMPL(XMIN,XMAX,YMIN,YMAX,X,DX,NCOL,Y,DY,NROW)) GOTO 30
C
C Open destination picture
C
      NPIC=IVALPN(NTO)
      LP2=0
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,NFMBYT,LP2)) GOTO 30
C
C Initialise graphics again (note that SEMOPN calls FSINIT)
C
      IF (FSINIT(MODE,N)) GOTO 30
C
C For each row in destination picture
C
      DO 20 J=1,NROW
C
C Read overlay row to IB1
C
         IF (FSOVRW(1,IB1,NCOL,X,Y)) GOTO 30
C
C Convert to ones and zeros
C
         DO 10 I=1,NCOL
            IF (IB1(I).NE.0) IB1(I)=1
   10    CONTINUE
C
C Write row to destination picture
C
         IF (SEMROW(2,IB1,NFMINT,J,1,LP2)) GOTO 30
C
C Increment row position
C
         Y=Y+DY
C
   20 CONTINUE
C
C Update current frame/partition/picture number
C
      IF (FSELEC()) GOTO 30
C
   30 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
