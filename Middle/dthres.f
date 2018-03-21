C Semper 6 subsidiary module DTHR1
C
      SUBROUTINE DTHR1(LBG,DX,DY,PIXEL,NCOL,R,IMIN,IMAX)
C
      LOGICAL LBG
      INTEGER DX(*),DY(*),PIXEL(*),NCOL,IMIN,IMAX
      REAL    R
C
C Evaluate final Euclidean distances from offsets to nearest
C background pixel and threshold result
C
      INTEGER   I,P
      INTEGER*4 IDX,IDY,IDDMAX
C
      INTEGER*4 MAXVAL
      PARAMETER ( MAXVAL = 32766 )
C
C See if threshold value falls within long integer range
C
      IF (R.LT.SQRT(REAL(MAXVAL*MAXVAL+MAXVAL*MAXVAL))) THEN
C
C If so, set up equivalent long integer threshold value
C
         IDDMAX=R*R
C
C Threshold Euclidean distance values, avoiding square root calculation
C
         DO 10 I=1,NCOL
            IDX=DX(I)
            IDY=DY(I)
            IF (IDX*IDX+IDY*IDY.GT.IDDMAX.EQV.LBG) THEN
               PIXEL(I)=0
               IMIN=0
            ELSE
               PIXEL(I)=1
               IMAX=1
            ENDIF
   10    CONTINUE
C
C Otherwise, all pixels must threshold the same way (0 or 1 depending
C on whether eroding or dilating)
C
      ELSE
         IF (LBG) THEN
            P=1
            IMAX=1
         ELSE
            P=0
            IMIN=0
         ENDIF
C
         DO 20 I=1,NCOL
            PIXEL(I)=P
   20    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DTHR2
C
      SUBROUTINE DTHR2(LBG,IDIST,PIXEL,NCOL,R,IMIN,IMAX)
C
      LOGICAL LBG
      INTEGER IDIST(*),PIXEL(*),NCOL,IMIN,IMAX
      REAL    R
C
C Threshold integer distance values
C
      INTEGER I,IDMAX,P
C
      INTEGER MAXVAL
      PARAMETER ( MAXVAL = 32766 )
C
C See if threshold value falls within integer range
C
      IF (R.LT.REAL(MAXVAL)) THEN
C
C If so, set up equivalent integer threshold value
C
         IDMAX=R
C
C Threshold distance values
C
         DO 10 I=1,NCOL
            IF (IDIST(I).GT.IDMAX.EQV.LBG) THEN
               PIXEL(I)=0
               IMIN=0
            ELSE
               PIXEL(I)=1
               IMAX=1
            ENDIF
   10    CONTINUE
C
C Otherwise, all pixels must threshold the same way (0 or 1 depending
C on whether eroding or dilating)
C
      ELSE
         IF (LBG) THEN
            P=1
            IMAX=1
         ELSE
            P=0
            IMIN=0
         ENDIF
C
         DO 20 I=1,NCOL
            PIXEL(I)=P
   20    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
