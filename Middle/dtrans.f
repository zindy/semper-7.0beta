C Semper 6 subsidiary module DTRAN1
C
      LOGICAL FUNCTION DTRAN1(IPASS,LBG,LCLOSE,DX,DY,ROW,NCOL,NROW,
     +                        LZERO,LNZERO)
C
      INTEGER IPASS,DX(0:*),DY(0:*),ROW,NCOL,NROW
      LOGICAL LBG,LCLOSE,LZERO,LNZERO
C
C Reads data from row ROW of picture LP1/LP3 (IPASS = 1/2) and sets edge
C distance offsets to EDGVAL.  EDGVAL is zero if LCLOSE is .TRUE. and
C MAXVAL (the default) otherwise.  If LBG is .TRUE., the distance
C transform is evaluated with respect to the foreground instead of the
C background, i.e. wherever source pixels are set to zero (background).
C If IPASS = 1, distance offsets are initialised to zero or MAXVAL and
C the range flags LZERO and LNZERO are set appropriately.
C
      LOGICAL SEMROW
C
      INTEGER I,EDGVAL,FGVAL,BGVAL
C
      INCLUDE 'COMMON'
C
      INTEGER MAXVAL
      PARAMETER ( MAXVAL = 32766 )
C
      DTRAN1=.TRUE.
C
C Determine EDGVAL according to flag LCLOSE
C
      IF (LCLOSE) THEN
         EDGVAL=0
      ELSE
         EDGVAL=MAXVAL
      ENDIF
C
C If edge row, set all pixel values to EDGVAL
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) THEN
         DO 10 I=0,NCOL+1
            DX(I)=EDGVAL
            DY(I)=EDGVAL
   10    CONTINUE
C
C Otherwise, read data from disc and initialise edge pixels
C
      ELSE
C
C If first pass, set up initialisation values ...
C
         IF (IPASS.EQ.1) THEN
            IF (LBG) THEN
               BGVAL=MAXVAL
               FGVAL=0
            ELSE
               BGVAL=0
               FGVAL=MAXVAL
            ENDIF
C
C ... and read source picture row ...
C
            IF (SEMROW(1,DX(1),NFMINT,ROW,1,LP1)) GOTO 30
C
C ... and initialise distance offsets
C
            DO 20 I=1,NCOL
               IF (DX(I).EQ.0) THEN
                  DX(I)=BGVAL
                  DY(I)=BGVAL
                  LZERO=.TRUE.
               ELSE
                  DX(I)=FGVAL
                  DY(I)=FGVAL
                  LNZERO=.TRUE.
               ENDIF
   20       CONTINUE
C
C Otherwise, read intermediate distance offsets from temporary picture
C
         ELSE
            IF (SEMROW(1,DX(1),NFMINT,ROW,1,LP3)) GOTO 30
            IF (SEMROW(1,DY(1),NFMINT,ROW,2,LP3)) GOTO 30
         ENDIF
C
C Set left and right edge distance offset to EDGVAL
C
         DX(0)=EDGVAL
         DY(0)=EDGVAL
C
         DX(NCOL+1)=EDGVAL
         DY(NCOL+1)=EDGVAL
      ENDIF
C
      DTRAN1=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DTRAN2
C
      SUBROUTINE DTRAN2(IPASS,DX1,DY1,DX2,DY2,NCOL)
C
      INTEGER IPASS,DX1(0:*),DY1(0:*),DX2(0:*),DY2(0:*),NCOL
C
C Carries out basic row processing for determining Euclidean distances
C given data for previous and current rows in arrays DX1,DY1 and DX2,DY2
C respectively.  Processing proceeds from top to bottom during the first
C pass (IPASS=1) and in the reverse direction on the second pass
C (IPASS=2).
C
      INTEGER   I
      INTEGER*4 DX,DY,DD,DN
C
C Calculate Euclicean offsets - forward pass ...
C
      IF (IPASS.EQ.1) THEN
         DO 10 I=1,NCOL
            DX=DX2(I)
            DY=DY2(I)
            DD=DX*DX+DY*DY
C
            IF (DD.GT.0) THEN
               DX=DX1(I)
               DY=DY1(I)+1
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
C
               DX=DX2(I-1)+1
               DY=DY2(I-1)
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
C
               DX=DX1(I-1)+1
               DY=DY1(I-1)+1
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
C
               DX=DX1(I+1)+1
               DY=DY1(I+1)+1
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
            ENDIF
   10    CONTINUE
C
         DO 20 I=NCOL,1,-1
            DX=DX2(I)
            DY=DY2(I)
            DD=DX*DX+DY*DY
C
            IF (DD.GT.0) THEN
               DX=DX2(I+1)+1
               DY=DY2(I+1)
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
            ENDIF
   20    CONTINUE
C
C ... or reverse pass
C
      ELSE
         DO 30 I=NCOL,1,-1
            DX=DX2(I)
            DY=DY2(I)
            DD=DX*DX+DY*DY
C
            IF (DD.GT.0) THEN
               DX=DX1(I)
               DY=DY1(I)+1
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
C
               DX=DX2(I+1)+1
               DY=DY2(I+1)
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
C
               DX=DX1(I-1)+1
               DY=DY1(I-1)+1
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
C
               DX=DX1(I+1)+1
               DY=DY1(I+1)+1
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
            ENDIF
   30    CONTINUE
C
         DO 40 I=1,NCOL
            DX=DX2(I)
            DY=DY2(I)
            DD=DX*DX+DY*DY
C
            IF (DD.GT.0) THEN
               DX=DX2(I-1)+1
               DY=DY2(I-1)
               DN=DX*DX+DY*DY
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
               ENDIF
            ENDIF
   40    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DTRAN3
C
      LOGICAL FUNCTION DTRAN3(IPASS,LBG,LCLOSE,IDIST,ROW,NCOL,NROW,
     +                        LZERO,LNZERO)
C
      INTEGER IPASS,IDIST(0:*),ROW,NCOL,NROW
      LOGICAL LBG,LCLOSE,LZERO,LNZERO
C
C Reads data from row ROW of picture LP1/LP3 (IPASS = 1/2) and sets
C edge pixels to MAXVAL.  EDGVAL is zero if LCLOSE is .TRUE. and
C MAXVAL (the default) otherwise.  If LBG is .TRUE., the distance
C transform is evaluated with respect to the foreground instead of the
C background, i.e. wherever source pixels are set to zero (background).
C If IPASS = 1, distance values are initialised to zero or MAXVAL and
C range flags LZERO and LNZERO are set appropriately.
C
      LOGICAL SEMROW
C
      INTEGER I,EDGVAL,FGVAL,BGVAL
C
      INCLUDE 'COMMON'
C
      INTEGER MAXVAL
      PARAMETER ( MAXVAL = 32766 )
C
      DTRAN3=.TRUE.
C
C Determine EDGVAL according to flag LCLOSE
C
      IF (LCLOSE) THEN
         EDGVAL=0
      ELSE
         EDGVAL=MAXVAL
      ENDIF
C
C If edge row, initialise all distance values to EDGVAL
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) THEN
         DO 10 I=0,NCOL+1
            IDIST(I)=EDGVAL
   10    CONTINUE
C
C Otherwise, read data from disc and initialise edge pixels
C
      ELSE
C
C If first pass, set up initialisation values ...
C
         IF (IPASS.EQ.1) THEN
            IF (LBG) THEN
               BGVAL=MAXVAL
               FGVAL=0
            ELSE
               BGVAL=0
               FGVAL=MAXVAL
            ENDIF
C
C ... and read source picture row ...
C
            IF (SEMROW(1,IDIST(1),NFMINT,ROW,1,LP1)) GOTO 30
C
C ... and initialise distance offsets
C
            DO 20 I=1,NCOL
               IF (IDIST(I).EQ.0) THEN
                  IDIST(I)=BGVAL
                  LZERO=.TRUE.
               ELSE
                  IDIST(I)=FGVAL
                  LNZERO=.TRUE.
               ENDIF
   20       CONTINUE
C
C Otherwise, read intermediate distance values
C
         ELSE
            IF (SEMROW(1,IDIST(1),NFMINT,ROW,1,LP3)) GOTO 30
         ENDIF
C
C Set left and right edge distance offset to EDGVAL
C
         IDIST(0)=EDGVAL
         IDIST(NCOL+1)=EDGVAL
      ENDIF
C
      DTRAN3=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DTRAN4
C
      SUBROUTINE DTRAN4(IPASS,IOP,IDIST1,IDIST2,NCOL,IMAX)
C
      INTEGER IPASS,IOP,IDIST1(0:*),IDIST2(0:*),NCOL,IMAX
C
C Carries out basic row processing for diamond, square or octagonal
C distances (IOP specifies which), given data for previous and current
C rows in arrays IDIST1 and IDIST2 respectively.  Processing proceeds
C from top to bottom during the first pass (IPASS=1) and in the reverse
C direction on the second pass (IPASS=2).  Updates the range variable
C IMAX on the second pass if any distance is larger.
C
      INTEGER   I
C     LDM Change to have type compatibility
      INTEGER   IDIST, CDIST
      CDIST = 1 
C
C Calculate 4-connected distance (option DIAMOND)
C
      IF (IOP.EQ.1) THEN
         IF (IPASS.EQ.1) THEN
            DO 10 I=1,NCOL
               IDIST2(I)=MIN(IDIST2(I),IDIST1(I)+1,IDIST2(I-1)+1)
   10       CONTINUE
         ELSE
            DO 20 I=NCOL,1,-1
               IDIST2(I)=MIN(IDIST2(I),IDIST1(I)+1,IDIST2(I+1)+1)
               IMAX=MAX(IMAX,IDIST2(I))
   20       CONTINUE
         ENDIF
C
C Calculate 8-connected distance (option SQUARE)
C
      ELSE IF (IOP.EQ.2) THEN
         IF (IPASS.EQ.1) THEN
            DO 30 I=1,NCOL
               IDIST2(I)=MIN(IDIST2(I),IDIST1(I)+1,IDIST2(I-1)+1,
     +                       IDIST1(I-1)+1,IDIST1(I+1)+1)
   30       CONTINUE
         ELSE
            DO 40 I=NCOL,1,-1
               IDIST2(I)=MIN(IDIST2(I),IDIST1(I)+1,IDIST2(I+1)+1,
     +                       IDIST1(I-1)+1,IDIST1(I+1)+1)
               IMAX=MAX(IMAX,IDIST2(I))
   40       CONTINUE
         ENDIF
C
C Calculate octagonal distance (option OCTAGON)
C
      ELSE IF (IOP.EQ.3) THEN
         IF (IPASS.EQ.1) THEN
            DO 50 I=1,NCOL
               IDIST2(I)=MIN(IDIST2(I),IDIST1(I)+1,IDIST2(I-1)+1)
C
               IDIST=IDIST1(I-1)
               IF (IAND(IDIST,CDIST).NE.0) THEN
                  IDIST2(I)=MIN(IDIST2(I),IDIST1(I-1)+1)
               ENDIF
C
               IDIST=IDIST1(I+1)
               IF (IAND(IDIST,CDIST).NE.0) THEN
                  IDIST2(I)=MIN(IDIST2(I),IDIST1(I+1)+1)
               ENDIF
   50       CONTINUE
C
            DO 60 I=NCOL,1,-1
               IDIST2(I)=MIN(IDIST2(I),IDIST2(I+1)+1)
   60       CONTINUE
         ELSE
            DO 70 I=NCOL,1,-1
               IDIST2(I)=MIN(IDIST2(I),IDIST1(I)+1,IDIST2(I+1)+1)
C
               IDIST=IDIST1(I-1)
               IF (IAND(IDIST,CDIST).NE.0) THEN
                  IDIST2(I)=MIN(IDIST2(I),IDIST1(I-1)+1)
               ENDIF
C
               IDIST=IDIST1(I+1)
               IF (IAND(IDIST,CDIST).NE.0) THEN
                  IDIST2(I)=MIN(IDIST2(I),IDIST1(I+1)+1)
               ENDIF
   70       CONTINUE
C
            DO 80 I=1,NCOL
               IDIST2(I)=MIN(IDIST2(I),IDIST2(I-1)+1)
               IMAX=MAX(IMAX,IDIST2(I))
   80       CONTINUE
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
