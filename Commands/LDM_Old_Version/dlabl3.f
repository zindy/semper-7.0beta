C Semper 6 subsidiary module DLABL3
C
      LOGICAL FUNCTION DLABL3(IPASS,LABEL,DX,DY,ROW,NCOL,NROW,
     +                         LZERO,LNZERO)
C
      INTEGER IPASS,LABEL(0:*),DX(0:*),DY(0:*),ROW,NCOL,NROW
      LOGICAL LZERO,LNZERO
C
C Reads data from row ROW of picture LP1 (IPASS = 1) or picture LP3
C (IPASS = 2) and sets edge pixels to MAXVAL.  If IPASS = 1, distance
C values are initialised to zero or MAXVAL and range flags LZERO and
C LNZERO are set appropriately.
C
      LOGICAL SEMROW
C
      INTEGER I
C
      INCLUDE 'COMMON'
C
      INTEGER MAXVAL
      PARAMETER ( MAXVAL = 32766 )
C
      DLABL3=.TRUE.
C
C If edge row, set all label values to zero and all distance offsets
C to MAXVAL
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) THEN
         DO 10 I=1,NCOL
            LABEL(I)=0
            DX(I)=MAXVAL
            DY(I)=MAXVAL
   10    CONTINUE
C
C Otherwise, read data from disc
C
      ELSE
C
C See if first pass
C
         IF (IPASS.EQ.1) THEN
C
C Read source picture row ...
C
            IF (SEMROW(1,LABEL(1),NFMINT,ROW,1,LP1)) GOTO 30
C
C ... and initialise distance offsets
C
            DO 20 I=1,NCOL
               IF (LABEL(I).LT.0) LABEL(I)=0
C
               IF (LABEL(I).EQ.0) THEN
                  DX(I)=MAXVAL
                  DY(I)=MAXVAL
                  LZERO=.TRUE.
               ELSE
                  DX(I)=0
                  DY(I)=0
                  LNZERO=.TRUE.
               ENDIF
   20       CONTINUE
C
C Otherwise, read intermediate label information and distance offsets
C
         ELSE
            IF (SEMROW(1,LABEL(1),NFMINT,ROW,1,LP3)) GOTO 30
            IF (SEMROW(1,   DX(1),NFMINT,ROW,2,LP3)) GOTO 30
            IF (SEMROW(1,   DY(1),NFMINT,ROW,3,LP3)) GOTO 30
         ENDIF
      ENDIF
C
C Set left and right edge label values to zero and distance offsets
C to MAXVAL
C
      LABEL(0)=0
      DX(0)=MAXVAL
      DY(0)=MAXVAL
C
      LABEL(NCOL+1)=0
      DX(NCOL+1)=MAXVAL
      DY(NCOL+1)=MAXVAL
C
      DLABL3=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DLABL4
C
      SUBROUTINE DLABL4(IPASS,LABEL1,DX1,DY1,LABEL2,DX2,DY2,NCOL)
C
      INTEGER IPASS,NCOL
      INTEGER LABEL1(0:*),DX1(0:*),DY1(0:*)
      INTEGER LABEL2(0:*),DX2(0:*),DY2(0:*)
C
C Carries out basic row processing for calculating Euclidean distances
C and propagating pixel labels.  Negative pixel labels are used to flag
C pixels which are equidistant from more than one labelled region.  Data
C for previous and current rows is passed via arrays LABEL1, DX1, DX2
C and LABEL2, DX2, DY2 respectively.  Processing proceeds from top to
C bottom during the first pass (IPASS=1) and in the reverse direction on
C the second pass (IPASS=2).
C
      INTEGER   I,LABEL
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
               LABEL=ABS(LABEL1(I))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX2(I-1)-1
               DY=DY2(I-1)
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL2(I-1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX1(I-1)-1
               DY=DY1(I-1)+1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I-1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
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
               DX=DX1(I)
               DY=DY1(I)+1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX2(I+1)+1
               DY=DY2(I+1)
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL2(I+1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX1(I+1)+1
               DY=DY1(I+1)+1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I+1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
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
               DY=DY1(I)-1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX2(I+1)+1
               DY=DY2(I+1)
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL2(I+1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX1(I+1)+1
               DY=DY1(I+1)-1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I+1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
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
               DX=DX1(I)
               DY=DY1(I)-1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX2(I-1)-1
               DY=DY2(I-1)
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL2(I-1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
C
               DX=DX1(I-1)-1
               DY=DY1(I-1)-1
               DN=DX*DX+DY*DY
               LABEL=ABS(LABEL1(I-1))
C
               IF (DN.LT.DD) THEN
                  DX2(I)=DX
                  DY2(I)=DY
                  DD=DN
                  LABEL2(I)=LABEL
               ELSE IF (DN.EQ.DD) THEN
                  IF (ABS(LABEL2(I)).NE.LABEL) THEN
                     LABEL2(I)=-ABS(LABEL2(I))
                  ENDIF
               ENDIF
            ENDIF
   40    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
