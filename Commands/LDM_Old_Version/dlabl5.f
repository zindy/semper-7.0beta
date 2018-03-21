C Semper 6 subsidiary module DLABL5
C
      LOGICAL FUNCTION DLABL5(IPASS,LABEL,DIST,ROW,NCOL,NROW,
     +                        LZERO,LNZERO)
C
      INTEGER IPASS,LABEL(0:*),DIST(0:*),ROW,NCOL,NROW
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
      DLABL5=.TRUE.
C
C If edge row, initialise all label values to zero and all distance
C values to MAXVAL
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) THEN
         DO 10 I=1,NCOL
            LABEL(I)=0
            DIST(I)=MAXVAL
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
C ... and initialise distance values
C
            DO 20 I=1,NCOL
               IF (LABEL(I).LT.0) LABEL(I)=0
C
               IF (LABEL(I).EQ.0) THEN
                  DIST(I)=MAXVAL
                  LZERO=.TRUE.
               ELSE
                  DIST(I)=0
                  LNZERO=.TRUE.
               ENDIF
   20       CONTINUE
C
C Otherwise, read intermediate label information and distance values
C
         ELSE
            IF (SEMROW(1,LABEL(1),NFMINT,ROW,1,LP3)) GOTO 30
            IF (SEMROW(1, DIST(1),NFMINT,ROW,2,LP3)) GOTO 30
         ENDIF
      ENDIF
C
C Set left and right edge label values to zero and distance values
C to MAXVAL
C
      LABEL(0)=0
      DIST(0)=MAXVAL
C
      LABEL(NCOL+1)=0
      DIST(NCOL+1)=MAXVAL
C
      DLABL5=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DLABL6
C
      SUBROUTINE DLABL6(IPASS,IOP,LABEL1,DIST1,LABEL2,DIST2,NCOL)
C
      INTEGER IPASS,IOP,NCOL
      INTEGER LABEL1(0:*),DIST1(0:*),LABEL2(0:*),DIST2(0:*)
C
C Carries out basic row processing for calculating diamond, square or
C octagonal distances (IOP specifies which) and propagating pixel
C labels.  Negative pixel labels are used to flag pixels which are
C equidistant from more than one labelled region.  Data for previous
C and current rows are passed via arrays LABEL1, DIST1 and LABEL2,
C DIST2 respectively.  Processing proceeds from top to bottom during
C the first pass (IPASS=1) and in the reverse direction on the second
C pass (IPASS=2).
C
      INTEGER   I,DIST,LABEL
C      INTEGER*2 IDIST,IDCHECK
C     LDM: Added so it has the same type as IDIST
C     Changed, August 2012
      INTEGER IDIST, IDCHECK
      IDCHECK=1
C
C Calculate 4-connected distance (option DIAMOND)
C
      IF (IOP.EQ.1) THEN
         IF (IPASS.EQ.1) THEN
            DO 10 I=1,NCOL
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST1(I)+1
                  LABEL=ABS(LABEL1(I))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST2(I-1)+1
                  LABEL=ABS(LABEL2(I-1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
               ENDIF
   10       CONTINUE
         ELSE
            DO 20 I=NCOL,1,-1
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST1(I)+1
                  LABEL=ABS(LABEL1(I))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST2(I+1)+1
                  LABEL=ABS(LABEL2(I+1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
               ENDIF
   20       CONTINUE
         ENDIF
C
C Calculate 8-connected distance (option SQUARE)
C
      ELSE IF (IOP.EQ.2) THEN
         IF (IPASS.EQ.1) THEN
            DO 30 I=1,NCOL
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST1(I)+1
                  LABEL=ABS(LABEL1(I))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST2(I-1)+1
                  LABEL=ABS(LABEL2(I-1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST1(I-1)+1
                  LABEL=ABS(LABEL1(I-1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST1(I+1)+1
                  LABEL=ABS(LABEL1(I+1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
               ENDIF
   30       CONTINUE
         ELSE
            DO 40 I=NCOL,1,-1
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST1(I)+1
                  LABEL=ABS(LABEL1(I))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST2(I+1)+1
                  LABEL=ABS(LABEL2(I+1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST1(I-1)+1
                  LABEL=ABS(LABEL1(I-1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST1(I+1)+1
                  LABEL=ABS(LABEL1(I+1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
               ENDIF
   40       CONTINUE
         ENDIF
C
C Calculate octagonal distance (option OCTAGON)
C
      ELSE IF (IOP.EQ.3) THEN
         IF (IPASS.EQ.1) THEN
            DO 50 I=1,NCOL
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST1(I)+1
                  LABEL=ABS(LABEL1(I))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST2(I-1)+1
                  LABEL=ABS(LABEL2(I-1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  IDIST=DIST1(I-1)
C
C       Change here -- use same type ?
                  IF (IAND(IDIST,IDCHECK).NE.0) THEN
                     DIST=DIST1(I-1)+1
                     LABEL=ABS(LABEL1(I-1))
C
                     IF (DIST.LT.DIST2(I)) THEN
                        DIST2(I)=DIST
                        LABEL2(I)=LABEL
                     ELSE IF (DIST.EQ.DIST2(I)) THEN
                        IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                     ENDIF
                  ENDIF
C
                  IDIST=DIST1(I+1)
C
                  IF (IAND(IDIST,IDCHECK).NE.0) THEN
                     DIST=DIST1(I+1)+1
                     LABEL=ABS(LABEL1(I+1))
C
                     IF (DIST.LT.DIST2(I)) THEN
                        DIST2(I)=DIST
                        LABEL2(I)=LABEL
                     ELSE IF (DIST.EQ.DIST2(I)) THEN
                        IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                     ENDIF
                  ENDIF
               ENDIF
   50       CONTINUE
C
            DO 60 I=NCOL,1,-1
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST2(I+1)+1
                  LABEL=ABS(LABEL2(I+1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
               ENDIF
   60       CONTINUE
         ELSE
            DO 70 I=NCOL,1,-1
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST1(I)+1
                  LABEL=ABS(LABEL1(I))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  DIST=DIST2(I+1)+1
                  LABEL=ABS(LABEL2(I+1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
C
                  IDIST=DIST1(I-1)
C
                  IF (IAND(IDIST,IDCHECK).NE.0) THEN
                     DIST=DIST1(I-1)+1
                     LABEL=ABS(LABEL1(I-1))
C
                     IF (DIST.LT.DIST2(I)) THEN
                        DIST2(I)=DIST
                        LABEL2(I)=LABEL
                     ELSE IF (DIST.EQ.DIST2(I)) THEN
                        IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                     ENDIF
                  ENDIF
C
                  IDIST=DIST1(I+1)
C
                  IF (IAND(IDIST,IDCHECK).NE.0) THEN
                     DIST=DIST1(I+1)+1
                     LABEL=ABS(LABEL1(I+1))
C
                     IF (DIST.LT.DIST2(I)) THEN
                        DIST2(I)=DIST
                        LABEL2(I)=LABEL
                     ELSE IF (DIST.EQ.DIST2(I)) THEN
                        IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                     ENDIF
                  ENDIF
               ENDIF
   70       CONTINUE
C
            DO 80 I=1,NCOL
               IF (DIST2(I).GT.0) THEN
                  DIST=DIST2(I-1)+1
                  LABEL=ABS(LABEL2(I-1))
C
                  IF (DIST.LT.DIST2(I)) THEN
                     DIST2(I)=DIST
                     LABEL2(I)=LABEL
                  ELSE IF (DIST.EQ.DIST2(I)) THEN
                     IF (ABS(LABEL2(I)).NE.LABEL) LABEL2(I)=-LABEL
                  ENDIF
               ENDIF
   80       CONTINUE
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
