C Semper 6 processing module CHULL
C
      SUBROUTINE CHULL
C
C Generate convex hull of foreground regions in source picture.
C Returns area and perimeter for convex hull in Semper variables
C A and P.
C
      LOGICAL SEMROW,SEMOPN,SEMRNG,SETVAR
      INTEGER IVALPN !,IPACK
C
      REAL      AREA,PERIM
      INTEGER   I,J,BJ,I0,I1,I2,J0,J1,J2,NJ
      INTEGER   IMIN,IMAX,JMIN,JMAX,PMIN,PMAX
      INTEGER   NCOL,NROW,NPIC,NH,NV1,NV2,NHVRUN
      INTEGER*4 DI1,DI2,DJ1,DJ2,ROUND,NCOL4
C
      INCLUDE 'COMMON'
C
      INTEGER PIXEL(LNBUF/LNINT)
      INTEGER HVRUN(LNBUF/LNINT)
      INTEGER RBMIN(LNBUF/LNINT)
      INTEGER RBMAX(LNBUF/LNINT)
      INTEGER RFMIN(LNBUF/LNINT)
      INTEGER RFMAX(LNBUF/LNINT)
      INTEGER RIMIN(LNBUF/LNINT)
      INTEGER RIMAX(LNBUF/LNINT)
C
      EQUIVALENCE (PIXEL,RB1)
      EQUIVALENCE (HVRUN,RB2)
      EQUIVALENCE (RBMIN,RB1),(RBMAX,RB2)
      EQUIVALENCE (RFMIN,RB3),(RFMAX,RB4)
      EQUIVALENCE (RIMIN,RB5),(RIMAX,RB6)
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault source picture with too many rows to process
C
      IF (NROW.GT.LNBUF/LNINT) THEN
         ERROR=5
         IDERR=1000*DEVN(LP1)+PICN(LP1)
         GOTO 230
      ENDIF
C
C Initialise non-empty row count, top and bottom extents and range
C values for source picture
C
      NJ=0
C
      JMIN=NROW+1
      JMAX=0
C
      PMIN=1
      PMAX=0
C
C Scan source picture for row left and right extents, top and bottom
C extents and data range
C
      DO 50 J=1,NROW
C
C Fetch row of data
C
         IF (SEMROW(1,PIXEL,NFMINT,J,1,LP1)) GOTO 230
C
C Scan row for first foreground pixel
C
         DO 10 IMIN=1,NCOL
            IF (PIXEL(IMIN).NE.0) GOTO 20
   10    CONTINUE
C
C Scan row for last foreground pixel
C
   20    DO 30 IMAX=NCOL,IMIN,-1
            IF (PIXEL(IMAX).NE.0) GOTO 40
   30    CONTINUE
C
C Store row extents
C
   40    RIMIN(J)=IMIN
         RIMAX(J)=IMAX
C
C See if row is not empty
C
         IF (IMIN.LE.IMAX) THEN
C
C Increment non-empty row count
C
            NJ=NJ+1
C
C Update top and bottom extents
C
            IF (J.LT.JMIN) JMIN=J
            IF (J.GT.JMAX) JMAX=J
         ENDIF
C
C Update range values for source picture
C
         IF (IMIN.GT.1.OR.IMAX.LT.NCOL) PMIN=0
C
         IF (IMIN.LE.IMAX) PMAX=1
   50 CONTINUE
C
C Open output picture
C
      NPIC=IVALPN(-601)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,NFMBYT,LP2)) GOTO 230
C
C Check for picture with little or no structure in it
C
      IF (NJ.EQ.1.OR.PMIN.EQ.PMAX) GOTO 240
C
C If two non-empty rows, set up linked list required for later
C interpolation phase
C
      IF (NJ.EQ.2) THEN
         RFMIN(JMIN)=JMAX
         RFMAX(JMIN)=JMAX
C
C Otherwise, if three or more non-empty rows execute main part of
C convex hull algorithm
C
      ELSE IF (NJ.GE.3) THEN
C
C Set up linked lists for left and right-hand profiles
C
         BJ=0
C
         DO 60 J=JMIN,JMAX
C
C See if row is not empty
C
            IF (RIMIN(J).LE.RIMAX(J)) THEN
C
C Set up links for left and right profiles
C
               IF (BJ.NE.0) THEN
                  RBMIN(J)=BJ
                  RBMAX(J)=BJ
C
                  RFMIN(BJ)=J
                  RFMAX(BJ)=J
               ENDIF
C
               BJ=J
            ENDIF
   60    CONTINUE
C
C Find pixels which lie on left-hand side of convex hull
C
         J0=JMIN
         J1=RFMIN(J0)
         J2=RFMIN(J1)
C
C Fetch column positions for current vertices (I0,J0),(I1,J1),(I2,J2)
C
   70    I0=RIMIN(J0)
         I1=RIMIN(J1)
         I2=RIMIN(J2)
C
C Calculate coordinate differences
C
         DI1=I1-I0
         DJ1=J1-J0
C
         DI2=I2-I0
         DJ2=J2-J0
C
C See if middle vertex (I1,J1) lies to the left of the line between
C first and last vertices (I0,J0) and (I2,J2), in which case we have a
C convex arc
C
         IF (DI1*DJ2.LT.DI2*DJ1) THEN
C
C If not at end of list, go forwards to next vertex
C
            IF (J2.LT.JMAX) THEN
               J0=J1
               J1=J2
               J2=RFMIN(J2)
C
C Otherwise, the job is done
C
            ELSE
               GOTO 80
            ENDIF
C
C Otherwise arc is straight or concave and the middle vertex can be
C deleted
C
         ELSE
C
C Delete middle vertex from linked list
C
            RFMIN(J0)=J2
            RBMIN(J2)=J0
C
C If not at start of list, go backwards to previous vertex
C
            IF (J0.GT.JMIN) THEN
               J1=J0
               J0=RBMIN(J0)
C
C Otherwise, if not at end of list, go forwards to next vertex
C
            ELSE IF (J2.LT.JMAX) THEN
               J1=J2
               J2=RFMIN(J2)
C
C Otherwise, the job is done
C
            ELSE
               GOTO 80
            ENDIF
         ENDIF
C
C Go back for more
C
         GOTO 70
C
C Find pixels which lie on right-hand side of convex hull
C
   80    J0=JMIN
         J1=RFMAX(J0)
         J2=RFMAX(J1)
C
C Fetch column positions for current vertices (I0,J0),(I1,J1),(I2,J2)
C
   90    I0=RIMAX(J0)
         I1=RIMAX(J1)
         I2=RIMAX(J2)
C
C Calculate coordinate differences
C
         DI1=I1-I0
         DJ1=J1-J0
C
         DI2=I2-I0
         DJ2=J2-J0
C
C See if middle vertex (I1,J1) lies to the right of the line between
C first and last vertices (I0,J0) and (I2,J2), in which case we have a
C convex arc
C
         IF (DI1*DJ2.GT.DI2*DJ1) THEN
C
C If not at end of list, go forwards to next vertex
C
            IF (J2.LT.JMAX) THEN
               J0=J1
               J1=J2
               J2=RFMAX(J2)
C
C Otherwise, the job is done
C
            ELSE
               GOTO 100
            ENDIF
C
C Otherwise arc is straight or concave and the middle vertex can be
C deleted
C
         ELSE
C
C Delete middle vertex from linked list
C
            RFMAX(J0)=J2
            RBMAX(J2)=J0
C
C If not at start of list, go backwards to previous vertex
C
            IF (J0.GT.JMIN) THEN
               J1=J0
               J0=RBMAX(J0)
C
C Otherwise, if not at end of list, go forwards to next vertex
C
            ELSE IF (J2.LT.JMAX) THEN
               J1=J2
               J2=RFMAX(J2)
C
C Otherwise, the job is done
C
            ELSE
               GOTO 100
            ENDIF
         ENDIF
C
C Go back for more
C
         GOTO 90
      ENDIF
C
C Interpolate between vertices on left-hand edge of convex hull
C
  100 J2=JMIN
C
C Fetch row positions for current pair of vertices (I0,J0) and (I2,J2)
C
  110 J0=J2
      J2=RFMIN(J0)
C
C Fetch column positions for current pair of vertices
C
      I0=RIMIN(J0)
      I2=RIMIN(J2)
C
C Calculate coordinate differences
C
      DI2=I2-I0
      DJ2=J2-J0
C
C Calculate value for rounding to nearest pixel
C
      IF (DI2.LT.0) THEN
         ROUND=-DJ2/2
      ELSE
         ROUND=DJ2/2
      ENDIF
C
C Interpolate all intermediate row positions
C
      DO 120 J1=J0+1,J2-1
         DJ1=J1-J0
         DI1=(DI2*DJ1+ROUND)/DJ2
         RIMIN(J1)=I0+DI1
  120 CONTINUE
C
C Go back for more
C
      IF (J2.LT.JMAX) GOTO 110
C
C Interpolate between vertices on right-hand edge of convex hull
C
      J2=JMIN
C
C Fetch row positions for current pair of vertices (I0,J0) and (I2,J2)
C
  130 J0=J2
      J2=RFMAX(J0)
C
C Fetch column positions for current pair of vertices
C
      I0=RIMAX(J0)
      I2=RIMAX(J2)
C
C Calculate coordinate differences
C
      DI2=I2-I0
      DJ2=J2-J0
C
C Calculate value for rounding to nearest pixel
C
      IF (DI2.LT.0) THEN
         ROUND=-DJ2/2
      ELSE
         ROUND=DJ2/2
      ENDIF
C
C Interpolate all intermediate row positions
C
      DO 140 J1=J0+1,J2-1
         DJ1=J1-J0
         DI1=(DI2*DJ1+ROUND)/DJ2
         RIMAX(J1)=I0+DI1
  140 CONTINUE
C
C Go back for more
C
      IF (J2.LT.JMAX) GOTO 130
C
C Initialise range values for output picture
C
      PMIN=1
      PMAX=0
C
C Initialise area sum
C
      AREA=0.0
C
C Output scan-converted result for convex hull
C
      DO 180 J=1,NROW
C
C Fetch left and right row extents
C
         IMIN=RIMIN(J)
         IMAX=RIMAX(J)
C
C Generate pixel values for current row
C
         DO 150 I=1,IMIN-1
            PIXEL(I)=0
  150    CONTINUE
C
         DO 160 I=IMIN,IMAX
            PIXEL(I)=1
  160    CONTINUE
C
         DO 170 I=IMAX+1,NCOL
            PIXEL(I)=0
  170    CONTINUE
C
C Write scan-converted result to output picture
C
         IF (SEMROW(2,PIXEL,NFMINT,J,1,LP2)) GOTO 230
C
C Update range values for output picture
C
         IF (IMIN.GT.1.OR.IMAX.LT.NCOL) PMIN=0
C
         IF (IMIN.LE.IMAX) PMAX=1
C
C Increment area sum
C
         AREA=AREA+REAL(IMAX-IMIN+1)
  180 CONTINUE
C
C Initialise perimeter counts
C
      DO 190 I=1,MAX(NCOL,NROW)
         HVRUN(I)=0
  190 CONTINUE
C
C Count initial horizontal run
C
      NH=RIMAX(JMIN)-RIMIN(JMIN)+1
      HVRUN(NH)=HVRUN(NH)+1
C
C Initialise vertical runs
C
      NV1=1
      NV2=1
C
C Count horizontal and vertical runs around the convex hull
C
      DO 200 J=JMIN+1,JMAX
C
C Check for vertical run on left-hand side
C
         IF (RIMIN(J).EQ.RIMIN(J-1)) THEN
            NV1=NV1+1
C
C Otherwise, count vertical run and left-hand horizontal run
C
         ELSE
            HVRUN(NV1)=HVRUN(NV1)+1
            NV1=1
C
            NH=ABS(RIMIN(J)-RIMIN(J-1))
            HVRUN(NH)=HVRUN(NH)+1
         ENDIF
C
C Check for vertical run on right-hand side
C
         IF (RIMAX(J).EQ.RIMAX(J-1)) THEN
            NV2=NV2+1
C
C Otherwise, count vertical run and right-hand horizontal run
C
         ELSE
            HVRUN(NV2)=HVRUN(NV2)+1
            NV2=1
C
            NH=ABS(RIMAX(J)-RIMAX(J-1))
            HVRUN(NH)=HVRUN(NH)+1
         ENDIF
  200 CONTINUE
C
C Count remaining vertical runs
C
      HVRUN(NV1)=HVRUN(NV1)+1
      HVRUN(NV2)=HVRUN(NV2)+1
C
C Count final horizontal run
C
      NH=RIMAX(JMAX)-RIMIN(JMAX)+1
      HVRUN(NH)=HVRUN(NH)+1
C
C Initialise perimeter sum
C
      PERIM=0.0
C
C Sum perimeter counts and count horizontal and vertical runs
C
      NHVRUN=0
C
      DO 210 I=1,MAX(NCOL,NROW)
         IF (HVRUN(I).GT.0) THEN
            PERIM=PERIM+HVRUN(I)*SQRT(1.0+REAL(I)*REAL(I))
            NHVRUN=NHVRUN+HVRUN(I)
         ENDIF
  210 CONTINUE
C
C Adjust perimeter sum to account for overlap between horizontal and
C vertical runs
C
      PERIM=PERIM-REAL(NHVRUN)/SQRT(2.0)
C
C Set Semper variables A and P to area/perimeter sums for convex hull
C
  220 IF (SETVAR(1600,AREA))  GOTO 230
      IF (SETVAR(25600,PERIM)) GOTO 230
C
C Store output range in picture label
C
      IF (SEMRNG(2,REAL(PMIN),REAL(PMAX),LP2)) GOTO 230
C
  230 RETURN
C
C Deal with special case of source picture with little or no structure
C
C Initialise contents of output row buffer
C
  240 DO 250 I=1,NCOL
         PIXEL(I)=PMIN
  250 CONTINUE
C
      NCOL4=NCOL
      CALL CFORM(PIXEL,PIXEL,NFMINT,NFMBYT,NCOL4)
C
C Write data to output picture
C
      DO 260 J=1,NROW
         IF (SEMROW(2,PIXEL,NFMBYT,J,1,LP2)) GOTO 230
  260 CONTINUE
C
C See if just one non-empty row
C
      IF (NJ.EQ.1) THEN
C
C Set pixels in row
C
         J=JMIN
C
         IMIN=RIMIN(J)
         IMAX=RIMAX(J)
C
         DO 270 I=1,NCOL
            IF (I.LT.IMIN.OR.I.GT.IMAX) THEN
               PIXEL(I)=0
            ELSE
               PIXEL(I)=1
            ENDIF
  270    CONTINUE
C
C Write data to output picture
C
         IF (SEMROW(2,PIXEL,NFMINT,J,1,LP2)) GOTO 230
C
C Set up dimensions of convex hull
C
         NCOL=IMAX-IMIN+1
         NROW=1
      ENDIF
C
C Determine area and perimeter sums
C
      IF (PMAX.NE.0) THEN
         AREA=REAL(NCOL)*REAL(NROW)
         PERIM=2.0*(SQRT(1.0+REAL(NCOL)*REAL(NCOL))+
     +              SQRT(1.0+REAL(NROW)*REAL(NROW)))
         PERIM=PERIM-4.0/SQRT(2.0)
      ELSE
         AREA=0.0
         PERIM=0.0
      ENDIF
C
      GOTO 220
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
