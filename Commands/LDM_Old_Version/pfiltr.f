C Semper 6 processing module PFILTR
C
      SUBROUTINE PFILTR
C
C Approximates a curve with a smaller number of straight-line sections.
C The curve is defined by the series of positions stored in the source
C position list.  PFILTR faults a source picture with less than two
C columns or layers and more than one row.  The TOLERANCE key specifies
C the tolerance for the approximation.  All source data layers are
C transferred from the source picture to the output picture.  The length
C of the curve is returned in variable P.  If position list defines a
C closed curve, the area enclosed by the curve is returned in the
C variable A.
C
C Syntax:  Pfilter :PFILTR  $1=sel from=$1 $2=from to=$2 tolerance=1 +
C                           open(lp1,old)=from
C
      REAL    VAL
      INTEGER IVALPN
      LOGICAL SEMROW,SEMOPN,SEMLAB,SEMLU
C
      REAL    TOL,P,A
      INTEGER NPIC,NCOL,NROW,NLAY,NGOOD,I,K
      LOGICAL CLOSED
C
      INCLUDE 'COMMON'
C
      INTEGER LABEL(LNLAB),IB3(LNBUF/LNINT)
      EQUIVALENCE (LABEL,RB1),(IB3,RB3)
C
C Packed names
C
      INTEGER NTOLER,NTO,NP,NA
      PARAMETER (NTOLER=-613, NTO=-601,NP=25600, NA=1600)
C
C Fetch source picture number and size
C
      NPIC=1000*DEVN(LP1)+PICN(LP1)
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Fault source picture with less than 2 columns, more than one row and
C less than 2 layers
C
      IF(NCOL.LT.2.OR.NROW.GT.1.OR.NLAY.LT.2) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 50
      ENDIF
C
C Fault source picture which is not a position list
C
      IF(CLASSN(LP1).NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 50
      ENDIF
C
C Fetch value for TOLERANCE key
C
      TOL=VAL(NTOLER)
C
C Fault bad value for TOLERANCE key
C
      IF(TOL.LT.0.0) THEN
         ERROR=3
         IDERR=NTOLER
         GOTO 50
      ENDIF
C
C Read the X and Y position data from the source picture
C
      IF(SEMROW(1,RB1,NFMFP,1,1,LP1)) GOTO 50
      IF(SEMROW(1,RB2,NFMFP,1,2,LP1)) GOTO 50
C
C Filter the source data
C
      CALL PFILT2(RB1,RB2,1,NCOL,IB3,NGOOD,TOL)
C
C Open the output picture
C
      LP2=LP1
      IF(SEMOPN(2,IVALPN(NTO),NGOOD,1,NLAY,NCLPLI,NFMFP,LP2)) GOTO 50
C
C Fetch output picture label
C
      IF (SEMLAB(1,LABEL,LP2)) GOTO 50
C
C See if position list type is closed curve and if not, force type for
C open curve
C
      CLOSED=LABEL(LBPLTY).EQ.3
C
      IF (.NOT.CLOSED) THEN
         LABEL(LBPLTY)=2
C
         IF (SEMLAB(2,LABEL,LP2)) GOTO 50
      ENDIF
C
C Transfer the data from source to output picture assembling the
C filtered data for each output layer
C
      DO 20 K=1,NLAY
C
C Read source data
C
         IF(SEMROW(1,RB1,NFMFP,1,K,LP1)) GOTO 50
C
C Assemble filtered data
C
         DO 10 I=1,NGOOD
            RB1(I)=RB1(IB3(I))
   10    CONTINUE
C
C Store result in output picture
C
         IF(SEMROW(2,RB1,NFMFP,1,K,LP2)) GOTO 50
   20 CONTINUE
C
C Fetch X and Y position data from output picture
C
      IF(SEMROW(1,RB1,NFMFP,1,1,LP2)) GOTO 50
      IF(SEMROW(1,RB2,NFMFP,1,2,LP2)) GOTO 50
C
C If closed curve, add extra point = start point
C
      IF (CLOSED) THEN
         NGOOD=NGOOD+1
         RB1(NGOOD)=RB1(1)
         RB2(NGOOD)=RB2(1)
      ENDIF
C
C Determine curve length and return in variable P
C
      P=0.0
      DO 30 I=1,NGOOD-1
         P=P+SQRT((RB1(I+1)-RB1(I))**2+(RB2(I+1)-RB2(I))**2)
   30 CONTINUE
C
      IF (SEMLU(1,NP,P)) GOTO 50
C
C If closed curve, determine enclosed area and return value in
C variable A
C
      IF (CLOSED) THEN
         A=0.0
         DO 40 I=1,NGOOD
            A=A+(RB1(I)*RB2(I+1)-RB1(I+1)*RB2(I))/2.0
   40    CONTINUE
C
         IF(SEMLU(1,NA,A)) GOTO 50
      ENDIF
C
   50 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module PFILT2
C
      SUBROUTINE PFILT2(X,Y,N1,N2,IGOOD,NGOOD,TOL)
C
C Given a set of points (X(N1),Y(N1)) to (X(N2),Y(N2)) defining a curve,
C PFILT2 returns a subset of the points (X(IGOOD(I)),Y(IGOOD(I)))
C between N1 and N2 inclusive, where I = 1,NGOOD.  The points
C approximate the original curve to tolerance TOL.  N2 must be greater
C than or equal than N1.  NGOOD returns the number of good points.  It
C is assumed that IGOOD is dimensioned at least as big as (N2-N1+1) so
C that it is always big enough to hold the filtered results.  IGOOD(1)
C is always set to N1 and IGOOD(NGOOD) = N2.  IGOOD may be too small to
C accomodate the required number of points but PFILT2 nevertheless
C returns IGOOD(NGOOD) pointing to the last included good point.
C
      REAL    X(*),Y(*),TOL
      INTEGER N1,N2,IGOOD(*),NGOOD
C
      LOGICAL PFILT3
C
      INTEGER I
C
      NGOOD=1
      IGOOD(1)=N1
C
      DO 10 I=N1+2,N2
         IF(PFILT3(X,Y,IGOOD(NGOOD),I,TOL)) THEN
            NGOOD=NGOOD+1
            IGOOD(NGOOD)=I-1
         ENDIF
   10 CONTINUE
C
      NGOOD=NGOOD+1
      IGOOD(NGOOD)=N2
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module PFILT3
C
      LOGICAL FUNCTION PFILT3(X,Y,N1,N2,TOL)
C
C Returns .TRUE. if the line from the point (X(N1),Y(N1)) to
C (X(N2),Y(N2)) fails to approximate all points (X(I),Y(I))
C for N1 < I < N2 within tolerance TOL.  N2 must be greater than
C or equal than N1.
C
      REAL X(*),Y(*),TOL
      INTEGER N1,N2
C
      LOGICAL PFILT4
      INTEGER I
C
      PFILT3=.TRUE.
C
      DO 10 I=N1+1,N2-1
         IF(PFILT4(X(N1),Y(N1),X(N2),Y(N2),X(I),Y(I),TOL)) GOTO 20
   10 CONTINUE
C
      PFILT3=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module PFILT4
C
      LOGICAL FUNCTION PFILT4(X1,Y1,X2,Y2,X,Y,TOL)
C
C Returns .TRUE. if distance between the point (X,Y) and the line
C segment between (X1,Y1) and (X2,Y2) exceeds the value of TOL.
C
      REAL X1,Y1,X2,Y2,X,Y,TOL
C
      REAL DX,DY,DX1,DY1,DX2,DY2,DD,P
C
C Calculate offsets between three points
C
      DX=X2-X1
      DY=Y2-Y1
      DX1=X-X1
      DY1=Y-Y1
      DX2=X-X2
      DY2=Y-Y2
C
C Length squared of line segment
C
      DD=DX*DX+DY*DY
C
C Project point onto the line segment
C Note:  0 < P < DD if point projects directly onto the line segment
C
      P=DX1*DX+DY1*DY
C
C See if point lies close enough to line segment
C
      IF(P.LT.0.0) THEN
         PFILT4=DX1*DX1+DY1*DY1.GT.TOL*TOL
      ELSE IF(P.GT.DD) THEN
         PFILT4=DX2*DX2+DY2*DY2.GT.TOL*TOL
      ELSE
         P=DY1*DX-DX1*DY
         PFILT4=P*P.GT.TOL*TOL*DD
      ENDIF
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
