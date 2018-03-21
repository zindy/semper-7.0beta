C Semper 6 processing module WEIGHT
C
      SUBROUTINE WEIGHT
C
C Multiplies the source picture by a circularly symmetric function
C in LP3, centred on the source picture origin, or adds the same
C function, if the option ADD is set.  The centre position can be
C offset from the picture origin by means of the keys POSITION and PO2.
C
      LOGICAL OPT,SEMROW
      INTEGER IVALPN
      REAL VAL
C
      INCLUDE 'COMMON'
C
      LOGICAL ADD
      INTEGER CCOL,CROW
      REAL B3(0:255),B4(0:255),B5(0:255),B6(0:255)
      EQUIVALENCE (B3,RB3),(B4,RB4),(B5,RB5),(B6,RB6)
      INTEGER I,IR,NCOL,NROW,J,IRMAX
      REAL R,X0,Y0,Y,YY,X,XX,RU,RV,DR,U,V
C
C Packed names
      INTEGER NADD,NWITH,NPOS,NPO2
      PARAMETER (NADD=1764, NWITH=-5181, NPOS=26219, NPO2=26232)
C
C Fault multi-layer source or output picture
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C Fault function picture which is not 1-D
      IF (NROWS(LP3).NE.1.OR.NLAYS(LP3).NE.1) THEN
         ERROR=5
         IDERR=IVALPN(NWITH)
         RETURN
      ENDIF
C
C Read function data
      IF (SEMROW(1,RB2,NFMCOM,1,1,LP3)) RETURN
C
C Determine number of points from centre to right-hand end of row
      IRMAX=NCOLS(LP3)-CCOLN(LP3)
C
C Split real and imaginary parts into separate row buffers
      I=2*CCOLN(LP3)-1
      DO 10 IR=0,IRMAX
         B3(IR)=RB2(I)
         B4(IR)=RB2(I+1)
         I=I+2
   10 CONTINUE
C
C Calculate differences between function values
      DO 20 IR=0,IRMAX-1
         B5(IR)=B3(IR+1)-B3(IR)
         B6(IR)=B4(IR+1)-B4(IR)
   20 CONTINUE
C
C Fetch source picture size and centre position
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Determine centre position (including offset) in pixel coordinates
      X0=REAL(CCOL)+VAL(NPOS)
      Y0=REAL(CROW)-VAL(NPO2)
C
C See if option ADD set
      ADD=OPT(NADD)
C
C Process source picture
      DO 40 J=1,NROW
C
C Read source row from LP1
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
C
C Determine Y picture coordinate
         Y=Y0-REAL(J)
         YY=Y*Y
C
C Initialise X picture coordinate
         X=-X0
C
C Process each pixel in row
         DO 30 I=1,2*NCOL,2
C
C Increment X picture coordinate
            X=X+1.0
            XX=X*X
C
C Calculate distance (radius) from centre position
            R=SQRT(XX+YY)
C
C Determine function value by interpolation
            IR=INT(R)
            IF (IR.GE.IRMAX) THEN
               RU=B3(IRMAX)
               RV=B4(IRMAX)
            ELSE
               DR=R-REAL(IR)
               RU=B3(IR)+DR*B5(IR)
               RV=B4(IR)+DR*B6(IR)
            ENDIF
C
C If option ADD set, add complex function value
            IF (ADD) THEN
               RB1(I)=RB1(I)+RU
               RB1(I+1)=RB1(I+1)+RV
C
C Otherwise, multiply by complex function value
            ELSE
               U=RB1(I)
               V=RB1(I+1)
               RB1(I)=RU*U-RV*V
               RB1(I+1)=RV*U+RU*V
            ENDIF
   30    CONTINUE
C
C Store result in LP2
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
   40 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
