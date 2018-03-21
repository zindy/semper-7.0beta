C Semper 6 processing module BAKPRO
C
      SUBROUTINE BAKPRO
C
C Adds to all points in source picture LP1 the interpolated value
C derived from the projection in LP3.  Points are multiplied by the
C the projection value if option MULTIPLY set.  The result goes to LP2.
C
      REAL VAL
      INTEGER IVALPN
      LOGICAL SEMROW,OPT
C
      INCLUDE 'COMMON'
C
      REAL ANGLE,C,DP,P,PU,PV,S,U,V
      INTEGER CCOL,CROW,I,IP,IPCEN,IPSIZE,J,NCOL,NROW
      LOGICAL MULT
C
C Packed names
C
      INTEGER NANGLE,NMULTI,NWITH
      PARAMETER (NANGLE=2167,NMULTI=21652,NWITH=-5181)
C
C Fault multi-layer source or output picture
C
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 50
      ENDIF
C
C Fault projection which is not 1-D
C
      IF (NROWS(LP3).NE.1.OR.NLAYS(LP3).NE.1) THEN
         ERROR=5
         IDERR=IVALPN(NWITH)
         GOTO 50
      ENDIF
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Fetch projection size and centre position
C
      IPSIZE=NCOLS(LP3)
      IPCEN=CCOLN(LP3)
C
C Fetch projection angle
C
      ANGLE=VAL(NANGLE)
      C=COS(ANGLE)
      S=SIN(ANGLE)
C
C Look for option MULTIPLY
C
      MULT=OPT(NMULTI)
C
C Read projection values from LP3
C
      IF (SEMROW(1,RB1,NFMCOM,1,1,LP3)) GOTO 50
C
C Split real and imaginary parts of projection into separate row buffers
C
      I = 1
      DO 10 IP=1,IPSIZE
         RB3(IP) = RB1(I)
         RB4(IP) = RB1(I+1)
         I = I+2
   10 CONTINUE
C
C Calculate differences between projection values
C
      DO 20 IP=1,IPSIZE-1
         RB5(IP)=RB3(IP+1)-RB3(IP)
         RB6(IP)=RB4(IP+1)-RB4(IP)
   20 CONTINUE
C
C Generate output rows
C
      DO 40 J=1,NROW
C
C Read source row from LP1
C
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 50
C
C Calculate initial projected coordinate
C
         P=REAL(IPCEN)+S*REAL(-CCOL)+C*REAL(J-CROW)
C
C Apply projected value for each point in source row
C
         DO 30 I=1,2*NCOL,2
C
C Determine projected coordinate
C
            P=P+S
            IP=INT(P)
C
C Determine interpolated projection value
C
            IF (IP.LT.1) THEN
               PU=RB3(1)
               PV=RB4(1)
            ELSE IF (IP.GE.IPSIZE) THEN
               PU=RB3(IPSIZE)
               PV=RB4(IPSIZE)
            ELSE
               DP=P-REAL(IP)
               PU=RB3(IP)+DP*RB5(IP)
               PV=RB4(IP)+DP*RB6(IP)
            ENDIF
C
C Apply projected value
C
            IF (MULT) THEN
               U=RB1(I)
               V=RB1(I+1)
               RB2(I)=PU*U-PV*V
               RB2(I+1)=PV*U+PU*V
            ELSE
               RB2(I)=RB1(I)+PU
               RB2(I+1)=RB1(I+1)+PV
            ENDIF
   30    CONTINUE
C
C Store result in LP2
C
         IF (SEMROW(2,RB2,NFMCOM,J,1,LP2)) GOTO 50
   40 CONTINUE
C
   50 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
