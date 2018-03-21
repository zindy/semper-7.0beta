C Semper 6 processing module PROJEC
C
      SUBROUTINE PROJEC
C
C Calculates projection of source picture LP1.  The size of the
C projection is calculated to span the entire projected width of
C the source picture.  Option AVERAGE causes the projection totals
C to be divided by the area of the source picture which contributes
C to each sum.  Options HORIZONTAL and VERTICAL select efficient
C code to deal with these special cases.
C
      LOGICAL SEMOPN,SEMCEN,SEMROW,OPT
      LOGICAL MARSET,FSINIT,FSLINE,FSARRO,FSFLUS
      INTEGER SEMFRM,IVALPN
      REAL VAL
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,CCOL,CROW,NCOL,NROW,IPSIZE,IPCEN,IPMIN,IPMAX
      INTEGER NPIC,MARK,IP,J,I
      REAL AREA,C,S,ANGLE,P1,P2,P3,P4,PMIN,PMAX,DP1,DP2,DP,XEND,YEND
      REAL SU,SV,P,CP
      LOGICAL HORIZ,VERT,ANNOT
C
C Packed names
      INTEGER NANGLE,NAVERA,NTO,NHORIZ,NVERT
      PARAMETER (NANGLE=2167, NAVERA=2485, NTO=-601)
      PARAMETER (NHORIZ=13418, NVERT=-3419)
C
C Fault multi-layer source picture
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C Look for options HORIZONTAL and VERTICAL
      HORIZ=OPT(NHORIZ)
      VERT=OPT(NVERT)
C
C Fault conflicting options HORIZONTAL+VERTICAL
      IF (HORIZ.AND.VERT) THEN
         ERROR=60
         IDERR=NHORIZ
         IDERR2=NVERT
         RETURN
      ENDIF
C
C Fetch source picture size and centre position
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Determine projection picture size and centre position
      IF (HORIZ) THEN
C
C Source picture rows to be projected
         IPSIZE=NROW
         IPCEN=CROW
         AREA=REAL(NCOL)
         C=1.0
         S=0.0
C
C Otherwise, source picture columns to be projected
      ELSE IF (VERT) THEN
         IPSIZE=NCOL
         IPCEN=CCOL
         AREA=REAL(NROW)
         C=0.0
         S=1.0
C
C Otherwise, source picture projected at arbitrary angle
      ELSE
C
C Fetch projection angle
         ANGLE=VAL(NANGLE)
         C=COS(ANGLE)
         S=SIN(ANGLE)
C
C Determine projected coordinates for corners of source picture
         P1=S*REAL(1-CCOL)+C*REAL(1-CROW)
         P2=P1+S*REAL(NCOL-1)
         P3=P1+C*REAL(NROW-1)
         P4=P2+P3-P1
C
C Determine minimum integer projected coordinate (rounded down)
         PMIN=MIN(P1,P2,P3,P4)
         IPMIN=INT(PMIN)
         IF (REAL(IPMIN).GT.PMIN) IPMIN=IPMIN-1
C
C Determine maximum integer projected coordinate (rounded up)
         PMAX=MAX(P1,P2,P3,P4)
         IPMAX=INT(PMAX)
         IF (REAL(IPMAX).LT.PMAX) IPMAX=IPMAX+1
C
C Set projection size and centre position
         IPSIZE=IPMAX-IPMIN+1
         IPCEN=1-IPMIN
         AREA=0.0
      ENDIF
C
C Create projection picture
      NPIC=IVALPN(NTO)
      CLASS=CLASSN(LP1)
      FORM=SEMFRM(FORMN(LP1))
      LP2=LP1
      IF (SEMOPN(2,NPIC,IPSIZE,1,1,CLASS,FORM,LP2)) RETURN
C
C Reset projection centre position
      IF (SEMCEN(LP2,IPCEN,1,1)) RETURN
C
C Fetch and check value of key MARK
      IF (MARSET(ANNOT,MARK)) RETURN
C
C If key MARK appropriately set, annotate specified display picture
      IF (ANNOT) THEN
C
C Initialise display graphics
         IF (FSINIT(3,MARK)) RETURN
C
C Annotate display picture only if 2-D image
         IF (FSPTYP.EQ.1) THEN
C
C Draw projection line
            DP1=REAL(IPCEN-1)
            DP2=REAL(IPSIZE-IPCEN)
            IF (FSLINE(-S*DP1,C*DP1,S*DP2,-C*DP2)) RETURN
C
C Draw projection direction
            DP=REAL(IPSIZE/2)
            XEND=-C*DP
            YEND=-S*DP
            IF (FSARRO(XEND,YEND,XEND/3.0,YEND/3.0)) RETURN
C
C Flush graphics buffer
            IF (FSFLUS()) RETURN
         ENDIF
      ENDIF
C
C Initialise projection sums and area sums
      DO 10 IP=1,IPSIZE
         RB3(IP)=0.0
         RB4(IP)=0.0
         RB5(IP)=AREA
   10 CONTINUE
C
C Process source picture
      DO 50 J=1,NROW
C
C Read source row from LP1
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
C
C Check for horizontal projection
         IF (HORIZ) THEN
C
C Projection is sum of row values
            SU=0.0
            SV=0.0
            DO 20 I=1,2*NCOL,2
               SU=SU+RB1(I)
               SV=SV+RB1(I+1)
   20       CONTINUE
C
C Store projection sum
            RB3(J)=SU
            RB4(J)=SV
C
C Otherwise, check for vertical projection
         ELSE IF (VERT) THEN
C
C Projection is sum of column values
            DO 30 I=1,NCOL
               RB3(I)=RB3(I)+RB1(I+I-1)
               RB4(I)=RB4(I)+RB1(I+I)
   30       CONTINUE
C
C Otherwise, general projection required
         ELSE
C
C Calculate initial projected coordinate
            P=REAL(IPCEN)+S*REAL(-CCOL)+C*REAL(J-CROW)
C
C Determine contribution to projection of each pixel in this row
            DO 40 I=1,2*NCOL,2
C
C Determine projected coordinate
               P=P+S
               IP=INT(P)
C
C Add pixel's contribution to projection
               IF (IP.LT.1) THEN
                  RB3(1)=RB3(1)+RB1(I)
                  RB4(1)=RB4(1)+RB1(I+1)
                  RB5(1)=RB5(1)+1.0
               ELSE IF (IP.GE.IPSIZE) THEN
                  RB3(IPSIZE)=RB3(IPSIZE)+RB1(I)
                  RB4(IPSIZE)=RB4(IPSIZE)+RB1(I+1)
                  RB5(IPSIZE)=RB5(IPSIZE)+1.0
               ELSE
                  DP=P-REAL(IP)
                  CP=1.0-DP
                  RB3(IP)=RB3(IP)+CP*RB1(I)
                  RB4(IP)=RB4(IP)+CP*RB1(I+1)
                  RB5(IP)=RB5(IP)+CP
                  IP=IP+1
                  RB3(IP)=RB3(IP)+DP*RB1(I)
                  RB4(IP)=RB4(IP)+DP*RB1(I+1)
                  RB5(IP)=RB5(IP)+DP
               ENDIF
   40       CONTINUE
         ENDIF
   50 CONTINUE
C
C Divide projection sums by source area if option AVERAGE specified
      IF (OPT(NAVERA)) THEN
         DO 60 IP=1,IPSIZE
C
C Check for zero divide
            IF (RB5(IP).NE.0.0) THEN
               RB3(IP)=RB3(IP)/RB5(IP)
               RB4(IP)=RB4(IP)/RB5(IP)
            ENDIF
   60    CONTINUE
      ENDIF
C
C Combine real and imaginary parts of projection into ouput row buffer
      DO 70 IP=1,IPSIZE
         RB2(2*IP-1)=RB3(IP)
         RB2(2*IP)=RB4(IP)
   70 CONTINUE
C
C Store result in LP2
      IF (SEMROW(2,RB2,NFMCOM,1,1,LP2)) RETURN
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
