C Semper 6 processing module PASTE
C
      SUBROUTINE PASTE
C
C Overwrites any part of the output picture with the source picture.
C
C Keys POSITION,PO2,PO3 and LAYER,LA2 and options LEFT,RIGHT,
C TOP,BOTTOM,NEAR and FAR may be used to define, with respect to the
C source picture, the region to overwrite.  The sub-region offsets are
C adjusted to allow positioning of the source picture with respect to
C its origin, in the absence of the keys LEFT, RIGHT, TOP, BOTTOM, NEAR
C and FAR.
C
C Any part of the source picture which lies outside the output
C picture is ignored.
C
      INTEGER IVAL
      LOGICAL SEMLU,OPT,VARSET,SEMROW,TSTSRG,MRKREG
C
      INCLUDE 'COMMON'
C
      INTEGER*4 N4COLS
      INTEGER FIRST(3),IB1(256),IB2(256),I1,I2,INFORM,IOFF,IPOS
      INTEGER J,J1,J2,J3,JOFF,K,K1,K2,K3,KOFF,LAST(3),LAYER,LAYER2
      LOGICAL DISREG
C
      EQUIVALENCE (IB1,RB1),(IB2,RB2)
      EQUIVALENCE (DISREG,SMGL2),(FIRST,SMGI1),(LAST,SMGI4)
C
C Packed names
C
      INTEGER NSIZE,NSI2,NSI3,NPOSIT,NPO2,NPO3,NLAYER,NLA2
      INTEGER NLEFT,NRIGHT,NTOP,NBOTTO,NNEAR,NFAR
      PARAMETER (NSIZE=30786, NSI2=30792, NSI3=30793)
      PARAMETER (NPOSIT=26219, NPO2=26232, NPO3=26233)
      PARAMETER (NLAYER=19265, NLA2=19272, NLEFT=19406, NRIGHT=29167)
      PARAMETER (NTOP=-617, NBOTTO=3820, NNEAR=22601, NFAR=9658)
C
C Set keys SIZE,SI2,SI3 to source picture size
C
      IF (SEMLU(2,NSIZE,REAL(NCOLS(LP1)))) GOTO 30
      IF (SEMLU(2,NSI2,REAL(NROWS(LP1)))) GOTO 30
      IF (SEMLU(2,NSI3,REAL(NLAYS(LP1)))) GOTO 30
C
C Adjust X offset if options LEFT or RIGHT not set
C
      IF (.NOT.(OPT(NLEFT).OR.OPT(NRIGHT))) THEN
         IPOS=IVAL(NPOSIT)-CCOLN(LP1)+(1+NCOLS(LP1)/2)
         IF (SEMLU(2,NPOSIT,REAL(IPOS))) GOTO 30
      ENDIF
C
C Adjust Y offset if options TOP or BOTTOM not set
C
      IF (.NOT.(OPT(NTOP).OR.OPT(NBOTTO))) THEN
         IPOS=IVAL(NPO2)+CROWN(LP1)-(1+NROWS(LP1)/2)
         IF (SEMLU(2,NPO2,REAL(IPOS))) GOTO 30
      ENDIF
C
C Adjust Z offset if options NEAR or FAR not set
C
      IF (.NOT.(OPT(NNEAR).OR.OPT(NFAR))) THEN
C
C See if LAYER key is set
C
         IF (VARSET(NLAYER)) THEN
C
C Fetch key value and then locally unset it
C
            LAYER=IVAL(NLAYER)
            IF (SEMLU(3,NLAYER,0.0)) GOTO 30
            IF (SEMLU(0,NLAYER,0.0)) GOTO 30
C
C See if LA2 key is set (default value equal to LAYER key)
C
            IF (VARSET(NLA2)) THEN
C
C Fetch key value and locally unset it
C
               LAYER2=IVAL(NLA2)
               IF (SEMLU(3,NLA2,0.0)) GOTO 30
               IF (SEMLU(0,NLA2,0.0)) GOTO 30
            ELSE
               LAYER2=LAYER
            ENDIF
C
C Fault range of layers not equal to source picture layer dimension
C
            IF (LAYER2-LAYER+1.NE.NLAYS(LP1)) THEN
               ERROR=3
               IDERR=NLAYER
               GOTO 30
            ENDIF
C
C Determine required offset
C
            IPOS=LAYER+NLAYS(LP1)/2-CLAYN(LP2)
         ELSE
            IPOS=IVAL(NPO3)-CLAYN(LP1)+(1+NLAYS(LP1)/2)
         ENDIF
         IF (SEMLU(2,NPO3,REAL(IPOS))) GOTO 30
      ENDIF
C
C Determine limits of source picture w.r.t. output picture
C
      IF (TSTSRG(2,LP2)) GOTO 30
C
C Finish if source picture does not overlap output picture
C
      IF (DISREG) GOTO 30
C
C Draw extraction frame
C
      IF (MRKREG(0)) GOTO 30
C
C Determine offset between source and output pictures
C
      IOFF=1-FIRST(1)
      JOFF=1-FIRST(2)
      KOFF=1-FIRST(3)
C
C Truncate source picture limits to fit output picture
C
      FIRST(1)=MAX(1,FIRST(1))
      FIRST(2)=MAX(1,FIRST(2))
      FIRST(3)=MAX(1,FIRST(3))
      LAST(1)=MIN(LAST(1),NCOLS(LP2))
      LAST(2)=MIN(LAST(2),NROWS(LP2))
      LAST(3)=MIN(LAST(3),NLAYS(LP2))
C
      I1=FIRST(1)
      I2=LAST(1)
C
C Reverse direction of processing if source and output would
C interfere, given in-situ processing
C
      IF (JOFF.GT.0) THEN
         J1=LAST(2)
         J2=FIRST(2)
         J3=-1
      ELSE
         J1=FIRST(2)
         J2=LAST(2)
         J3=1
      ENDIF
C
      IF (KOFF.GT.0) THEN
         K1=LAST(3)
         K2=FIRST(3)
         K3=-1
      ELSE
         K1=FIRST(3)
         K2=LAST(3)
         K3=1
      ENDIF
C
C Adjust limits and offset if output picture form is complex
C
      IF (FORMN(LP2).EQ.NFMCOM) THEN
         INFORM=NFMCOM
         I1=2*I1-1
         I2=2*I2
         IOFF=2*IOFF
      ELSE IF (FORMN(LP2).EQ.NFMFP) THEN
         INFORM=NFMFP
      ELSE
         INFORM=NFMINT
      ENDIF
C
C Insert source picture into output picture
C
      DO 20 K=K1,K2,K3
C
         DO 10 J=J1,J2,J3
C
C Fetch source and output picture rows
C
            IF (SEMROW(1,RB1,INFORM,J+JOFF,K+KOFF,LP1)) GOTO 30
            IF (SEMROW(1,RB2,INFORM,J,K,LP2)) GOTO 30
C
C Copy source data into output row
C
            N4COLS = I2 - I1 + 1
            IF (INFORM.EQ.NFMINT) THEN
               CALL CFORM(IB1(IOFF+I1),IB2(I1),NFMINT,NFMINT,N4COLS)
            ELSE
               CALL CFORM(RB1(IOFF+I1),RB2(I1),NFMFP,NFMFP,N4COLS)
            ENDIF
C
C Store result in LP2
C
            IF (SEMROW(2,RB2,INFORM,J,K,LP2)) GOTO 30
   10    CONTINUE
   20 CONTINUE
C
   30 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
