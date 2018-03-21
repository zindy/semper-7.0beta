C Semper 6 subsidiary module LMANU3
C
      SUBROUTINE LMANU3(LUT,L1,L2,LUTINT)
C
C Add highlighting band of intensity LUTINT to LUT channel.
C LUT channel is first scaled down by 3/4 and then the band from
C L1 to L2 is set to the value LUTINT.
C
      INTEGER LUT(*)
      INTEGER L1,L2,LUTINT
C
      INCLUDE 'COMMON'
C
      INTEGER L
C
C Set first part of LUT channel to 3/4 of original
C
      DO 10 L=1,L1-1
         LUT(L)=NINT(REAL(LUT(L)*3)/4.0)
   10 CONTINUE
C
C Set middle part to value LUTINT
C
      DO 20 L=L1,L2
         LUT(L)=LUTINT
   20 CONTINUE
C
C Set last part to 3/4 of original
C
      DO 30 L=L2+1,LUTLEN
         LUT(L)=NINT(REAL(LUT(L)*3)/4.0)
   30 CONTINUE
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LMANU4
C
      SUBROUTINE LMANU4(LUT,B,C,LR1,LR2,LH1,LH2,LHIGHL)
C
C Determines make-up of LUT channel in LUT.  Locates transition region
C by stepping inwards from both ends until LUT values are different from
C end values.  The transition region is assumed to consist of a linear
C grey-scale and for this the relative brightness B and contrast C are
C calculated.  The limits of the transition region (range) are returned
C in LR1 and LR2.  If a highlighting band can be distinguished, LH1 and
C LH2 return its extent and the flag LHIGHL is set to report the
C presence of highlighting.  The end values are also scaled up by 4/3,
C to allow for the factoring by 3/4 when highlighting is added.
C
      INTEGER LUT(*)
      REAL B,C
      INTEGER LR1,LR2,LH1,LH2
      LOGICAL LHIGHL
C
      INCLUDE 'COMMON'
C
      INTEGER L,L1,L2,LUT1,LUT2
C
C Find end of left-hand constant region to determine value of LR1
C
      LUT1=LUT(1)
      DO 10 L=1,LUTLEN
         IF (LUT(L).NE.LUT1) GOTO 20
         LR1=L
   10 CONTINUE
C
C Find end of right-hand constant region to determine value of LR2
C
   20 LUT2=LUT(LUTLEN)
      DO 30 L=LUTLEN,1,-1
         IF (LUT(L).NE.LUT2) GOTO 40
         LR2=L
   30 CONTINUE
C
C See if LUT is constant (LR1 and LR2 reversed)
C
   40 IF (LR1.GT.LR2) THEN
C
C Set return values accordingly
C
         LR1=1
         LR2=LUTLEN
         LH1=LUTLEN/3
         LH2=LUTLEN*2/3
         LHIGHL=.FALSE.
C
C Otherwise, see if highlighting band can be distinguished
C
      ELSE
C
C Determine region within which highlighting band might be found
C
         IF (LUT1.EQ.LUTMAX) THEN
            L1=LR1+2
         ELSE
            L1=1
         ENDIF
C
         IF (LUT2.EQ.LUTMAX) THEN
            L2=LR2-2
         ELSE
            L2=LUTLEN
         ENDIF
C
C Find left-hand end of highlighting band
C
         LH1=0
         DO 50 L=L1,L2
            IF (LUT(L).EQ.LUTMAX) THEN
               LH1=L
               GOTO 60
            ENDIF
   50    CONTINUE
C
C Find right-hand end of highlighting band
C
   60    LH2=0
         DO 70 L=L2,L1,-1
            IF (LUT(L).EQ.LUTMAX) THEN
               LH2=L
               GOTO 80
            ENDIF
   70    CONTINUE
C
C See if highlighting band detected
C
   80    LHIGHL=LH1.NE.0.AND.LH2.NE.0
C
C If highlighting band found, factor end LUT values by 4/3
C
         IF (LHIGHL) THEN
            LUT1=MIN(NINT(REAL(LUT1*4)/3.0),LUTMAX)
            LUT2=MIN(NINT(REAL(LUT2*4)/3.0),LUTMAX)
C
C Otherwise, set LH1 and LH2 to middle third of range LR1 to LR2
C
         ELSE
            LH1=LR1+(LR2-LR1)/3
            LH2=LR1+(LR2-LR1)*2/3
         ENDIF
      ENDIF
C
C Determine relative brightness B and contrast C
C
      B=REAL(LUT1+LUT2)/REAL(2*LUTMAX)
C
      IF (LUT1.EQ.LUT2) THEN
         C=0.0
      ELSE
         C=REAL(LUT2-LUT1)/REAL(LUTMAX-ABS(LUT1+LUT2-LUTMAX))
      ENDIF
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 sudsidiary module LMANU5
C
      SUBROUTINE LMANU5(STRING,ITEM)
C
C Simple routine to add string in ITEM to end of non-blank string in
C STRING, with two blanks to separate items.
C
      CHARACTER*(*) STRING,ITEM
C
      INTEGER I,N
C
C Find last non-blank in STRING
C
      DO 10 I=LEN(STRING),1,-1
         IF (STRING(I:I).NE.' ') THEN
            N=I
            GOTO 20
         ENDIF
   10 CONTINUE
C
      N=-2
C
C Add ITEM to STRING
C
   20 STRING(N+3:)=ITEM
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LMANUF
C
      SUBROUTINE LMANUF(LUT,L1,L2)
C
C Initialise LUT to false colour scale.  The colour scale has been
C chosen so that it is invertible and follows an exact grey-scale
C if the RED,GREEN,BLUE channels are assigned grey factors of
C LUTMAX*2/7,LUTMAX*4/7,LUTMAX/7 and are then summed.
C Note: The correct factors to convert from colour to grey-scale
C       are RED,GREEN,BLUE = 0.30,0.59,0.11 - which is not too far
C       different from the factors 2/7,4/7,1/7.
C
      INTEGER LUT(*)
      INTEGER L1,L2
C
      INCLUDE 'COMMON'
C
      REAL DL,X
      INTEGER J,L,LOFF,I
C
      INTEGER LUTINT(0:7,3)
      DATA LUTINT /0,0,1,1,0,0,1,1,
     +             0,0,0,0,1,1,1,1,
     +             0,1,0,1,0,1,0,1/
C
C Determine width of false colour band
C
      DL=REAL(L2-L1)
C
C Initialise each LUT channel in turn
C
      DO 40 J=1,3
C
C Determine offset into LUT for specified LUT channel
C
         LOFF=(J-1)*LUTLEN
C
C Set first part of LUT channel to zero
C
         DO 10 L=LOFF+1,LOFF+L1
            LUT(L)=0
   10    CONTINUE
C
C Set middle part to false colour scale
C
         DO 20 L=LOFF+L1+1,LOFF+L2-1
            X=7.0*REAL(L-(LOFF+L1))/DL
            I=INT(X)
            LUT(L)=NINT((REAL(LUTINT(I,J))+(X-REAL(I))*
     +                   REAL(LUTINT(I+1,J)-LUTINT(I,J)))*REAL(LUTMAX))
   20    CONTINUE
C
C Set last part to LUTMAX
C
         DO 30 L=LOFF+L2,LOFF+LUTLEN
            LUT(L)=LUTMAX
   30    CONTINUE
   40 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LMANUR
C
      LOGICAL FUNCTION LMANUR(NAME1,NAME2,L1,L2)
C
C Converts range specified by named keys NAME1 and NAME2 to nearest
C LUT positions.  If key SCALED is set, the range is specified with
C respect to the intensity range in the current DPD.
C
      LOGICAL VARSET
      REAL VAL
C
      INCLUDE 'COMMON'
C
      INTEGER NAME1,NAME2,L1,L2
      REAL XMIN,XMAX,X1,X2
C
C Packed names
C
      INTEGER NSCALE
      PARAMETER (NSCALE=30521)
C
      LMANUR=.TRUE.
C
C Determine the limits of the intensity range
C
      IF (VARSET(NSCALE)) THEN
         XMIN=DPMIN
         XMAX=DPMAX
      ELSE
         XMIN=0.0
         XMAX=REAL(LUTLEN-1)
      ENDIF
C
C Determine the range start value
C
      IF (VARSET(NAME1)) THEN
         X1=VAL(NAME1)
      ELSE
         X1=XMIN
      ENDIF
C
C Determine the range finish value
C
      IF (VARSET(NAME2)) THEN
         X2=VAL(NAME2)
      ELSE
         X2=XMAX
      ENDIF
C
C Fault incorrect range values
C
      IF (X1.LT.XMIN.OR.X2.LT.X1.OR.X2.GT.XMAX) THEN
         ERROR=3
         IDERR=NAME1
      ELSE
C
C Convert values to nearest LUT positions
C
         L1=1+NINT((X1-XMIN)*REAL(LUTLEN-1)/(XMAX-XMIN))
         L2=1+NINT((X2-XMIN)*REAL(LUTLEN-1)/(XMAX-XMIN))
C
         LMANUR=.FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
