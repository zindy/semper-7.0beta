C Semper 6 processing module RAMPS
C
      SUBROUTINE RAMPS
C
C Fills sub-region defined with respect to current graphics coordinates
C with series of grey-scale ramps.  Graphics coordinates are chosen
C according to the options FRAME, PARTITION and PICTURE (default).
C Sub-region is specified using 2-D sub-region keys and options in
C relation to border limits set up for the graphics coordinates.  The
C key TIMES specifies the number of ramps to generate.  If option FULL
C is set, a ramp made up of unit intensity steps is generated.  If
C option VIEW is set, the view conditions are switched to centre the
C view on the graphics border (frame, partition or picture limits).
C
      LOGICAL OPT,ABANDN,FSOPTN,FSINIT,FSREGN,FSVIEW,FSELEC,FSRO61
      INTEGER IVAL,OPC
C
      INCLUDE 'COMMON'
C
      REAL DGREY,GREY,XMIN,XMAX,YMIN,YMAX,BLACK,WHITE
      INTEGER I,I1,I2,J,J1,J2,K,N,FORM
      INTEGER*4 I4N
C
      INTEGER IB1(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1)
C
C Packed names
C
      INTEGER NVIEW,NFULL,NTIMES
      PARAMETER (NVIEW=-3566, NFULL=10452, NTIMES=-374)
C
C Determine nature of graphics coordinates according to options FRAME,
C PARTITION and PICTURE
C
      IF (FSOPTN(OPC,N)) GOTO 50
C
C Set up specified graphics coordinates
C
      IF (FSINIT(OPC,N)) GOTO 50
C
C If option VIEW is set, switch view to area of interest
C
      IF (OPT(NVIEW)) THEN
         IF (FSVIEW()) GOTO 50
      ENDIF
C
C Determine size and limits of sub-region specified with 2-D sub-region
C keys and options
C
      IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 50
C
C Determine position in display coordinates of corners of sub-region
C
      I1=NINT(FSXSCA*XMIN+FSXOFF)
      I2=NINT(FSXSCA*XMAX+FSXOFF)
      J1=NINT(FSYSCA*YMAX+FSYOFF)
      J2=NINT(FSYSCA*YMIN+FSYOFF)
C
C Determine maximum width of ramp (limited by row buffer size)
C
      N=MIN(I2-I1+1,LNBUF/LNINT)
C
C Determine ramp increment (check for N = 1 prevents zero divide)
C
      IF (OPT(NFULL)) THEN
         DGREY=1.0
      ELSE
         IF (N.EQ.1) THEN
            DGREY=0.0
         ELSE
            DGREY=(REAL(LUTLEN)*REAL(MAX(IVAL(NTIMES),1))-1.0)/REAL(N-1)
         ENDIF
      ENDIF
C
C Generate ramp values
C
      GREY=0.0
      DO 10 I=1,N
         IB1(I)=NINT(GREY)
         IF (IB1(I).EQ.LUTLEN) IB1(I)=0
C
         GREY=MOD(GREY+DGREY,REAL(LUTLEN))
   10 CONTINUE
C
C Choose output form - if LUTLEN is sufficiently small use byte
C
      IF (LUTLEN .LE. 256) THEN
         FORM = NFMBYT
         I4N = N
         CALL CFORM(IB1,IB1,NFMINT,NFMBYT,I4N)
      ELSE
         FORM = NFMINT
      ENDIF
C
C Set up output levels for black and white
C
      BLACK=0.0
      WHITE=REAL(LUTLEN-1)
C
C Fill sub-region once or twice as required
C Note: FSI1,FSI2 = 1,1 = option RE
C                 = 2,2 = option IM
C                 = 1,2 = default
C
      DO 40 I=FSI1,FSI2
C
C Add offset for imaginary part if required
C
         IF (I.EQ.2) I1=NINT(FSXSCA*(XMIN+FSIOFF)+FSXOFF)
C
C Fill all rows of sub-region with ramps
C
         DO 30 J=J1,J2
C
C Fill all layers of each row with ramps
C
            DO 20 K=FSFRA,FSFRA2
C
C Output contents of row buffer to display
C
               IF (FSRO61(IB1,1,N,1,FORM,I1,J,K,BLACK,WHITE,0,ERROR))
     +            GOTO 50
C
C Set flag to flush graphics buffer at the end of the current command
C
               REQFSF=.TRUE.
C
C Check for request to abandon this command
C
               IF (ABANDN(ERROR)) GOTO 50
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
C
C Update current frame/partition/picture number
C
      IF (FSELEC()) GOTO 50
C
   50 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
