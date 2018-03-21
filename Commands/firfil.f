C Semper 6 processing module FIRFIL
C
      SUBROUTINE FIRFIL
C
C FIR filter routine (small block convolution), with arbitrary
C kernel size, special code for 1-D or square 3 or 5 point kernels,
C and for product-separable square kernels.
C
C Provides verb FIR, with options LAPLACEAN and GAUSSIAN
C invoking preset kernels; otherwise kernel taken from picture WITH
C
C ADD causes filter o/p to be added to source;
C SUBTRACT causes filter o/p to be subtracted from source;
C MODULUS causes final result to be replaced by its modulus
C
C Preset kernels are as follows:
C Laplacean: .25  .5  .25   1-D form:  1  -2   1
C             .5  -3   .5
C            .25  .5  .25
C
C Gaussian:  exp(-x.x/2.r.r) for x from -2*r to 2*r, separable
C
C VD required:
C  FIR :FIRFIL laplacean gaussian separable add subtract modulus +
C    with= radius=1  >$ft
C
      LOGICAL SEMOPN,SEMROW,OPT,FIRF2
      LOGICAL LMOD,LSEP,FIFO,PPADD,PPSUB,LADD
      REAL K(25),CLAP(9)
      INTEGER NSEPAR,NREAL,NWITH,NADD,NSUBTR,NMODUL,NLAPLA,NGAUSS,NRADIU
      INTEGER WITH,FFSIZE,NCOL,NROW,NLAY,KBY,IVALPN,NKP,I,J,L,N,KBB
      INTEGER JK,KP,JIN,KSIZE
      INTEGER*4 BPS(6),PO,P1,P2,P3,P4,P5,P6,P,LASTO
      INTEGER*4 INP,I1,I2,I3,I4,I5,IO,N4
      INTEGER*4 BP1,BP2,BP3,BP4,BP5,BP6
      REAL T,X,VAL
C
C Packed names
C
      PARAMETER (NSEPAR=30616,NREAL=29001,NWITH=-5181)
      PARAMETER (NADD=1764,NSUBTR=31242,NMODUL=21404)
      PARAMETER (NLAPLA=19256,NGAUSS=11261,NRADIU=28844)
C
      INCLUDE 'COMMON'
C
      PARAMETER (BP1=LNEDGE+1)
      PARAMETER (BP2=BP1+LNBUF/LNREAL+LNEDGE*2)
      PARAMETER (BP3=BP2+LNBUF/LNREAL+LNEDGE*2)
      PARAMETER (BP4=BP3+LNBUF/LNREAL+LNEDGE*2)
      PARAMETER (BP5=BP4+LNBUF/LNREAL+LNEDGE*2)
      PARAMETER (BP6=BP5+LNBUF/LNREAL+LNEDGE*2)
C
      EQUIVALENCE (BPS(1),P1),(BPS(2),P2),(BPS(3),P3)
      EQUIVALENCE (BPS(4),P4),(BPS(5),P5),(BPS(6),P6)
      EQUIVALENCE (SMGL1,LMOD),(SMGL2,PPADD),(SMGL3,PPSUB)
C
C Coefficients defining 2-D Laplacean operator
C
      DATA CLAP/.25,.5,.25,.5,-3.,.5,.25,.5,.25/
C
C Initialise buffer pointers
C
      BPS(1)=BP1
      BPS(2)=BP2
      BPS(3)=BP3
      BPS(4)=BP4
      BPS(5)=BP5
      BPS(6)=BP6
C
C Initialise options
C
      LMOD=OPT(NMODUL)
      LSEP=OPT(NSEPAR)
      PPADD=OPT(NADD)
      PPSUB=OPT(NSUBTR)
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Establish kernel
C ----------------
C Load kernel into RB5 and dims into KSIZE,KBY; options
C ADD, SUB, SEP may be forced at this stage
C
C Laplacean?
C
      IF (OPT(NLAPLA)) THEN
         KSIZE=3
         IF (NROW.EQ.1) THEN
            KBY=1
            RB5(1)=1.0
            RB5(2)=-2.0
            RB5(3)=1.0
         ELSE
            KBY=3
            DO 10 I=1,9
               RB5(I)=CLAP(I)
   10       CONTINUE
         ENDIF
         LSEP=.FALSE.
         GOTO 90
      ENDIF
C
C Gaussian?
C
      IF (OPT(NGAUSS)) THEN
         T=VAL(NRADIU)
         IF (T.LE.0.) GOTO 260
         KSIZE=2.*T
         IF (KSIZE.EQ.0) KSIZE=1
         X=-KSIZE
         KSIZE=2*KSIZE+1
         IF (KSIZE.GT.2*LNEDGE+1) GOTO 260
         DO 20 I=1,KSIZE
            RB5(I)=EXP(-X*X/2./T/T)
            X=X+1.
   20    CONTINUE
C
C Normalise kernel
C
         LSEP=NROW.NE.1
         T=0.
         IF (LSEP) THEN
            DO 40 I=1,KSIZE
               DO 30 J=1,KSIZE
                  T=T+RB5(I)*RB5(J)
   30          CONTINUE
   40       CONTINUE
            T=SQRT(T)
         ELSE
            DO 50 I=1,KSIZE
               T=T+RB5(I)
   50       CONTINUE
         ENDIF
C
         DO 60 I=1,KSIZE
            RB5(I)=RB5(I)/T
   60    CONTINUE
         KBY=1
         GOTO 90
      ENDIF
C
C Kernel comes from WITH: open it..
C
      WITH=IVALPN(NWITH)
      IF (WITH.LE.0) THEN
         ERROR=3
         IDERR=NWITH
         GOTO 250
      ENDIF
      IF (SEMOPN(1,WITH,KSIZE,KBY,N,I,J,LP3)) GOTO 250
      IDERR=WITH
      NKP=KSIZE*KBY
      IF (NKP.LE.1) GOTO 260
      IF (KSIZE.GT.2*LNEDGE+1) GOTO 260
      IF (NKP.GT.LNBUF/LNREAL) GOTO 260
C
C .. and read data
C
      N=1
      T=0.
      DO 80 J=1,KBY
         IF (SEMROW(1,RB1,NFMFP,J,1,LP3)) GOTO 250
         DO 70 I=1,KSIZE
            RB5(N)=RB1(I)
            T=T+RB5(N)*RB5(N)
            N=N+1
   70    CONTINUE
   80 CONTINUE
C
C All coefficients zero?
C
      IF (T.EQ.0.) GOTO 270
C
C Set final KBY if separable
C
   90 IF (LSEP) THEN
         IF (KBY.NE.1) GOTO 260
         KBY=KSIZE
      ENDIF
C
C Store some useful values
C
      NKP=KSIZE*KBY
      KBB=(KBY-1)/2
      FFSIZE=KBB+1
C
C Adjust kernel to accomodate options ADD, SUBTRACT (unless SEPARABLE,
C in which case the options are accommodated via direct post-processing
C
      IF (LSEP) GOTO 110
      I=KBY/2*KSIZE+KSIZE/2+1
      IF (PPADD) THEN
         PPADD=.FALSE.
         RB5(I)=RB5(I)+1.
      ENDIF
      IF (PPSUB) THEN
         PPSUB=.FALSE.
         RB5(I)=RB5(I)-1.
         DO 100 I=1,NKP
            RB5(I)=-RB5(I)
  100    CONTINUE
      ENDIF
  110 CONTINUE
C
C If processing in situ, open temp picture for use as a FIFO output
C buffer avoiding the source being overwritten prematurely
C
      FIFO=.FALSE.
      IF (LP1.EQ.LP2.AND.KBY.NE.1) THEN
         FIFO=.TRUE.
         LP3=0
         IF (SEMOPN(3,0,NCOL,FFSIZE,1,NCLIMA,NFMFP,LP3)) GOTO 250
      ENDIF
C
C Switch to special code for square or 1-D 3 or 5 point kernels
C
      IF (NKP.EQ.3 .OR. NKP.EQ.5) GOTO 140
C
      IF (KBY.NE.1) THEN
         IF (NKP.EQ.9 .OR. NKP.EQ.25) GOTO 140
      ENDIF
C
C Code for general kernel sizes
C -----------------------------
C Brute force is used, with one buffer accumulating the result while
C source rows are passed through a second KBY times
C
C Disguise separable filter initially as vertical
C
      IF (LSEP) KSIZE=1
      DO 130 L=1,NLAY
      DO 130 J=1,NROW
C
C Construct output row by reading required source rows in turn
C and calling FIRN1 to accumulate the filter output
C
      JIN=J-KBB
      DO 120 JK=1,KBY
         IF (FIRF2(1,P2,NCOL,KSIZE,JIN,L,FIFO,FFSIZE,P6)) GOTO 250
         KP=(KBY-JK)*KSIZE+1
         LADD=JK.NE.1
         CALL FIRN1(RB1LHS,P1,P2,NCOL,RB5(KP),KSIZE,LADD)
         JIN=JIN+1
  120 CONTINUE
C
C Output row, applying horizontal filter first if separable
C
      IF (LSEP) THEN
         CALL FIRPEP(RB1LHS,P1,NCOL,KBY)
         CALL FIRN1(RB1LHS,P2,P1,NCOL,RB5,KBY,.FALSE.)
         IF (FIRF2(2,P2,NCOL,KSIZE,J,L,FIFO,FFSIZE,P6)) GOTO 250
      ELSE
         IF (FIRF2(2,P1,NCOL,KSIZE,J,L,FIFO,FFSIZE,P6)) GOTO 250
      ENDIF
  130 CONTINUE
      GOTO 250
C
C Code for special kernel sizes
C -----------------------------
C Row buffers are cycled so as to achieve the filter output with a
C single serial pass through the source
C
C Copy kernel to local array K
C
  140 DO 150 I=1,NKP
         K(I)=RB5(I)
  150 CONTINUE
C
C Loop over layers
C
      DO 240 L=1,NLAY
C
C Start above top to ramp up
C
      J=2-KBY
C
C Read next row to 'last' row buffer and switch code
C
  160 PO=P6
      P=BPS(KBY)
      IF (FIRF2(1,P,NCOL,KSIZE,J+KBB,L,FIFO,FFSIZE,P6)) GOTO 250
      IF (J.LE.0) GOTO 230
      IF (KSIZE.EQ.1) GOTO 200
      IF (LSEP) GOTO 200
      IF (KBY.EQ.1) GOTO 170
C
C 2-D filter
C
      IF (KSIZE.EQ.3) THEN
         CALL FIR33(RB1LHS,PO,P1,P2,P3,NCOL,K)
      ELSE
         CALL FIR55(RB1LHS,PO,P1,P2,P3,P4,P5,NCOL,K)
         ENDIF
      GOTO 220
C
C Horizontal filter
C
  170 INP=P1
  180 LASTO=PO+NCOL-1
      I1=INP
      DO 190 IO=PO,LASTO
      IF (KSIZE.EQ.3) THEN
         RB1LHS(IO)=K(3)*RB1LHS(I1-1)+K(2)*RB1LHS(I1)+K(1)*RB1LHS(I1+1)
      ELSE
         RB1LHS(IO)=K(5)*RB1LHS(I1-2)+K(4)*RB1LHS(I1-1)+K(3)*RB1LHS(I1)
     +             +K(2)*RB1LHS(I1+1)+K(1)*RB1LHS(I1+2)
         ENDIF
  190 I1=I1+1
      GOTO 220
C
C Vertical filter
C
  200 I1=P1
      I2=P2
      I3=P3
      I4=P4
      I5=P5
      LASTO=PO+NCOL-1
      DO 210 IO=PO,LASTO
      IF (KBY.EQ.3) THEN
         RB1LHS(IO)=K(3)*RB1LHS(I1)+K(2)*RB1LHS(I2)+K(1)*RB1LHS(I3)
      ELSE
         RB1LHS(IO)=K(5)*RB1LHS(I1)+K(4)*RB1LHS(I2)+K(3)*RB1LHS(I3)
     +             +K(2)*RB1LHS(I4)+K(1)*RB1LHS(I5)
         I4=I4+1
         I5=I5+1
      ENDIF
      I1=I1+1
      I2=I2+1
  210 I3=I3+1
      IF (LSEP) THEN
         INP=PO
         PO=P1
         CALL FIRPEP(RB1LHS,INP,NCOL,KSIZE)
         GOTO 180
      ENDIF
C
C Output resulting row, offering P1 for post-processing workspace
C
  220 IF (FIRF2(2,PO,NCOL,KSIZE,J,L,FIFO,FFSIZE,P1)) GOTO 250
C
C Cycle buffer pointers and proceed to next row
C
  230 N4=P1
      P1=P2
      P2=P3
      P3=P4
      P4=P5
      P5=N4
      J=J+1
      IF (J.LE.NROW) GOTO 160
C
C End of loop over layers
C
  240 CONTINUE
  250 RETURN
C
C Errors
C
  260 ERROR=66
      GOTO 250
  270 ERROR=65
      GOTO 250
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module FIRF2
C
C Read/writes rows for FIRFIL with pre/post processing;
C - on reads, propagates edge pixels outwards
C - on writes, include modulus/add/subtract processing and uses
C   FIFO buffer if nec
C P,PW are buffer pointers rel to RB1LHS
C
      LOGICAL FUNCTION FIRF2(OPC,P,NCOL,KSIZE,ROW,LAYER,FIFO,FFSIZE,PW)
C
      LOGICAL SEMROW,LMOD,FIFO,PPADD,PPSUB
      INTEGER OPC,FFSIZE,ROW,NROW,NCOL,N,JT,LAYER,KSIZE
      INTEGER*4 P,PW,IO,I1,LAST
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (SMGL1,LMOD),(SMGL2,PPADD),(SMGL3,PPSUB)
C
      FIRF2=.TRUE.
      NCOL=NCOLS(LP1)
      LAST=P+NCOL-1
      NROW=NROWS(LP1)
C
C Read
C
      IF (OPC.EQ.1) THEN
         N=MIN0(NROW,MAX0(1,ROW))
         IF (SEMROW(1,RB1LHS(P),NFMFP,N,LAYER,LP1)) RETURN
         CALL FIRPEP(RB1LHS,P,NCOL,KSIZE)
         GOTO 40
         ENDIF
C
C Write: perform any post-processing for ADD/SUB
C
      IF (PPADD.OR.PPSUB) THEN
         IF (SEMROW(1,RB1LHS(PW),NFMFP,ROW,LAYER,LP1)) RETURN
         I1=PW
         DO 10 IO=P,LAST
         IF (PPSUB) THEN
            RB1LHS(IO)=RB1LHS(I1)-RB1LHS(IO)
         ELSE
            RB1LHS(IO)=RB1LHS(I1)+RB1LHS(IO)
            ENDIF
   10    I1=I1+1
         ENDIF
C
C Take modulus of output if requested
C
      IF (LMOD) THEN
         DO 20 IO=P,LAST
   20    RB1LHS(IO)=ABS(RB1LHS(IO))
         ENDIF
C
C Direct output
C
      IF (.NOT.FIFO) THEN
         IF (SEMROW(2,RB1LHS(P),NFMFP,ROW,LAYER,LP2)) RETURN
C
C FIFO buffered output
C
      ELSE
         IF (SEMROW(2,RB1LHS(P),NFMFP,MOD(ROW,FFSIZE)+1,1,LP3)) RETURN
         JT=ROW-FFSIZE+1
         IF (JT.LE.0) GOTO 40
   30    IF (SEMROW(1,RB1LHS(P),NFMFP,MOD(JT,FFSIZE)+1,1,LP3)) RETURN
         IF (SEMROW(2,RB1LHS(P),NFMFP,JT,LAYER,LP2)) RETURN
C
C On last row, ensure buffer flush complete..
C
         IF (ROW.LT.NROW) GOTO 40
         JT=JT+1
         IF (JT.LE.NROW) GOTO 30
         ENDIF
C
C Normal return
C
   40 FIRF2=.FALSE.
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary routine FIRPEP
C
      SUBROUTINE FIRPEP(B,P,NCOL,KSIZE)
C
C Propagates edge pixels outwards from buffer at P wrt B, so as to
C accommodate a kernel of KSIZE points
C
      REAL B(*),T
      INTEGER I,NCOL,KSIZE
      INTEGER*4 P,LAST
C
      T=B(P)
      DO 10 I=1,KSIZE/2
   10 B(P-I)=T
      LAST=P+NCOL-1
      T=B(LAST)
      DO 20 I=1,(KSIZE-1)/2
   20 B(LAST+I)=T
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module FIRN1
C
      SUBROUTINE FIRN1(B,PO,P,L,K,KSIZE,ADD)
C
C Applies N by 1 filter defined by K to P, placing result in PO;
C result added to P if ADD; PO,P are buffer pointers wrt B
C
      LOGICAL ADD
      INTEGER J,L,KSIZE,KSB2
      INTEGER*4 PO,P,LASTO,IO,N4
      REAL B(*),K(KSIZE),T
C
      KSB2=KSIZE/2
      LASTO=PO+L-1
      DO 20 IO=PO,LASTO
      N4=P+IO-PO+KSB2
      T=B(IO)
      IF (.NOT.ADD) T=0.
      DO 10 J=1,KSIZE
      T=T+K(J)*B(N4)
   10 N4=N4-1
   20 B(IO)=T
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
