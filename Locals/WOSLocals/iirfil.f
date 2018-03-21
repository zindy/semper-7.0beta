C Semper 6 processing module IIRFIL
C
      SUBROUTINE IIRFIL
C
C IIR filter routine (small block convolution), with arbitrary kernel
C - at present, is simply stripped down FIR
C
C Provides verb IIR, taking kernel from (two layers of) pic WITH
C
C VD required:
C  IIR :IIRFIL with= >$ft
C
      LOGICAL SEMOPN,SEMROW,OPT,IIRF2
      LOGICAL FIFO
      REAL K(25),CLAP(9)
      INTEGER NREAL,NWITH
      INTEGER WITH,FFSIZE,NCOL,NROW,NLAY,KBY,IVALPN,NKP,I,J,L,N,KBB
      INTEGER JK,KP,JIN,KSIZE
      INTEGER*4 BPS(6),PO,P1,P2,P3,P4,P5,P6,P,LASTO
      INTEGER*4 INP,I1,I2,I3,I4,I5,IO,N4
      INTEGER*4 BP1,BP2,BP3,BP4,BP5,BP6
      REAL T,X,VAL
C     Change, maybe in fact a bug
      LOGICAL LADD
C
C Packed names
C
      PARAMETER (NREAL=29001,NWITH=-5181)
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
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Establish kernel
C ----------------
C Load kernel into RB5 and dims into KSIZE,KBY
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
C Store some useful values
C
      KBB=(KBY-1)/2
      FFSIZE=KBB+1
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
C Code for general kernel sizes
C -----------------------------
C Brute force is used, with one buffer accumulating the result while
C source rows are passed through a second KBY times
C
      DO 130 L=1,NLAY
      DO 130 J=1,NROW
C
C Construct output row by reading required source rows in turn
C and calling IIRN1 to accumulate the filter output
C
      JIN=J-KBB
      DO 120 JK=1,KBY
         IF (IIRF2(1,P2,NCOL,KSIZE,JIN,L,FIFO,FFSIZE,P6)) GOTO 250
         KP=(KBY-JK)*KSIZE+1
         LADD=JK.NE.1
         CALL IIRN1(RB1LHS,P1,P2,NCOL,RB5(KP),KSIZE,LADD)
         JIN=JIN+1
  120 CONTINUE
C
C Output row
      IF (IIRF2(2,P1,NCOL,KSIZE,J,L,FIFO,FFSIZE,P6)) GOTO 250
  130 CONTINUE
C
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
C Semper 6 subsidiary module IIRF2
C
C Read/writes rows for FIRFIL with pre/post processing;
C - reads propagate edge pixels outwards
C - writes go via FIFO buffer if nec
C P,PW are buffer pointers rel to RB1LHS
C
      LOGICAL FUNCTION IIRF2(OPC,P,NCOL,KSIZE,ROW,LAYER,FIFO,FFSIZE,PW)
C
      LOGICAL SEMROW,FIFO
      INTEGER OPC,FFSIZE,ROW,NROW,NCOL,N,JT,LAYER,KSIZE
      INTEGER*4 P,PW,IO,I1
C
      INCLUDE 'COMMON'
C
      IIRF2=.TRUE.
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Read
      IF (OPC.EQ.1) THEN
         N=MIN0(NROW,MAX0(1,ROW))
         IF (SEMROW(1,RB1LHS(P),NFMFP,N,LAYER,LP1)) RETURN
         CALL FIRPEP(RB1LHS,P,NCOL,KSIZE)
         GOTO 40
      ENDIF
C
C Write
C
C Direct output
      IF (.NOT.FIFO) THEN
         IF (SEMROW(2,RB1LHS(P),NFMFP,ROW,LAYER,LP2)) RETURN
C
C FIFO buffered output
      ELSE
         IF (SEMROW(2,RB1LHS(P),NFMFP,MOD(ROW,FFSIZE)+1,1,LP3)) RETURN
         JT=ROW-FFSIZE+1
         IF (JT.LE.0) GOTO 40
   30    IF (SEMROW(1,RB1LHS(P),NFMFP,MOD(JT,FFSIZE)+1,1,LP3)) RETURN
         IF (SEMROW(2,RB1LHS(P),NFMFP,JT,LAYER,LP2)) RETURN
C
C On last row, ensure buffer flush complete..
         IF (ROW.LT.NROW) GOTO 40
         JT=JT+1
         IF (JT.LE.NROW) GOTO 30
      ENDIF
C
C Normal return
C
   40 IIRF2=.FALSE.
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module IIRN1
C
      SUBROUTINE IIRN1(B,PO,P,L,K,KSIZE,ADD)
C
C Applies N by 1 filter defined by K(1:KSIZE) to P, 
C placing result in PO; result added to P if ADD;
C PO,P are buffer pointers wrt B
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







