C Semper 6 processing module RANK
C
      SUBROUTINE RANK
C
C Small block ranking module, for 3 or 5 point kernels, allowing
C erosion/dilation/median filtering according to output rank selected;
C special code for 1-D
C
C MACRO line required:
C Rank RANK(4) erode dilate over=3 position= >$C1
C
C The histogram initialisation loop does not in fact take a
C significant time on the SEL 32/27 at least, though it could be
C eliminated by ramping down as well as up..  there is a significant
C time saving in a 0/1 binary picture as opposed to a 0/255 picture
C because of reduced times shifting MDN/NBELOW up and down
C
      INTEGER IVAL
      LOGICAL SEMROW,SEMROWI,OPT
C
      INCLUDE 'COMMON'
C
      INTEGER*4 IBPS(5),N4
C
      INTEGER HIST(256),IB1LHS(10),IB6(256),I,J,K
      INTEGER KADD,KBY,KSIZE,KSUB,MDN,N,NBELOW,NCOL,NROW,OPRANK,ROW
      LOGICAL ONED
C
C Packed names
C
      INTEGER NOVER,NPOSIT,NERODE,NDILAT
      PARAMETER (NOVER=24885,NPOSIT=26219)
      PARAMETER (NERODE=8735,NDILAT=6772)
C
      INTEGER*4 IBP1,IBP2,IBP3,IBP4,IBP5
      PARAMETER (IBP1=LNEDGE*LNREAL/LNINT+1)
      PARAMETER (IBP2=IBP1+LNBUF/LNINT+LNEDGE*2*LNREAL/LNINT)
      PARAMETER (IBP3=IBP2+LNBUF/LNINT+LNEDGE*2*LNREAL/LNINT)
      PARAMETER (IBP4=IBP3+LNBUF/LNINT+LNEDGE*2*LNREAL/LNINT)
      PARAMETER (IBP5=IBP4+LNBUF/LNINT+LNEDGE*2*LNREAL/LNINT)
C
      EQUIVALENCE (RB1LHS,IB1LHS),(RB6,IB6)
C
C Initialise
C
      IBPS(1)=IBP1
      IBPS(2)=IBP2
      IBPS(3)=IBP3
      IBPS(4)=IBP4
      IBPS(5)=IBP5
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      ONED=NROW.EQ.1
C
C Establish kernel size
C
      KSIZE=IVAL(NOVER)
      IDERR=NOVER
      IF (KSIZE.NE.3 .AND. KSIZE.NE.5) GOTO 110
      IF (ONED) THEN
         KBY=1
      ELSE
         KBY=KSIZE
      ENDIF
C
C Establish output ranking required
C
      N=KSIZE*KBY
      OPRANK=IVAL(NPOSIT)
      IF (OPRANK.EQ.0) OPRANK=N/2+1
      IDERR=NPOSIT
      IF (OPRANK.LT.0.OR.OPRANK.GT.N) GOTO 110
C ERODE  => POSITION 1,1,2,4  for 3x1,5x1,3x3,5x5
C DILATE =>          3,5,8,21 for 3x1,5x1,3x3,5x5
      I=(N+2)/9
      IF (OPT(NERODE)) OPRANK=1+I
      IF (OPT(NDILAT)) OPRANK=N-I
C
C Begin loop over target (output) rows, counted by J
C
      J=1-KBY+1
C
C Next row to read is target plus 0/1/2 (trunc at top/bot)
C
   10 ROW=J+KBY/2
      IF (ROW.LT.1) THEN
         ROW=1
      ELSE IF (ROW.GT.NROW) THEN
         ROW=NROW
      ENDIF
C
C Read it into buffer KBY of cycled set
C
      N4=IBPS(KBY)
      IF (SEMROWI(1,IB1LHS(N4),1,ROW,1,LP1)) GOTO 100
C
C Propagate edge pixels
C
      N4=IBPS(KBY)
      IB1LHS(N4-1)=IB1LHS(N4)
      IB1LHS(N4-2)=IB1LHS(N4)
      N4=N4+NCOL
      IB1LHS(N4)=IB1LHS(N4-1)
      IB1LHS(N4+1)=IB1LHS(N4-1)
C
C Run histogram along rows, unless still ramping up
C
      IF (J.LE.0) GOTO 90
C
C Initialise histogram to zero (time does not in fact appear to be
C significant)
C
      DO 20 I=1,256
         HIST(I)=0
   20 CONTINUE
      MDN=0
      NBELOW=0
C
C Begin loop over target pixels within row, counted by I
C
      KADD=-KSIZE/2
      I=1-KSIZE+1
C
C Add new rhs column to histogram
C
   30 DO 40 K=1,KBY
         N4=IBPS(K)+KADD
         N=IB1LHS(N4)
         IF (N.LT.0 .OR. N.GT.255) THEN
            IDMESS='RANK requires integer data in the range 0 to 255'
            ERROR = 77
            GOTO 100
         ENDIF
         HIST(N+1)=HIST(N+1)+1
         IF (N.LT.MDN) NBELOW=NBELOW+1
   40 CONTINUE
C
C Subtract old lhs column from histogram
C
      IF (I.GT.1) THEN
         KSUB=KADD-KSIZE
         DO 50 K=1,KBY
            N4=IBPS(K)+KSUB
            N=IB1LHS(N4)
            HIST(N+1)=HIST(N+1)-1
            IF (N.LT.MDN) NBELOW=NBELOW-1
   50    CONTINUE
      ENDIF
C
C Adjust median and LT count, unless still ramping up
C
      IF (I.LE.0) GOTO 80
   60 IF (NBELOW.LT.OPRANK) THEN
         N=NBELOW+HIST(MDN+1)
         IF (N.GE.OPRANK) GOTO 70
         NBELOW=N
         MDN=MDN+1
      ELSE
         MDN=MDN-1
         NBELOW=NBELOW-HIST(MDN+1)
      ENDIF
      GOTO 60
C
C Store required o/p value in IB6
C
   70 IB6(I)=MDN
C
C End of pixel loop
C
   80 KADD=KADD+1
      I=I+1
      IF (I.LE.NCOL) GOTO 30
C
C Output row
C
      IF (SEMROW(2,RB6,1,J,1,LP2)) GOTO 100
C
C Cycle the buffer pointers
C
   90 N4=IBPS(1)
      IBPS(1)=IBPS(2)
      IBPS(2)=IBPS(3)
      IBPS(3)=IBPS(4)
      IBPS(4)=IBPS(5)
      IBPS(5)=N4
C
C Step row number - any more to do?
C
      J=J+1
      IF (J.LE.NROW) GOTO 10
  100 RETURN
C
C Errors
C
  110 ERROR=3
      IDERR=NOVER
      GOTO 100
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
