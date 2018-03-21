C Semper 6 processing module WALSH
C
      SUBROUTINE WALSH
C
C Code for WALSH/IMAGE commands to carry out forward/inverse Walsh
C transforms.  The source picture class must be IMAGE for the WALSH
C command and WALSH for the IMAGE command.  Any other picture class
C is faulted.
C
      LOGICAL SEMCP2,SEMOPN,WALSH1,WALSH2
      INTEGER IPACK,IVALPN,SEMFRM
C
      REAL    RNORM
      INTEGER NPIC,NCOL,NROW,ICLASS,OCLASS,FORM
C
      INCLUDE 'COMMON'
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault picture size if not a power of 2
C
      IF (SEMCP2(NCOL,NROW)) THEN
         ERROR=5
      IDERR=1000*DEVN(LP1)+PICN(LP1)
         GOTO 10
      ENDIF
C
C Determine required class for source and output pictures and
C normalisation factor according to whether WALSH or IMAGE command
C
      IF (VERB.EQ.-4853) THEN
         ICLASS=NCLIMA
         OCLASS=NCLWAL
         RNORM=1.0
      ELSE
         ICLASS=NCLWAL
         OCLASS=NCLIMA
         RNORM=1.0/(REAL(NCOL)*REAL(NROW))
      ENDIF
C
C Fault bad class for source picture
C
      IF (CLASSN(LP1).NE.ICLASS) THEN
         ERROR=6
         IDERR=1000*DEVN(LP1)+PICN(LP1)
         GOTO 10
      ENDIF
C
C Fetch the output picture number specified with the TO key
C
      NPIC=IVALPN(-601)
C
C Determine data form for output picture
C
      IF (FORMN(LP1).EQ.NFMCOM) THEN
         FORM=NFMCOM
      ELSE
         FORM=NFMFP
      ENDIF
C
C Open output picture
C
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,OCLASS,FORM,LP2)) GOTO 10
C
C Carry out Walsh transform
C
         IF (WALSH1(LP1,LP2)) GOTO 10
C
C Output picture becomes source for next stage of processing
C
         LP1=LP2
C
C If IMAGE command, re-open output picture to allow specified data form
C (if any) to take effect
C
         IF (VERB.EQ.14921) THEN
C
C Force output data form
C
            FORM=SEMFRM(FORM)
C
C Open output picture
C
            IF (SEMOPN(2,NPIC,NCOL,NROW,1,OCLASS,FORM,LP2)) GOTO 10
         ENDIF
C
C Re-order and normalise the results
C
         IF (WALSH2(LP1,LP2,RNORM)) GOTO 10
C
   10 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module WALSH1
C
      LOGICAL FUNCTION WALSH1(LPIN,LPOUT)
C
      INTEGER LPIN,LPOUT
C
C 2-D Walsh/Hadamard transform (class 1 to class 7 and back)
C
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      INTEGER FORM,TL,P,Q,R,S
      INTEGER LPN,NROW,NCOL,NCOL2,NL,NL2,M1,LL,K,KK,JJ,I
      LOGICAL DONE1D,ODD,COMPL
      REAL APR,ARR,AQR,ASR
C
      WALSH1=.TRUE.
C
      LPN=LPIN
      DONE1D=.FALSE.
      NROW=NROWS(LPIN)
      NCOL=NCOLS(LPIN)
C
      COMPL=FORMN(LPIN).EQ.NFMCOM
C
      IF (COMPL) THEN
         FORM=NFMCOM
         NCOL2=2*NCOL
      ELSE
         FORM=NFMFP
         NCOL2=NCOL
      ENDIF
C
C Treat 1-D case specialy
C
      IF (NROW.EQ.1) THEN
         IF (SEMROW(1,RB1,FORM,1,1,LPIN)) GOTO 80
         CALL WAL1D(RB1,NCOL,COMPL)
         IF (SEMROW(2,RB1,FORM,1,1,LPOUT)) GOTO 80
      ELSE
C
C Initialise column transform
C
         ODD=.FALSE.
         TL=1
         NL=NROW
         NL2=NL/4
C
         M1=0
         LL=NL
   10    IF (LL.GT.2) THEN
            LL=LL/2
            ODD=.NOT.ODD
            M1=M1+1
            GOTO 10
         ENDIF
C
C Begin 3 level transform loop
C
         DO 50 LL=1,M1,2
C
            K=1
            DO 40 KK=1,NL2
C
               P=K
               DO 30 JJ=1,TL
C
                  R=P+NL2
                  Q=R+NL2
                  S=Q+NL2
C
C Read in four rows
C
                  IF (SEMROW(1,RB2,FORM,P,1,LPN)) GOTO 80
                  IF (SEMROW(1,RB3,FORM,R,1,LPN)) GOTO 80
                  IF (SEMROW(1,RB4,FORM,Q,1,LPN)) GOTO 80
                  IF (SEMROW(1,RB1,FORM,S,1,LPN)) GOTO 80
C
C Perform row transforms at IMAGE end of operation
C
                  IF (.NOT.DONE1D) THEN
                     CALL WAL1D(RB2,NCOL,COMPL)
                     CALL WAL1D(RB3,NCOL,COMPL)
                     CALL WAL1D(RB4,NCOL,COMPL)
                     CALL WAL1D(RB1,NCOL,COMPL)
                  ENDIF
C
C Four-point transform of rows
C
                  DO 20 I=1,NCOL2
                     APR=RB2(I)
                     ARR=RB3(I)
                     AQR=RB4(I)
                     ASR=RB1(I)
                     RB2(I)=APR+ARR+AQR+ASR
                     RB4(I)=APR-AQR+ARR-ASR
                     RB3(I)=APR+AQR-ARR-ASR
                     RB1(I)=APR-AQR-ARR+ASR
   20             CONTINUE
C
C Write back
C
                  IF (SEMROW(2,RB1,FORM,S,1,LPOUT)) GOTO 80
                  IF (SEMROW(2,RB4,FORM,Q,1,LPOUT)) GOTO 80
                  IF (SEMROW(2,RB3,FORM,R,1,LPOUT)) GOTO 80
                  IF (SEMROW(2,RB2,FORM,P,1,LPOUT)) GOTO 80
C
                  P=P+NL
   30          CONTINUE
C
               K=K+1
   40       CONTINUE
C
C Do no more 1-D transforms
C
            DONE1D=.TRUE.
C
C Do the rest in situ in LPOUT
C
            LPN=LPOUT
            TL=TL*4
            NL=NL/4
            NL2=NL2/4
   50    CONTINUE
C
C Extra factor 2?
C
         IF (.NOT.ODD) THEN
            TL=NROW/2
            P=1
            DO 70 JJ=1,TL
               Q=P+1
C
               IF (SEMROW(1,RB2,FORM,P,1,LPOUT)) GOTO 80
               IF (SEMROW(1,RB3,FORM,Q,1,LPOUT)) GOTO 80
C
               DO 60 I=1,NCOL2
                  APR=RB2(I)
                  AQR=RB3(I)
                  RB2(I)=APR+AQR
                  RB3(I)=APR-AQR
   60          CONTINUE
C
               IF (SEMROW(2,RB3,FORM,Q,1,LPOUT)) GOTO 80
               IF (SEMROW(2,RB2,FORM,P,1,LPOUT)) GOTO 80
C
               P=P+2
   70       CONTINUE
         ENDIF
      ENDIF
C
      WALSH1=.FALSE.
C
   80 RETURN
C
C Copyright (C) 1987,1989,1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module WALSH2
C
      LOGICAL FUNCTION WALSH2(LPIN,LPOUT,RNORM)
C
      INTEGER LPIN,LPOUT
      REAL    RNORM
C
C Reorders (bit-reversed to natural or natural to bit-reversed);
C scrambles and grids LPIN to LPOUT so that (WALSH1,WALSH2) and
C (WALSH2,WALSH1) are both central origin transforms
C
      LOGICAL SEMROW,WALSH3
C
      INTEGER FORM,NROW,NB4,INC1,INC2,IR,I,J,IQ,NB2
C
      INCLUDE 'COMMON'
C
      WALSH2=.TRUE.
C
      NROW=NROWS(LPIN)
      NB2=NROW/2
      NB4=NROW/4
      INC1=NB2
      INC2=1
C
      IF (FORMN(LPIN).EQ.NFMCOM) THEN
         FORM=NFMCOM
      ELSE
         FORM=NFMFP
      ENDIF
C
C Treate 1-D case specially
C
      IF (NROW.EQ.1) THEN
         IF (WALSH3(LPIN,1,RB1,FORM,RNORM)) GOTO 30
         IF (SEMROW(2,RB1,FORM,1,1,LPOUT)) GOTO 30
C
      ELSE
         IR=1
         DO 20 I=1,NROW,2
C
C Ring
C
            IF (I.LT.IR) THEN
               IF (IR.LE.NB2) THEN
                  IF (WALSH3(LPIN,I,RB2,FORM,RNORM)) GOTO 30
C
                  J=IR+INC1
                  IF (WALSH3(LPIN,J,RB1,FORM,RNORM)) GOTO 30
                  IF (SEMROW(2,RB2,FORM,J,1,LPOUT)) GOTO 30
C
                  J=I+1+NB2
                  IF (WALSH3(LPIN,J,RB2,FORM,RNORM)) GOTO 30
                  IF (SEMROW(2,RB1,FORM,J,1,LPOUT)) GOTO 30
C
                  J=IR+INC2
                  IF (WALSH3(LPIN,J,RB1,FORM,RNORM)) GOTO 30
                  IF (SEMROW(2,RB2,FORM,J,1,LPOUT)) GOTO 30
                  IF (SEMROW(2,RB1,FORM,I,1,LPOUT)) GOTO 30
C
                  IF (WALSH3(LPIN,IR,RB2,FORM,RNORM)) GOTO 30
C
                  J=I+INC1
                  IF (WALSH3(LPIN,J,RB1,FORM,RNORM)) GOTO 30
                  IF (SEMROW(2,RB2,FORM,J,1,LPOUT)) GOTO 30
C
                  J=IR+1+NB2
                  IF (WALSH3(LPIN,J,RB2,FORM,RNORM)) GOTO 30
                  IF (SEMROW(2,RB1,FORM,J,1,LPOUT)) GOTO 30
C
                  J=I+INC2
                  IF (WALSH3(LPIN,J,RB1,FORM,RNORM)) GOTO 30
                  IF (SEMROW(2,RB2,FORM,J,1,LPOUT)) GOTO 30
                  IF (SEMROW(2,RB1,FORM,IR,1,LPOUT)) GOTO 30
               ENDIF
C
C Line
C
            ELSE IF (I.EQ.IR) THEN
               IF (WALSH3(LPIN,I,RB2,FORM,RNORM)) GOTO 30
C
               J=I+INC1
               IF (WALSH3(LPIN,J,RB1,FORM,RNORM)) GOTO 30
               IF (SEMROW(2,RB2,FORM,J,1,LPOUT)) GOTO 30
C
               J=I+1+NB2
               IF (WALSH3(LPIN,J,RB2,FORM,RNORM)) GOTO 30
               IF (SEMROW(2,RB1,FORM,J,1,LPOUT)) GOTO 30
C
               J=I+INC2
               IF (WALSH3(LPIN,J,RB1,FORM,RNORM)) GOTO 30
               IF (SEMROW(2,RB2,FORM,J,1,LPOUT)) GOTO 30
               IF (SEMROW(2,RB1,FORM,I,1,LPOUT)) GOTO 30
            ENDIF
C
            IQ=NB4
   10       IF (IR.GT.IQ) THEN
               IR=IR-IQ
               IQ=IQ/2
               IF (IQ.GT.0) GOTO 10
            ENDIF
C
            IR=IR+IQ
   20    CONTINUE
      ENDIF
C
      WALSH2=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987,1989,1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module WALSH3
C
      LOGICAL FUNCTION WALSH3 (LPIN,IROW,BUFFER,FORM,RNORM)
C
      INTEGER LPIN,IROW,FORM
      REAL    BUFFER(*),RNORM
C
C Reads rows for WALSH2 performing the 1-d part of the operation
C
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      INTEGER P,BRP,L,I,J,INC1,INC2,INC3,IR,IQ
      REAL T1,T2,AR,BR
C
      WALSH3=.TRUE.
C
      IF (SEMROW(1,BUFFER,FORM,IROW,1,LPIN)) GOTO 50
C
C Final grid is achieved indirectly by negating (+,+) and (-,-)
C quadrants before bit reversal; normalisation is included if
C in direction of real plane
C
      IF (IROW.GT.NROWS(LPIN)/2) THEN
         T1=-RNORM
         T2= RNORM
      ELSE
         T1= RNORM
         T2=-RNORM
      ENDIF
C
      IF (FORM.NE.NFMCOM) THEN
         L=NCOLS(LPIN)/2
      ELSE
         L=NCOLS(LPIN)
      ENDIF
C
      DO 10 I=1,L
         BUFFER(I  )=T1*BUFFER(I  )
         BUFFER(I+L)=T2*BUFFER(I+L)
   10 CONTINUE
C
      IF (FORM.NE.NFMCOM) THEN
         INC1=NCOLS(LPIN)/2
         INC2=INC1+1
         INC3=1
      ELSE
         INC1=NCOLS(LPIN)
         INC2=INC1+2
         INC3=2
      ENDIF
C
C Loop over every other element, reversing P to BRP each cycle
C
      BRP=1
      DO 40 P=1,NCOLS(LPIN),2
C
         IF (FORM.NE.NFMCOM) THEN
            I=P
            IR=BRP
         ELSE
            I=2*P-1
            IR=2*BRP-1
         ENDIF
C
C Ring
C
   20    IF (I.LT.IR)THEN
            AR=BUFFER(I)
            J=IR+INC1
            BR=BUFFER(J)
            BUFFER(J)=AR
            J=I+INC2
            AR=BUFFER(J)
            BUFFER(J)=BR
            J=IR+INC3
            BR=BUFFER(J)
            BUFFER(J)=AR
            BUFFER(I)=BR
            AR=BUFFER(IR)
            J=I+INC1
            BR=BUFFER(J)
            BUFFER(J)=AR
            J=IR+INC2
            AR=BUFFER(J)
            BUFFER(J)=BR
            J=I+INC3
            BR=BUFFER(J)
            BUFFER(J)=AR
            BUFFER(IR)=BR
C
C Line
C
         ELSE IF (I.EQ.IR) THEN
            AR=BUFFER(I)
            J=I+INC1
            BR=BUFFER(J)
            BUFFER(J)=AR
            J=I+INC2
            AR=BUFFER(J)
            BUFFER(J)=BR
            J=I+INC3
            BR=BUFFER(J)
            BUFFER(J)=AR
            BUFFER(I)=BR
         ENDIF
C
C Repeat for im part?
C
         IF (FORM.EQ.NFMCOM.AND.I.LT.2*P) THEN
            I=I+1
            IR=IR+1
            GOTO 20
         ENDIF
C
C Generate bit reverse of next input P (add msb with downward
C carry)
C
         IQ=NCOLS(LPIN)/4
   30    IF (BRP.GT.IQ) THEN
            BRP=BRP-IQ
            IQ=IQ/2
            IF (IQ.GE.1) GOTO 30
         ENDIF
C
         BRP=BRP+IQ
   40 CONTINUE
C
      WALSH3=.FALSE.
C
   50 RETURN
C
C Copyright (C) 1987,1989,1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module WAL1D
C
      SUBROUTINE WAL1D(F,N,COMPL)
C
      REAL    F(*)
      INTEGER N
      LOGICAL COMPL
C
C 1-D Walsh/Hadamard transform in situ
C
      LOGICAL ODD
      INTEGER P,Q,R,S,FIRST,PRB2SE,INC,NPASS,N1,NL,M2,NLB4,M3,N2,N3
      REAL APR,ARR,AQR,ASR,BPR,BRR,BQR,BSR
C
C Initialise
C
      IF (COMPL) THEN
         INC=2
      ELSE
         INC=1
      ENDIF
C
      ODD=.FALSE.
      NPASS=0
      N1=N
   10 IF (N1.GT.2) THEN
         N1=N1/2
         ODD=.NOT.ODD
         NPASS=NPASS+1
         GOTO 10
      ENDIF
C
C Begin 3 level transform loop
C
      DO 60 FIRST=1,INC
C
         NL=N
         IF (COMPL) NL=2*NL
         M2=N/4
         NLB4=NL/4
         M3=1
C
         DO 40 N1=1,NPASS,2
C
            PRB2SE=FIRST
            DO 30 N2=1,M2
C
               P=PRB2SE
               DO 20 N3=1,M3
C
                  R=P+NLB4
                  Q=R+NLB4
                  S=Q+NLB4
                  APR=F(P)
                  ARR=F(R)
                  AQR=F(Q)
                  ASR=F(S)
                  BPR=APR+AQR+ARR+ASR
                  BRR=APR+AQR-ARR-ASR
                  BQR=APR-AQR+ARR-ASR
                  BSR=APR-AQR-ARR+ASR
                  F(P)=BPR
                  F(R)=BRR
                  F(Q)=BQR
                  F(S)=BSR
C
                  P=P+NL
   20          CONTINUE
C
               PRB2SE=PRB2SE+INC
   30       CONTINUE
C
            M3=M3*4
            NL=NL/4
            M2=M2/4
            NLB4=NLB4/4
   40    CONTINUE
C
C Extra factor 2
C
         IF (.NOT.ODD) THEN
            M3=N/2
C
            P=FIRST
            DO 50 N3=1,M3
C
               Q=P+INC
               APR=F(P)
               AQR=F(Q)
               F(P)=APR+AQR
               F(Q)=APR-AQR
C
               P=Q+INC
   50       CONTINUE
         ENDIF
   60 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989,1992:  Synoptics Ltd,  All Rights Reserved
C
      END
