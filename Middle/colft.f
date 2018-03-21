C Semper 6 subsidiary module COLFT
C
      LOGICAL FUNCTION COLFT(LPIN,LPOUT,SIGN,NCOL,HPL,MODSQ,ZEROC)
C
      INTEGER LPIN,LPOUT,SIGN,NCOL
      LOGICAL HPL,MODSQ,ZEROC
C
C Cooley-Tukey column transform, normal to bit-reversed order;
C LPIN to LPOUT; NCOL is number of image space pixels (real or complex)
C
C The finely stepped transform passes are combined where all the
C relevant rows remain available in the cache
C
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      REAL AI,AR,API,APR,AQI,AQR,ARI,ARR,ASI,ASR
      REAL RSIGN,T,T2I,T2R,T3I,T3R,TH,TH1,TI,TR,ZI,ZR
      INTEGER*4 I4N
      INTEGER FORM,I,INC,INC1,J,JJ,KK,LCOMB,LL,LL1,LL2,LPN,M1
      INTEGER N,NBL,NBL1,NC1,NC2,NC2B2,NCACHE,NP2L,NROW,P,P1,R,Q,S
      LOGICAL FORWRD,ODD,MSQ
C
      COLFT=.TRUE.
C
      NROW=NROWS(LPIN)
C
      IF (HPL) THEN
         NC2=NCOL+2
         NC1=NCOL/2
      ELSE
         NC1=NCOL
         NC2=NCOL+NCOL
      ENDIF
C
      FORWRD=SIGN.LT.0
      RSIGN=SIGN
      LPN=LPIN
C
      IF (FORWRD.AND.HPL) THEN
         FORM=NFMFP
      ELSE
         FORM=NFMCOM
      ENDIF
C
      IF (SIGN.GT.0) THEN
         MSQ=MODSQ
      ELSE
         MSQ=.FALSE.
      ENDIF
C
C Treat 1-D case specially
C
      IF (NROW.EQ.1) THEN
         IF (SEMROW(1,RB1,FORM,1,1,LPIN)) GOTO 130
         IF (FORWRD) CALL FT1D(RB1,NC1,SIGN,HPL,MSQ,.FALSE.)
         IF (SEMROW(2,RB1,NFMCOM,1,1,LPOUT)) GOTO 130
      ELSE
C
C Establish number of rows fitting cache
C
         N=LNBLK/LNREAL
         NBL=(NC2-1)/N+1
         I4N=CBSIZE
         I4N=I4N*CBNUM/NBL
         IF (I4N .GT. 32767) THEN
            NCACHE = 32767
         ELSE IF (I4N .LT. 4) THEN
            NCACHE = 4
         ELSE
            NCACHE = I4N
         ENDIF
C
C Find number of passes
C
         M1=1
         LL=NROW
   10    IF (LL.NE.2) THEN
            LL=LL/2
            M1=M1+1
            GOTO 10
         ENDIF
         I=(M1+1)/2
         ODD=I*2.NE.M1
         M1=I
C
C Analyse which passes can be combined
C
         NBL1=1
         LCOMB=1
         I=2
         IF (.NOT.ODD) I=4
   20    I=I*4
         IF (I.LE.NCACHE.AND.I.LE.NROW) THEN
            LCOMB=LCOMB+1
            NBL1=NBL1*4
            GOTO 20
         ENDIF
C
C Caching may fail disastrously with 8 areas accessed at once: unless
C both pictures fit cache, ensure in situ now by copying
C
         IF (LPIN.NE.LPOUT.AND.NCACHE.LT.2*NROW) THEN
            DO 30 J=1,NROW
               IF (SEMROW(1,RB1,FORM,J,1,LPIN)) GOTO 130
               IF (SEMROW(2,RB1,NFMCOM,J,1,LPOUT)) GOTO 130
   30       CONTINUE
C
            LPN=LPOUT
            FORM=NFMCOM
         ENDIF
C
C Begin transform passes
C
         LL1=1
         P1=1
         INC=NROW/4
         NBL=1
         TH=RSIGN*TWOPI/NROW
C
C Set transform pass range for next physical pass
C
   40    LL2=LL1
         IF (LL1.LE.M1-LCOMB) GOTO 60
         LL2=M1
         INC1=INC
         NP2L=NROW/NBL/2
         NBL1=1
         TH1=TH
C
   50    INC=INC1
         NBL=NBL1
         TH=TH1
C
C Level 2 loop: over combined passes
C
   60    DO 120 LL=LL1,LL2
            P=P1
C
C Base 2 code
C
            IF (ODD.AND.LL.EQ.M1) THEN
C
               DO 80 J=1,NP2L
                  R=P+1
C
                  IF (SEMROW(1,RB1,FORM,P,1,LPN)) GOTO 130
                  IF (SEMROW(1,RB2,FORM,R,1,LPN)) GOTO 130
C
                  DO 70 I=1,NC2
                     APR=RB1(I)
                     ARR=RB2(I)
                     RB1(I)=APR+ARR
                     RB2(I)=APR-ARR
   70             CONTINUE
C
                  IF (SEMROW(2,RB2,NFMCOM,R,1,LPOUT)) GOTO 130
                  IF (SEMROW(2,RB1,NFMCOM,P,1,LPOUT)) GOTO 130
C
                  P=P+2
   80          CONTINUE
C
C Base 4 code
C
            ELSE
               ZR=COS(TH)
               ZI=SIN(TH)
C
C Level 3 loop: over blocks
C
               DO 110 JJ=1,NBL
                  TR=1.
                  TI=0.
C
C Level 4 loop: over quartets within block
C
                  DO 100 KK=1,INC
C
C Obtain trig factors for twice and thrice angle
C
                     T2R=TR*TR-TI*TI
                     T2I=TR*TI*2
                     T3R=TR*T2R-TI*T2I
                     T3I=TR*T2I+TI*T2R
C
C Read in four rows
C
                     R=P+INC
                     Q=R+INC
                     S=Q+INC
C
                     IF (SEMROW(1,RB1,FORM,S,1,LPN)) GOTO 130
                     IF (SEMROW(1,RB2,FORM,Q,1,LPN)) GOTO 130
                     IF (SEMROW(1,RB3,FORM,R,1,LPN)) GOTO 130
                     IF (SEMROW(1,RB4,FORM,P,1,LPN)) GOTO 130
C
C Include row transforms on first forward pass
C
                     IF (LL.EQ.1) THEN
                        IF (FORWRD) THEN
                           CALL FT1D(RB1,NC1,SIGN,HPL,MSQ,.FALSE.)
                           CALL FT1D(RB2,NC1,SIGN,HPL,MSQ,.FALSE.)
                           CALL FT1D(RB3,NC1,SIGN,HPL,MSQ,.FALSE.)
                           CALL FT1D(RB4,NC1,SIGN,HPL,MSQ,.FALSE.)
                        ELSE
C
C Zero centre pixel on first inverse pass if ZEROC
C
                           IF (ZEROC.AND.P.EQ.1) THEN
                              IF (HPL) THEN
                                 I=1
                              ELSE
                                 I=NCOL+1
                              ENDIF
C
                              RB2(I)=0.
                              RB2(I+1)=0.
                           ENDIF
C
C Take mod squ on first inverse pass if MSQ
C
                           IF (MSQ) THEN
                              NC2B2=NC2/2
                              CALL FT1D3(RB1,NC2B2,.FALSE.)
                              CALL FT1D3(RB2,NC2B2,.FALSE.)
                              CALL FT1D3(RB3,NC2B2,.FALSE.)
                              CALL FT1D3(RB4,NC2B2,.FALSE.)
                           ENDIF
                        ENDIF
                     ENDIF
C
C Four-point transform on columns
C
                     DO 90 I=1,NC2,2
                        APR=RB4(I)
                        API=RB4(I+1)
                        ARR=RB3(I)
                        ARI=RB3(I+1)
                        AQR=RB2(I)
                        AQI=RB2(I+1)
                        ASR=RB1(I)
                        ASI=RB1(I+1)
C
C 4-point transform block (natural to bit-reversed order)
C
                        RB4(I)=APR+ARR+AQR+ASR
                        RB4(I+1)=API+ARI+AQI+ASI
                        AR=APR-AQR+RSIGN*(ASI-ARI)
                        AI=API-AQI+RSIGN*(ARR-ASR)
                        RB2(I)=TR*AR-TI*AI
                        RB2(I+1)=TR*AI+TI*AR
                        AR=APR+AQR-ARR-ASR
                        AI=API+AQI-ARI-ASI
                        RB3(I)=T2R*AR-T2I*AI
                        RB3(I+1)=T2R*AI+T2I*AR
                        AR=APR-AQR+RSIGN*(ARI-ASI)
                        AI=API-AQI+RSIGN*(ASR-ARR)
                        RB1(I)=T3R*AR-T3I*AI
                        RB1(I+1)=T3R*AI+T3I*AR
   90                CONTINUE
C
C Output rows
C
                     IF (SEMROW(2,RB4,NFMCOM,P,1,LPOUT)) GOTO 130
                     IF (SEMROW(2,RB3,NFMCOM,R,1,LPOUT)) GOTO 130
                     IF (SEMROW(2,RB2,NFMCOM,Q,1,LPOUT)) GOTO 130
                     IF (SEMROW(2,RB1,NFMCOM,S,1,LPOUT)) GOTO 130
C
C Recurse trig factor
C
                     T=ZR*TR-ZI*TI
                     TI=ZR*TI+ZI*TR
                     TR=T
C
                     P=P+1
  100             CONTINUE
C
C End of level 4 loop (over quartets)
C
                  P=S+1
  110          CONTINUE
C
C End of level 3 loop (over blocks)
C
C Prepare next pass
C
               TH=TH*4
C
               NBL=NBL*4
               INC=INC/4
               LPN=LPOUT
               FORM=NFMCOM
            ENDIF
C
  120    CONTINUE
C
C End of level 2 loop (over combined passes)
C
C Multi-pass blocks in progress?
C
         IF (P.LE.NROW) THEN
            P1=P
            GOTO 50
         ENDIF
C
C Full pass complete
C
         IF (LL2.NE.M1) THEN
            P1=1
            LL1=LL2+1
            GOTO 40
         ENDIF
      ENDIF
C
      COLFT=.FALSE.
C
  130 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 subsidiary module COLFT2
C
      LOGICAL FUNCTION COLFT2(LPIN,LPOUT,S,NCOL,HPL,MSQ,LN,RN)
C
      INTEGER LPIN,LPOUT,S,NCOL
      LOGICAL HPL,MSQ,LN
      REAL RN
C
C Bit reverses columns, including origin shift in both planes;
C if inverse, performs row transforms and normalises at end;
C if forward and MSQ, takes (ln, if LN) mod squ
C
      LOGICAL COLFT3,SEMROW
C
      INCLUDE 'COMMON'
C
      INTEGER FORM,I,INC1,INC2,J,J1,JBR,JBR1,N,N2,NC1,NC2,NROW
      LOGICAL TERM
C
      COLFT2=.TRUE.
C
      NROW=NROWS(LPIN)
C
      IF (HPL) THEN
         NC1=NCOL/2
         NC2=NCOL+2
         IF (S.LT.0) THEN
            FORM=NFMCOM
         ELSE
            FORM=NFMFP
         ENDIF
      ELSE
         NC1=NCOL
         NC2=2*NCOL
         FORM=NFMCOM
      ENDIF
C
C Treat 1-D case specially
C
      IF (NROW.EQ.1) THEN
         IF (SEMROW(1,RB1,NFMCOM,1,1,LPIN)) GOTO 60
         IF (COLFT3(RB1,FORM,1,LPOUT,NC2,NC1,S,.FALSE.,HPL,MSQ,LN,RN))
     +      GOTO 60
C
C Combining the origin shift with the bit reversal calls for a row
C access pattern in rings of 8 as follows (j denotes the inner bits
C of a row number and b the bit reverse of j):
C   0j0 (0b0) 1b0 (0j1) 1j1 (1b1) 0b1 (1j0) 0j0
C + 0b0 (0j0) 1j0 (0b1) 1b1 (1j1) 0j1 (1b0) 0b0    (forward)
C
C Loop over J from 1 to N, with JBR=bitrev(J) in step (both in fact
C offset by 1)
C
      ELSE
         N=NROW/4
         INC1=NROW/2
         INC2=1
         JBR=1
C
         DO 50 J=1,N
            J1=2*J-1
            JBR1=2*JBR-1
            TERM=.FALSE.
            IF (JBR-J)30,10,20
   10       TERM=.TRUE.
   20       IF (SEMROW(1,RB2,NFMCOM,J1,1,LPIN)) GOTO 60
C
            JBR1=JBR1+INC1
            IF (SEMROW(1,RB1,NFMCOM,JBR1,1,LPIN)) GOTO 60
C
C COLFT3 outputs row, including post processing for 1-D xforms, mod squ,
C ln, negation and normalisation
C
            IF (COLFT3(RB2,FORM,JBR1,LPOUT,NC2,NC1,S,.FALSE.,HPL,MSQ,LN,
     +                 RN)) GOTO 60
C
            J1=J1+INC1+INC2
            IF (SEMROW(1,RB2,NFMCOM,J1,1,LPIN)) GOTO 60
            IF (COLFT3(RB1,FORM,J1,LPOUT,NC2,NC1,S,.TRUE.,HPL,MSQ,LN,
     +                 RN)) GOTO 60
C
            JBR1=JBR1+INC2-INC1
            IF (SEMROW(1,RB1,NFMCOM,JBR1,1,LPIN)) GOTO 60
            IF (COLFT3(RB2,FORM,JBR1,LPOUT,NC2,NC1,S,.TRUE.,HPL,MSQ,LN,
     +                 RN)) GOTO 60
C
            J1=J1-INC1-INC2
            IF (COLFT3(RB1,FORM,J1,LPOUT,NC2,NC1,S,.FALSE.,HPL,MSQ,LN,
     +                 RN)) GOTO 60
C
C Repeat for second (adjacent) ring, unless identical
C
            IF (.NOT.TERM) THEN
               I=J1
               J1=JBR1-INC2
               JBR1=I
               TERM=.TRUE.
               GOTO 20
            ENDIF
C
C Generate next JBR (add one to most sig bit, with downward carry)
C
   30       N2=N/2
C
   40       IF (JBR.GT.N2) THEN
               JBR=JBR-N2
               N2=N2/2
               IF (N2.GE.1) GOTO 40
            ENDIF
C
            JBR=JBR+N2
   50    CONTINUE
      ENDIF
C
      COLFT2=.FALSE.
C
   60 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module COLFT3
C
      LOGICAL FUNCTION COLFT3(B,FORM,N,LPN,NC2,NC1,S,NEG,HPL,MSQ,LN,RN)
C
      REAL B(*),RN
      INTEGER FORM,N,LPN,NC2,NC1,S
      LOGICAL NEG,HPL,MSQ,LN
C
C Performs post-processing for COLFT2, inserting 1-D transforms,
C mod squ, ln, negation and normalisation acc to args
C
      LOGICAL SEMROW
C
      REAL T
      INTEGER I
C
      IF (S.LT.0) THEN
         IF (MSQ) THEN
            CALL FT1D3(B,NC1+1,LN)
         ELSE IF (NEG) THEN
            DO 10 I=1,NC2
               B(I)=-B(I)
   10       CONTINUE
         ENDIF
      ELSE
         CALL FT1D(B,NC1,S,HPL,.FALSE.,.FALSE.)
C
C Pick up central image pixel as normalising factor if negative
C factor supplied (ACF case)
C
         IF (RN.LT.0.) RN=1./B(NC1+1)
C
         IF (NEG) THEN
            T=-RN
         ELSE
            T=RN
         ENDIF
C
         DO 20 I=1,NC2
            B(I)=B(I)*T
   20    CONTINUE
      ENDIF
C
      COLFT3=SEMROW(2,B,FORM,N,1,LPN)
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
