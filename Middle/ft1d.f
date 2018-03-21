C Semper 6 subsidiary module FT1D
C
      SUBROUTINE FT1D(F,N,SIGN,HPLANE,MODSQ,LN)
      REAL F(*)
      INTEGER N,SIGN
      LOGICAL HPLANE,MODSQ,LN
C
C Effects in-place 1D FT on array F as follows:
C If HPLANE false: obtains full plane xform of N complex values, with
C                  origin central in both planes
C           true:  obtains half plane xform (orders 0 to N/2+1) of N/2
C                  real values, with origin central in real space but
C                  at left in Fourier space
C SIGN=-1 goes from real to Fourier space; SIGN=1 inverts, returning
C data larger than originals by number of pixels.  If MODSQ, mod squared
C of data is taken at the F-space end; if LN and SIGN=-1, the mod square
C is followed by a log, so that ln spectra are easily calculated.
C
C Code for real <> c-symm case
C
      IF (HPLANE) THEN
         IF (SIGN.LT.0) THEN
            CALL FT1(F,N,SIGN)
            CALL FT1D5(F,N)
            CALL FT1D2(F,N,SIGN)
            IF (MODSQ) CALL FT1D3(F,N,LN)
         ELSE
            IF (MODSQ) CALL FT1D3(F,N,.FALSE.)
            CALL FT1D2(F,N,SIGN)
            CALL FT1D5(F,N)
            CALL FT1(F,N,SIGN)
         ENDIF
C
C Code for general complex case
C
      ELSE
         IF (SIGN.GT.0) THEN
            IF (MODSQ) CALL FT1D3(F,N,.FALSE.)
         ENDIF
         CALL FT1(F,N,SIGN)
         CALL FT1D4(F,N)
         CALL FT1D5(F,N)
         IF (SIGN.LT.0) THEN
            IF (MODSQ) CALL FT1D3(F,N,LN)
            ENDIF
         ENDIF
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module FT1
C
      SUBROUTINE FT1(F,N,SIGN)
C
C 1-D complex FT routine, with origin at lhs in both planes, and normal
C ordering.  Bit reverses data and then performs Cooley-Tukey passes.
C
      REAL F(*)
      INTEGER N,SIGN
C
      REAL TWOPI
      PARAMETER (TWOPI=6.283185307)
C
      REAL API,APR,AQI,AQR,ARI,ARR,ASI,ASR
      REAL BPI,BPR,BQI,BQR,BRI,BRR,BSI,BSR
      REAL TI,TR,T2I,T2R,T3I,T3R,TMI,TMR,ZI,ZR
      REAL SINTH,THEAPR
      INTEGER I,I1,I2,I5,N2,NL2,NL3,NREALS,P,Q,R,S
C
      NREALS=N+N
C
C Initial bit reversal
C
      I1=1
      DO 20 I=1,NREALS,2
         IF (I.LT.I1) THEN
            TMR=F(I)
            TMI=F(I+1)
            F(I)=F(I1)
            F(I+1)=F(I1+1)
            F(I1)=TMR
            F(I1+1)=TMI
         ENDIF
C
C Add one with downward carry to the high order bit of J1-1.
C
         NL2=N
   10    IF (I1.GT.NL2) THEN
            I1=I1-NL2
            NL2=NL2/2
            IF (NL2.GE.2) GOTO 10
         ENDIF
         I1=I1+NL2
   20 CONTINUE
C
C DFT, bit-reversed to normal order.
C Cooley-Tukey algorithm, Sande-Tukey phase shifts
C
      NL2=2
      N2=N
   30 IF (N2-2)70,50,40
   40 N2=N2/4
      GOTO 30
C
C Extra factor 2?
C
   50 IF (NL2.GE.NREALS) RETURN
      NL3=NL2*2
      DO 60 I5=1,NREALS,NL3
         P=I5
         Q=P+NL2
         TMR=F(Q)
         TMI=F(Q+1)
         APR=F(P)
         API=F(P+1)
         F(Q)=APR-TMR
         F(Q+1)=API-TMI
         F(P)=APR+TMR
         F(P+1)=API+TMI
   60 CONTINUE
      NL2=NL3
C
C 4 point transform
C
   70 IF (NL2.GE.NREALS) RETURN
      NL3=NL2*4
      THEAPR=TWOPI*SIGN/(NL3/2)
      SINTH=SIN(THEAPR/2.)
      ZR=-2.*SINTH*SINTH
      ZI=SIN(THEAPR)
      TR=1.
      TI=0.
      DO 90 I2=1,NL2,2
      IF (I2.GT.1) THEN
         T2R=TR*TR-TI*TI
         T2I=2.*TR*TI
         T3R=T2R*TR-T2I*TI
         T3I=T2R*TI+T2I*TR
      ENDIF
C
      DO 80 P=I2,NREALS,NL3
      Q=P+NL2
      R=Q+NL2
      S=R+NL2
      APR=F(P)
      API=F(P+1)
      AQR=F(Q)
      AQI=F(Q+1)
      ARR=F(R)
      ARI=F(R+1)
      ASR=F(S)
      ASI=F(S+1)
      IF (I2.GT.1) THEN
C
C Apply the phase shift factors
C
         TMR=AQR
         AQR=T2R*AQR-T2I*AQI
         AQI=T2R*AQI+T2I*TMR
         TMR=ARR
         ARR=TR*ARR-TI*ARI
         ARI=TR*ARI+TI*TMR
         TMR=ASR
         ASR=T3R*ASR-T3I*ASI
         ASI=T3R*ASI+T3I*TMR
      ENDIF
C
      BPR=APR+AQR
      BPI=API+AQI
      BQR=APR-AQR
      BQI=API-AQI
      BRR=ARR+ASR
      BRI=ARI+ASI
      BSR=ARR-ASR
      BSI=ARI-ASI
      F(P)=BPR+BRR
      F(P+1)=BPI+BRI
      F(R)=BPR-BRR
      F(R+1)=BPI-BRI
      IF (SIGN.LE.0) THEN
         BSR=-BSR
         BSI=-BSI
      ENDIF
      F(Q)=BQR-BSI
      F(Q+1)=BQI+BSR
      F(S)=BQR+BSI
   80 F(S+1)=BQI-BSR
      TMR=TR
      TR=ZR*TMR-ZI*TI+TMR
   90 TI=ZR*TI+ZI*TMR+TI
      NL2=NL3
      GOTO 70
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module FT1D2
C
      SUBROUTINE FT1D2(F,N,SIGN)
C
C Unpacks an N point complex transform to give the first half
C of the corresponding 2N point transform on real data
C
      REAL F(*)
      INTEGER N,SIGN
C
      REAL AI,AR,BI,BI1,BR,C,COSD,D,S,SIND
      INTEGER I,J,N2P
C
      REAL PI
      PARAMETER (PI=3.1415926536)
C
      N2P=N+N+1
      D=SIGN*PI/REAL(N)
      COSD=COS(D)
      SIND=SIN(D)
      C=-SIGN
      S=0.
      IF (SIGN.GT.0) THEN
         D=1.
      ELSE
         D=.5
      ENDIF
C
C Loop over pixels
C
      J=N2P
      DO 10 I=3,N+1,2
         J=J-2
C
C Recurse trig factor
C
         AR=C
         C=AR*COSD-S*SIND
         S=S*COSD+AR*SIND
C
C Extract conj-symm and -antisymm parts
C
         AR=(F(I)+F(J))*D
         AI=(F(I+1)-F(J+1))*D
         BR=(F(I+1)+F(J+1))*D
         BI=(F(J)-F(I))*D
C
C Apply trig factor to latter
         BI1=BR*S+BI*C
         BR=BR*C-BI*S
C
C Add to obtain first point; subtract conjugates for second
C
         F(I)=AR+BR
         F(I+1)=AI+BI1
         F(J)=AR-BR
         F(J+1)=BI1-AI
   10 CONTINUE
C
C Special code for zero, max orders
C
      IF (SIGN.GT.0) F(2)=F(N2P)
      C=F(2)
      F(2)=F(1)-C
      F(1)=F(1)+C
      IF (SIGN.LT.0) THEN
         F(N2P)=F(2)
         F(N2P+1)=0.
         F(2)=0.
      ENDIF
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module FT1D3
C
      SUBROUTINE FT1D3(F,N,LN)
C
C Replaces complex F(1-N) by mod squared (optionally ln mod squared)
C
      REAL F(*),V1,V2
      INTEGER I,N,N2
      LOGICAL LN
C
      N2=N+N
C
      IF (LN) THEN
         DO 10 I = 1,N2,2
            V1 = F(I)
            V2 = F(I+1)
            V1 = (V1*V1) + (V2*V2)
            IF (V1.NE.0.) THEN
               F(I) = ALOG(V1)
            ELSE
               F(I) = 0.
            ENDIF
            F(I+1) = 0.
   10    CONTINUE
      ELSE
         DO 20 I = 1,N2,2
            V1 = F(I)
            V2 = F(I+1)
            F(I) = (V1*V1) + (V2*V2)
            F(I+1) = 0.
   20    CONTINUE
      ENDIF
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module FT1D4
C
      SUBROUTINE FT1D4(F,N)
C
C Exchanges elements 1..N of F with elems N+1..2N
C
      REAL F(*),T
      INTEGER I,J,N
C
      J = N
      DO 10 I = 1,N
         T = F(I)
         J = J + 1
         F(I) = F(J)
         F(J) = T
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module FT1D5
C
      SUBROUTINE FT1D5(F,N)
C
C Reverses sign of alternate elements of complex F(N)
C
      REAL F(*)
      INTEGER I,J,N
C
      J = 0
      DO 10 I=1,N/2
         J = J + 3
         F(J) = -F(J)
         J = J + 1
         F(J) = -F(J)
   10 CONTINUE
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
