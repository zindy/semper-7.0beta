C Semper local module LQSET
C -  solves linear equations, checking conditioning
C
      SUBROUTINE LQSET(A,MA,N,RHS,SOLN,CONDN)
      INTEGER MA,N,MAXN
      PARAMETER (MAXN=10)
      REAL A(MA,N),RHS(N),SOLN(N),W(MAXN),V(MAXN,MAXN),T,WMAX,WMIN,CONDN
      INTEGER I
C Debug
C      character*80 record
C      logical semcon,ldum
C
C Make singular-value decomposition
      CALL SVDCMP(A,N,N,MA,MA,W,V)
C
C Debug
C      ldum=semcon('Coeff matrix eigenvalues:')
C      write (record,1) (w(i),i=1,n)
C 1    format (6g12.4)
C      ldum=semcon(record)
C
C Appraise conditioning
      WMAX=W(1)
      WMIN=W(1)
      DO 10 I=1,N
         T=ABS(W(I))
         WMAX=MAX(WMAX,T)
         WMIN=MIN(WMIN,T)
   10 CONTINUE
      IF (WMAX.EQ.0.OR.WMIN.LT.1E-5*WMAX) THEN
         CONDN=1E5
      ELSE
         CONDN=WMAX/WMIN
      ENDIF
 
C Zero small elements of W
      DO 20 I=1,N
         IF (W(I).LT.1E-6*WMAX) W(I)=0.
   20 CONTINUE
C
C Back-substitute
      CALL SVBKSB(A,W,V,N,N,MA,MA,RHS,SOLN)
C
      RETURN
      END
C
C Pro tem, from Numerical Recipes with no more than minor adjs
C
      SUBROUTINE SVDCMP(A,M,N,MP,NP,W,V)
      INTEGER NMAX,M,N,MP,NP
      PARAMETER (NMAX=10)
      REAL A(MP,*),W(*),V(NP,*),RV1(NMAX)
      REAL C,F,G,H,SCALE,ANORM,S,X,Y,Z
      INTEGER I,J,K,L,ITS,NM
      G=0.0
      SCALE=0.0
      ANORM=0.0
      DO 140 I=1,N
         L=I+1
         RV1(I)=SCALE*G
         G=0.0
         S=0.0
         SCALE=0.0
         IF (I.LE.M) THEN
            DO 10 K=I,M
               SCALE=SCALE+ABS(A(K,I))
   10       CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 20 K=I,M
                  A(K,I)=A(K,I)/SCALE
                  S=S+A(K,I)*A(K,I)
   20          CONTINUE
               F=A(I,I)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,I)=F-G
               IF (I.NE.N) THEN
                  DO 50 J=L,N
                     S=0.0
                     DO 30 K=I,M
                        S=S+A(K,I)*A(K,J)
   30                CONTINUE
                     F=S/H
                     DO 40 K=I,M
                        A(K,J)=A(K,J)+F*A(K,I)
   40                CONTINUE
   50             CONTINUE
               ENDIF
               DO 60 K= I,M
                  A(K,I)=SCALE*A(K,I)
   60          CONTINUE
            ENDIF
         ENDIF
         W(I)=SCALE *G
         G=0.0
         S=0.0
         SCALE=0.0
         IF ((I.LE.M).AND.(I.NE.N)) THEN
            DO 70 K=L,N
               SCALE=SCALE+ABS(A(I,K))
   70       CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 80 K=L,N
                  A(I,K)=A(I,K)/SCALE
                  S=S+A(I,K)*A(I,K)
   80          CONTINUE
               F=A(I,L)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,L)=F-G
               DO 90 K=L,N
                  RV1(K)=A(I,K)/H
   90          CONTINUE
               IF (I.NE.M) THEN
                  DO 120 J=L,M
                     S=0.0
                     DO 100 K=L,N
                        S=S+A(J,K)*A(I,K)
  100                CONTINUE
                     DO 110 K=L,N
                        A(J,K)=A(J,K)+S*RV1(K)
  110                CONTINUE
  120             CONTINUE
               ENDIF
               DO 130 K=L,N
                  A(I,K)=SCALE*A(I,K)
  130          CONTINUE
            ENDIF
         ENDIF
         ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
  140 CONTINUE
C
      DO 200 I=N,1,-1
         IF (I.LT.N) THEN
            IF (G.NE.0.0) THEN
               DO 150 J=L,N
                  V(J,I)=(A(I,J)/A(I,L))/G
  150          CONTINUE
               DO 180 J=L,N
                  S=0.0
                  DO 160 K=L,N
                     S=S+A(I,K)*V(K,J)
  160             CONTINUE
                  DO 170 K=L,N
                     V(K,J)=V(K,J)+S*V(K,I)
  170             CONTINUE
  180          CONTINUE
            ENDIF
            DO 190 J=L,N
               V(I,J)=0.0
               V(J,I)=0.0
  190       CONTINUE
         ENDIF
         V(I,I)=1.0
         G=RV1(I)
         L=I
  200 CONTINUE
C
      DO 270 I=N,1,-1
         L=I+1
         G=W(I)
         IF (I.LT.N) THEN
            DO 210 J=L,N
               A(I,J)=0.0
  210       CONTINUE
         ENDIF
         IF (G.NE.0.0) THEN
            G=1.0/G
            IF (I.NE.N) THEN
            DO 240 J=L,N
               S=0.0
               DO 220 K=L,M
                  S=S+A(K,I)*A(K,J)
  220          CONTINUE
               F=(S/A(I,I))*G
               DO 230 K=I,M
                  A(K,J)=A(K,J)+F*A(K,I)
  230          CONTINUE
  240       CONTINUE
         ENDIF
         DO 250 J=I,M
            A(J,I)=A(J,I)*G
  250    CONTINUE
      ELSE
         DO 260 J= I,M
            A(J,I)=0.0
  260    CONTINUE
      ENDIF
      A(I,I)=A(I,I)+1.0
  270 CONTINUE
C
      DO 390 K=N,1,-1
         DO 370 ITS=1,30
            DO 280 L=K,1,-1
               NM=L-1
               IF ((ABS(RV1(L))+ANORM).EQ.ANORM)  GOTO 320
               IF ((ABS(W(NM))+ANORM).EQ.ANORM)  GOTO 290
  280       CONTINUE
  290       C=0.0
            S=1.0
            DO 310 I=L,K
               F=S*RV1(I)
               IF ((ABS(F)+ANORM).NE.ANORM) THEN
                  G=W(I)
                  H=SQRT(F*F+G*G)
                  W(I)=H
                  H=1.0/H
                  C= (G*H)
                  S=-(F*H)
                  DO 300 J=1,M
                     Y=A(J,NM)
                     Z=A(J,I)
                     A(J,NM)=(Y*C)+(Z*S)
                     A(J,I)=-(Y*S)+(Z*C)
  300             CONTINUE
               ENDIF
  310       CONTINUE
  320       Z=W(K)
            IF (L.EQ.K) THEN
               IF (Z.LT.0.0) THEN
                  W(K)=-Z
                  DO 330 J=1,N
                     V(J,K)=-V(J,K)
  330             CONTINUE
               ENDIF
               GOTO 380
            ENDIF
            X=W(L)
            NM=K-1
            Y=W(NM)
            G=RV1(NM)
            H=RV1(K)
            F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
            G=SQRT(F*F+1.0)
            F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
            C=1.0
            S=1.0
            DO 360 J=L,NM
               I=J+1
               G=RV1(I)
               Y=W(I)
               H=S*G
               G=C*G
               Z=SQRT(F*F+H*H)
               RV1(J)=Z
               C=F/Z
               S=H/Z
               F= (X*C)+(G*S)
               G=-(X*S)+(G*C)
               H=Y*S
               Y=Y*C
               DO 340 NM=1,N
                  X=V(NM,J)
                  Z=V(NM,I)
                  V(NM,J)= (X*C)+(Z*S)
                  V(NM,I)=-(X*S)+(Z*C)
  340          CONTINUE
               Z=SQRT(F*F+H*H)
               W(J)=Z
               IF (Z.NE.0.0) THEN
                  Z=1.0/Z
                  C=F*Z
                  S=H*Z
               ENDIF
               F= (C*G)+(S*Y)
               X=-(S*G)+(C*Y)
               DO 350 NM=1,M
                  Y=A(NM,J)
                  Z=A(NM,I)
                  A(NM,J)= (Y*C)+(Z*S)
                  A(NM,I)=-(Y*S)+(Z*C)
  350          CONTINUE
  360       CONTINUE
            RV1(L)=0.0
            RV1(K)=F
            W(K)=X
  370    CONTINUE
  380    CONTINUE
  390 CONTINUE
      RETURN
      END
C
      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)
      INTEGER NMAX,M,N,MP,NP,I,J,JJ
      PARAMETER (NMAX=10)
      REAL U(MP,*),W(*),V(NP,*),B(*),X(*),TMP(10)
      REAL S
      DO 20 J=1,N
         S=0.
         IF(W(J).NE.0.) THEN
            DO 10 I=1,M
               S=S+U(I,J)*B(I)
   10       CONTINUE
            S=S/W(J)
         ENDIF
         TMP(J)=S
   20 CONTINUE
      DO 40 J=1,N
         S=0.
         DO 30 JJ=1,N
            S=S+V(J,JJ)*TMP(JJ)
   30    CONTINUE
         X(J)=S
   40 CONTINUE
      RETURN
      END
