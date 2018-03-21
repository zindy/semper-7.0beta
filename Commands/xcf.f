C Semper 6 processing module XCF
C
      SUBROUTINE XCF
C
C Places xcf of LP1 and LP3 in TO; if LP3 is not FOURIER,
C its transform is left in VIA
C
      LOGICAL SEMOPN,SEMROW,SEMTFC,SEMLU,FT2D,INVFT2,FINDCM
      LOGICAL MARSET,FSINIT,FSMARK,FSCIRC,ANNOT,HPL,OPT,OPTNO,SEMCON
      INTEGER IVALPN
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NSEARC,NX,NY,NT,NRADIU,NTO,NVIA,NVERIF
      PARAMETER (NSEARC=30601,NX=-6401,NY=-8001,NT=-1,NRADIU=28844)
      PARAMETER (NTO=-601,NVIA=-3562,NVERIF=-3419)
C
      INTEGER LPS(2),LPFT(2),IPNN(2),CLASS,CROW
C
      INTEGER LPERR,ITO,NCOL,NROW,LPTO,NC2,NCIP,MARK
      REAL V1,V2,T1,T2,COPIES,P,Q,R,S,RNORM,RADIUS
      REAL XCFVAL,X,Y
      INTEGER N, I, J
C
      DATA IPNN/NTO,NVIA/
C
      LPS(1)=LP1
      LPS(2)=LP3
C
C Transform FROM,WITH in turn to TO,VIA
C
      DO 10 N=1,2
C
C If source pictures identical, omit any second transform
C
         IF (LPS(1).EQ.LPS(2).AND.N.EQ.2) THEN
            LPFT(2)=LPFT(1)
         ELSE
            LPFT(N)=LPS(N)
            ITO=IVALPN(IPNN(N))
            CLASS=CLASSN(LPS(N))
            IF (CLASS.EQ.NCLIMA) THEN
               LP1=LPS(N)
               IF (FT2D(ITO,NCLFOU,NFMCOM,.FALSE.,.FALSE.)) GOTO 50
               LPFT(N)=LP2
            ELSE IF (CLASS.EQ.NCLFOU) THEN
               IF (SEMTFC(LPS(N),HPL)) GOTO 50
C
               IF (.NOT.HPL) THEN
                  ERROR=63
                  LPERR=LPS(N)
                  GOTO 60
               ENDIF
            ELSE
               ERROR=6
               LPERR=LPS(N)
               GOTO 60
            ENDIF
         ENDIF
   10 CONTINUE
C
C Check sizes match now (both pics are in same space)
C
      NCOL=NCOLS(LPFT(1))
      NROW=NROWS(LPFT(1))
C
      IF (NCOLS(LPFT(2)).NE.NCOL.OR.NROWS(LPFT(2)).NE.NROW) THEN
         ERROR=5
         LPERR=LPS(2)
         GOTO 60
      ENDIF
C
C Open ITO to receive cross product for back transformation
C
      ITO=IVALPN(NTO)
      LPTO=LPFT(1)
      IF (SEMOPN(2,ITO,NCOL,NROW,1,NCLCOR,NFMCOM,LPTO)) GOTO 50
C
C Pass through transform rows
C
      NC2=2*NCOL
      NCIP=NC2-2
      CROW=NROW/2+1
      V1=0.
      V2=0.
      DO 40 J=1,NROW
         IF (SEMROW(1,RB1,NFMCOM,J,1,LPFT(1))) GOTO 50
         IF (SEMROW(1,RB2,NFMCOM,J,1,LPFT(2))) GOTO 50
C
C Zero centre pixels
C
         IF (J.EQ.CROW) THEN
            RB1(1)=0.
            RB2(1)=0.
         ENDIF
C
C Pass along row
C
         T1=0.
         T2=0.
         I=1
C
C To accumulate pixel variance, take two copies of all pixels except
C first,last column
C
   20    COPIES=1.
   30    P=RB1(I)
         Q=RB1(I+1)
         R=RB2(I)
         S=RB2(I+1)
C
C Accumulate variance
C
         T1=T1+COPIES*(P*P+Q*Q)
         T2=T2+COPIES*(R*R+S*S)
C
C Replace first xform by conj times second
C
         RB1(I)=P*R+Q*S
         RB1(I+1)=P*S-Q*R
         COPIES=2.
         I=I+2
         IF (I.LT.NCIP) GOTO 30
         IF (I.LT.NC2) GOTO 20
C
C Accumulate row variance
C
         V1=V1+T1
         V2=V2+T2
         IF (SEMROW(2,RB1,NFMCOM,J,1,LPTO)) GOTO 50
   40 CONTINUE
C
      IF (V1.EQ.0.0) THEN
         ERROR=12
         LPERR=LPFT(1)
         GOTO 60
      ENDIF
C
      IF (V2.EQ.0.0) THEN
         ERROR=12
         LPERR=LPFT(2)
         GOTO 60
      ENDIF
C
      RNORM=2./SQRT(V1*V2)
C
C Invert transform
C
      LP1=LPTO
      IF (INVFT2(ITO,NCLCOR,.FALSE.,.FALSE.,RNORM)) GOTO 50
C
C See if SEARCH option is set
C
      IF (.NOT.OPTNO(NSEARC)) THEN
C
C Prepare display marking
C
         IF (MARSET(ANNOT,MARK)) THEN
            IF (FSINIT(3,MARK)) GOTO 50
            ANNOT=FSPTYP.EQ.1
         ENDIF
C
C Search full picture for highest/lowest point
C
         IF (FINDCM(LP2,.FALSE.,.FALSE.,0.,0.,0.,XCFVAL,X,Y)) GOTO 50
C
C If RADIUS set, find local cm
C
         IF (SEMLU(-1,NRADIU,RADIUS)) THEN
            IF (ANNOT) THEN
               IF (FSCIRC(X,Y,RADIUS)) GOTO 50
            ENDIF
C
            IF (FINDCM(LP2,.TRUE.,.TRUE.,X,Y,RADIUS,XCFVAL,P,Q)) GOTO 50
C
            X=P
            Y=Q
         ENDIF
C
         IF (ANNOT) THEN
            IF (FSMARK(X,Y,FSMMOD,FSMSIZ)) GOTO 50
         ENDIF
C
C Return peak position and value
C
         IF (SEMLU(1,NX,X)) GOTO 50
         IF (SEMLU(1,NY,Y)) GOTO 50
         IF (SEMLU(1,NT,XCFVAL)) GOTO 50
C
C If VERIFY option is set, print results
C
         IF (OPT(NVERIF)) THEN
            WRITE (RECORD,70) XCFVAL,X,Y
            IF (SEMCON(RECORD)) GOTO 50
         ENDIF
      ENDIF
C
   50 RETURN
C
C Common setting of error parameter
C
   60 IDERR=1000*DEVN(LPERR)+PICN(LPERR)
      GOTO 50
C
   70 FORMAT ('Peak value ',F8.3,' position ',2F7.2)
C
C Copyright (C) 1987,1989,1992:  Synoptics Ltd,  All Rights Reserved
C
      END
