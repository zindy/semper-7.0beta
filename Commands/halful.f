C Semper 6 processing module HALFUL
C
      SUBROUTINE HALFUL
C
C Provides verbs FULL and HALF, interchanging half- and full-plane
C Fourier transforms
C
      LOGICAL SEMOPN,SEMCEN,SEMROW,SEMROWI,SEMTFC,HPL,COMPL
      INTEGER IVALPN,SEMFRM
      INTEGER IB1(256),IB2(256),CLASS,FORM,CROW,OUTP,OUTNC
C
C Packed names
C
      INTEGER NFROM,NTO,NHALFP,NFULLP
      PARAMETER (NFROM=10335,NTO=-601,NHALFP=12852,NFULLP=10452)
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB1,IB1),(RB2,IB2)
C
      INTEGER NCOL,NROW,NLAY,INP,N,L,NCONJ,NC,IS,I2,I
C
      NCOL = NCOLS(LP1)
      NROW = NROWS(LP1)
      CROW = CROWN(LP1)
      NLAY = NLAYS(LP1)
      CLASS = CLASSN(LP1)
      FORM = FORMN(LP1)
      IF (FORM .EQ. NFMBYT) FORM = NFMINT
      COMPL = FORM .EQ. NFMCOM
C
C Check origin
C
      IF (CLASS.NE.NCLFOU .AND. CLASS.NE.NCLSPE) THEN
         ERROR = 6
         GOTO 60
      ENDIF
C
      IF (SEMTFC(LP1,HPL)) GOTO 40
C
      IF (HPL) THEN
         NCOL = 2*(NCOL-1)
         IF (VERB .EQ. NHALFP) GOTO 50
      ELSE
         IF (VERB .EQ. NFULLP) GOTO 50
      ENDIF
C
C Establish input and output offsets
C
      INP = 1
      OUTP = NCOL/2 + 1
      OUTNC = OUTP
      IF (HPL) THEN
         INP = OUTP
         OUTP = 1
         OUTNC = NCOL
      ENDIF
      IF (COMPL) THEN
         INP = 2*INP - 1
         OUTP = 2*OUTP - 1
      ENDIF
C
C Open output
C
      LP2 = LP1
      N = IVALPN(NTO)
      IF (SEMOPN(2,N,OUTNC,NROW,NLAY,CLASS,SEMFRM(FORMN(LP1)),LP2))
     +   GOTO 40
      IF (.NOT.HPL) THEN
         IF (SEMCEN(LP2,1,CROW,NLAY/2+1)) GOTO 40
      ENDIF
C
C Pass through picture, loading rows in pairs
C
      DO 30 L = 1,NLAYS(LP1)
         DO 20 N = 1,CROW
            NCONJ = MOD(NROW-N+1,NROW) + 1
C
            IF (FORM .EQ. NFMINT) THEN
               IF (SEMROWI(1,IB1(INP),FORM,N,L,LP1)) GOTO 40
               IF (SEMROWI(1,IB2(INP),FORM,NCONJ,L,LP1)) GOTO 40
            ELSE
               IF (SEMROW(1,RB1(INP),FORM,N,L,LP1)) GOTO 40
               IF (SEMROW(1,RB2(INP),FORM,NCONJ,L,LP1)) GOTO 40
            ENDIF
C
C Convert half-plane to full-plane: fill in first half-row from
C far end of conj row
C
            IF (HPL) THEN
               NC = NCOL/2
               IS = 1
               I2 = NCOL + 1
               IF (COMPL) THEN
                  NC = 2*NC
                  IS = 2
                  I2 = 2*I2 - 1
               ENDIF
               DO 10 I = 1,NC,IS
                  IF (FORM .EQ. NFMINT) THEN
                     IB2(I) = IB1(I2)
                     IB1(I) = IB2(I2)
                  ELSE
                     RB2(I) = RB1(I2)
                     RB1(I) = RB2(I2)
                     IF (COMPL) THEN
                        RB2(I+1) = -RB1(I2+1)
                        RB1(I+1) = -RB2(I2+1)
                     ENDIF
                  ENDIF
                  I2 = I2 - IS
   10          CONTINUE
            ELSE
C
C Convert full to half-plane: conj-reverse first col to rhs and output
C from centre of rows
C
               I2 = NCOL + 1
               IF (FORM .EQ. NFMINT) THEN
                 IB1(I2) = IB2(1)
                  IB2(I2) = IB1(1)
               ELSE
                  IF (COMPL) I2 = 2*I2 - 1
                  RB1(I2) = RB2(1)
                  RB2(I2) = RB1(1)
                  IF (COMPL) THEN
                     RB1(I2+1) = -RB2(2)
                     RB2(I2+1) = -RB1(2)
                  ENDIF
               ENDIF
            ENDIF
C
C Return rows to disc
C
            IF (FORM.EQ.NFMINT) THEN
               IF (SEMROWI(2,IB1(OUTP),FORM,N,L,LP2)) GOTO 40
               IF (SEMROWI(2,IB2(OUTP),FORM,NCONJ,L,LP2)) GOTO 40
            ELSE
               IF (SEMROW(2,RB1(OUTP),FORM,N,L,LP2)) GOTO 40
               IF (SEMROW(2,RB2(OUTP),FORM,NCONJ,L,LP2)) GOTO 40
            ENDIF
   20    CONTINUE
   30 CONTINUE
C
C If data real, preserve source range
C
      IF (.NOT.COMPL) WSTAT(LP2) = 0
   40 RETURN
C
C Error(s)
C
   50 ERROR = 63
   60 IDERR = IVALPN(NFROM)
      GOTO 40
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
