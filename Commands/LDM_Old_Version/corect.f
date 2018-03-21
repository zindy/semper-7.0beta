C Semper 6 processing module CORECT
C
      SUBROUTINE CORECT
C
C Correct modulus of picture in LP1 to match reference modulus in LP3
C
      INTEGER IVALPN
      LOGICAL SEMROW,SEMLU,SEMCON,OPT
C
      INCLUDE 'COMMON'
C
      REAL D,DM,E,FACT,RM,SM
      INTEGER I,J,NCOL,NROW
C
C Packed names
C
      INTEGER NWITH,NE,NFROM,NVERIF
      PARAMETER (NWITH=-5181, NE=8000, NFROM=10335, NVERIF=-3419)
C
C Fault multi-layer source or output picture
C
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 40
      ENDIF
C
C Fault differing picture sizes
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      IF (NCOLS(LP3).NE.NCOL .OR. NROWS(LP3).NE.NROW .OR.
     +    NLAYS(LP3).NE.1) THEN
         ERROR=5
         IDERR=IVALPN(NWITH)
         GOTO 40
      ENDIF
C
C Correct modulus of source picture
C
      E=0.0
      D=0.0
      DO 20 J=1,NROW
C
C Read source row from LP1 and reference modulus from LP3
C
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 40
         IF (SEMROW(1,RB3,NFMCOM,J,1,LP3)) GOTO 40
C
C Correct data for this row using reference modulus
C
         DO 10 I=1,2*NCOL,2
C
C Determine source modulus
C
            SM=SQRT(RB1(I)*RB1(I)+RB1(I+1)*RB1(I+1))
C
C Determine reference modulus
C
            IF (RB3(I+1).EQ.0.0) THEN
               RM=ABS(RB3(I))
            ELSE
               RM=SQRT(RB3(I)*RB3(I)+RB3(I+1)*RB3(I+1))
            ENDIF
C
C Determine error measure
C
            DM=SM-RM
            D=D+DM*DM
            E=E+RM*RM
C
C Correct source modulus if non-zero
C
            IF (SM.NE.0.0) THEN
               FACT=RM/SM
               RB1(I)=FACT*RB1(I)
               RB1(I+1)=FACT*RB1(I+1)
            ENDIF
   10    CONTINUE
C
C Store result in LP2
C
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) GOTO 40
   20 CONTINUE
C
C Set variable E to error measure
C
      E=D/E
      IF (SEMLU(1,NE,E)) GOTO 40
C
C If VERIFY option is set, print error measure on console
C
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,30) 100.0*E
   30    FORMAT ('Fractional sum-square difference',G12.4,'%')
         IF (SEMCON(RECORD)) GOTO 40
      ENDIF
C
   40 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
