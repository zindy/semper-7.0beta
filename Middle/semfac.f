C Semper 6 subsidiary module SEMFAC
C
      LOGICAL FUNCTION SEMFAC(N,FACTOR,NF)
C
      INTEGER N,FACTOR(*),NF
C
C Factorises number < 32768, using up to 9 factors of 2,3,4,5
C
C [ret] logical SEMFAC: .true. iff error, i.e. N is not 1 and is
C         not factorisable in 9 factors with at least one factor 4
C [in]  integer N: number to be factorised
C [out] integer FACTOR(NF): factors found
C [out] integer NF: number of factors found
C
C Lists factors in order 2,3,5,4 with no more than one factor 2
C Special case: if N=1, returns .FALSE. with FACTOR(1)=1,NF=1
C
      INTEGER RF,FAC,I,NFMAX
C
      INTEGER TRY(4)
      DATA TRY / 4,5,3,2 /
C
C Trap N<1
C
      IF (N.LT.1) THEN
         NF=0
         SEMFAC=.TRUE.
C
C Trap N=1
C
      ELSE IF (N.EQ.1) THEN
         FACTOR(1)=1
         NF=1
         SEMFAC=.FALSE.
C
C Extract factors in turn
C
      ELSE
         NFMAX=9
         NF=0
         RF=N
         DO 20 I=1,4
            FAC=TRY(I)
   10       IF (MOD(RF,FAC).EQ.0) THEN
C
C Record factor
C
               NF=NF+1
               FACTOR(NF)=FAC
C
C Divide out factor
C
               RF=RF/FAC
C
C Room for any more factors?
C
               IF (NF.LT.NFMAX) THEN
                  GOTO 10
               ELSE
                  GOTO 30
               ENDIF
            ENDIF
   20    CONTINUE
C
C Factorisation succeeded if residual factor=1 and first factor=4
C
   30    SEMFAC=RF.NE.1.OR.FACTOR(1).NE.4
      ENDIF
C
C Reverse order of factors
C
      DO 40 I=1,NF/2
         FAC=FACTOR(I)
         FACTOR(I)=FACTOR(NF+1-I)
         FACTOR(NF+1-I)=FAC
   40 CONTINUE
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
