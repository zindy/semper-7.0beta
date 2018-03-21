C Semper 6 system module SEMXPL
C
      LOGICAL FUNCTION SEMXPL(TEXT,LENGTH,PTR,V,VLEN)
C
C Reads series of up to VLEN expressions separated by commas,
C resetting VLEN to number found - and insisting on at least one.
C
      INTEGER TEXT(*),LENGTH,PTR,VLEN
      REAL V(*)
C
      LOGICAL SEMEXP,SEMXA1
C
      INTEGER N
      REAL    X
C
      INCLUDE 'COMMON'
C
      SEMXPL=.TRUE.
C
C Advance pointer to next non-space, faulting a blank string
C
      IF (SEMXA1(0,TEXT,LENGTH,PTR,X,N)) THEN
         ERROR=20
         GOTO 20
      ENDIF
C
      N=0
C
C Read next expression
C
   10 IF (N.LT.VLEN) THEN
         IF (SEMEXP(TEXT,LENGTH,PTR,X,.FALSE.)) GOTO 20
C
C Store value in return array
C
         N=N+1
         V(N)=X
C
C Another expected?
C
         IF (PTR.LE.LENGTH) THEN
            IF (TEXT(PTR).EQ.KCOMMA) THEN
               PTR=PTR+1
               GOTO 10
            ENDIF
         ENDIF
      ENDIF
C
C Return number of expressions decoded
C
      VLEN=N
C
      SEMXPL=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
