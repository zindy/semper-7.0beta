C Semper 6 subsidiary module SHOW8
C
      LOGICAL FUNCTION SHOW8(NAMES,N)
      INTEGER N
      INTEGER NAMES(N)
C
      LOGICAL SEMCON,SEMTPS
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      CHARACTER*3 UNPACKF
C
      INCLUDE 'COMMON'
C
      INTEGER I,J,K,IWID,ITEMS,ITEM1
C
      CHARACTER*(RECLEN) OUTREC
C
      SHOW8 = .TRUE.
C
C Sort list of names
C
      CALL SHOW5(NAMES,N)
C
      OUTREC = ' '
      IF (SEMTPS(IWID,I)) GOTO 30
C
C Get at least one item per line ?
C
      IF (IWID .LT. 6) IWID = 6
C
C Limit to the size of our string
C
      IF (IWID .GT. RECLEN) IWID = RECLEN
      IWID = IWID - 1
      ITEMS = IWID / 5
      ITEM1 = ITEMS - 1
C
      DO 20 I = 1,N,ITEMS
         K = 1
         DO 10 J = I,MIN(I+ITEM1,N)
            K = K + 5
            OUTREC(K-2:K) = UNPACKF(NAMES(J))
   10    CONTINUE
         IF (SEMCON(OUTREC(1:K))) GOTO 30
   20 CONTINUE
C
      SHOW8 = .FALSE.
   30 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
