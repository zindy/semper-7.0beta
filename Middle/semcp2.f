C Semper 6 system module SEMCP2
C
      LOGICAL FUNCTION SEMCP2 (M,N)
      INTEGER M,N
C
C Returns .FALSE. iff args are powers of 2 and at least 4
C (except that N is ignored if equal to 1)
C Multiplication of M and N might overflow and so is avoided
C
      INCLUDE 'COMMON'
C
      INTEGER I,J,IARG
C
      SEMCP2 = .FALSE.
C
      IARG = 1
      J = M
   10 I = 2
   20 I = I+I
      IF (J .GT. I) GOTO 20
C
      IF (J .LT. I) THEN
         ERROR = 5
         SEMCP2 = .TRUE.
      ELSE IF (IARG .EQ. 1 .AND. N .NE. 1) THEN
         J = N
         IARG = 2
         GOTO 10
      ENDIF
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
