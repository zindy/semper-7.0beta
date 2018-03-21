C Semper 6 subsidiary module SHOW4
C
      SUBROUTINE SHOW4(ECHFLG,STRING,N)
C
C Encodes current ECHO options for terminal and log files
C
      LOGICAL ECHFLG(6)
      INTEGER N
      CHARACTER*(*) STRING
C
      STRING(1:3)='   '
      N=3
C
      IF (ECHFLG(1)) THEN
         STRING(N+1:N+9)='  console'
         N=N+9
      ENDIF
C
      IF (ECHFLG(2)) THEN
         STRING(N+1:N+13)='  diagnostics'
         N=N+13
      ENDIF
C
      IF (ECHFLG(3)) THEN
         STRING(N+1:N+5)='  log'
         N=N+5
      ENDIF
C
      IF (ECHFLG(4)) THEN
         STRING(N+1:N+9)='  monitor'
         N=N+9
      ENDIF
C
      IF (ECHFLG(5)) THEN
         STRING(N+1:N+10)='  commands'
         N=N+10
      ENDIF
C
      IF (ECHFLG(6)) THEN
         STRING(N+1:N+7)='  input'
         N=N+7
      ENDIF
C
      IF (N.EQ.3) THEN
         STRING(4:9)='  none'
         N=9
      ELSE IF (N.EQ.56) THEN
         STRING(4:8)='  all'
         N=8
      ENDIF
C
C Copyright (C) 1989,1993:  Synoptics Ltd,  All Rights Reserved
C
      END
