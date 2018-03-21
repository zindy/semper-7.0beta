C Semper 6 system module FSBORD
C
      LOGICAL FUNCTION FSBORD()
C
C Draws border limits
C
      LOGICAL FSLINE
C
      INCLUDE 'COMMON'
C
      FSBORD=.TRUE.
C
C Draw lines round four sides of graphics border
C
      IF (FSLINE(FSBLEF,FSBBOT,FSBRIG,FSBBOT)) GOTO 10
      IF (FSLINE(FSBRIG,FSBBOT,FSBRIG,FSBTOP)) GOTO 10
      IF (FSLINE(FSBRIG,FSBTOP,FSBLEF,FSBTOP)) GOTO 10
      IF (FSLINE(FSBLEF,FSBTOP,FSBLEF,FSBBOT)) GOTO 10
C
      FSBORD=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
