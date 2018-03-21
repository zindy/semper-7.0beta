      SUBROUTINE PAUSES
C
C  Exerciser for routine WAITS
C
C  SYNTAX   Pause :PAUSES
C
      INCLUDE 'COMMON'
      LOGICAL SEMCON,SEMWAI,SEMBRK
      INTEGER IERROR
C
      IF(SEMCON(' -- Exerciser for WAITS and SEMWAI -- ')) RETURN
      IF(SEMCON(' ')) RETURN
      IF(SEMCON('Waiting for 2 seconds (WAITS) ')) RETURN
      CALL WAITS(2.0)
C
      IF(SEMCON(' ')) RETURN
      IF(SEMCON('Now waiting for 10 seconds (WAITS) ')) RETURN
      IF(SEMCON('try to use <control>-C to interrupt')) RETURN
      CALL WAITS(10.0)
      IERROR=ERROR
C
C  clear break queue and error status (in that order!)
C
      IF(SEMBRK()) THEN
          IF(ERROR .NE. 4) RETURN
          ERROR=0
      ENDIF
C
C  ..reflect error passed back by WAITS
C
      WRITE(RECORD,10) IERROR
   10 FORMAT('WAITS returns with ERROR set to ',I4)
      IF(SEMCON(RECORD)) RETURN
C
      IF(SEMCON(' ')) RETURN
      IF(SEMCON('Now waiting for 1000 seconds using SEMWAI')) RETURN
      IF(SEMCON('try to use <control>-C to interrupt')) RETURN
C
C  wait intelligently
C
      IF(SEMWAI(1000.0)) THEN
C
C  reflect error returned by SEMWAI
C
         IERROR=ERROR
         ERROR=0
         WRITE(RECORD,20) IERROR
   20    FORMAT('SEMWAI returns .TRUE. with ERROR set to ',I4)
         IF(SEMCON(RECORD)) RETURN
      ENDIF
C
      RETURN
      END
