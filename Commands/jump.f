C Semper 6 processing module JUMP
C---------------------------------------------------------------------
C
C      SUBROUTINE JUMP
C      ---------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command JUMP
C
C---------------------------------------------------------------------
C
      SUBROUTINE JUMP
C     ===============
C
      INTEGER IVAL
      LOGICAL SEMENV,SEMJMP,SEMUNW,SEMXA1
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      REAL X
      INTEGER IPTR,NAME
      INTEGER LABNUM,LABPTR,LABENT
      PARAMETER (LABENT=4)
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) THEN
         ERROR = 17
         GOTO 20
      ENDIF
C
C JUMP processing
C
      IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,NAME)) CONTINUE
      IF (ERROR .NE. 0) GOTO 20
C
   10 IF (FORLEV .GT. INPFOR(INPLEV)) THEN
C
C In FOR loop - scan between beginning and end of loop
C
         IF (SEMJMP(NAME,LABNUM,FORFSL(FORLEV)+1,.TRUE.)) THEN
C
C Not found - unstack loop
C
            IF (SEMUNW(FORLOC(FORLEV))) GOTO 20
            FORLEV = FORLEV - 1
            GOTO 10
         ENDIF
      ELSE
C
C Not in FOR loop - scan whole range
C
         IF (SEMJMP(NAME,LABNUM,0,.TRUE.)) THEN
C
C Not found - check if label present at all
C
            IF (SEMJMP(NAME,LABNUM,0,.FALSE.)) THEN
C
C Not found at all - complain unless in a run file
C
               IF (INPUT .EQ. TERM1 .OR. INPLEV .NE. 0) THEN
                  ERROR = 7
                  IDERR = NAME
               ELSE
                  JMPDST = NAME
                  LINLEN = 0
               ENDIF
C
C (Rewind offline units so that END trap will work again)
C
               IF (INPUT .NE. TERM1) REWIND INPUT
               GOTO 20
            ELSE
C
C Attempting to jump into a FOR loop !!
C
               ERROR = 112
               IDERR = NAME
               GOTO 20
            ENDIF
         ENDIF
      ENDIF
C
C Found label - set environment accordingly
C
      LABPTR = LABNUM*LABENT
      IF (SEMENV(LINDEV,LINSLT,LINDEX(LABPTR+2))) GOTO 20
      LASTSC = LINDEX(LABPTR+3) - 1
   20 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
