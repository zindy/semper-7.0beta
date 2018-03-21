C Semper 6 processing module SEMLOO
C---------------------------------------------------------------------
C
C      SUBROUTINE SEMLOO
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command LOOP
C
C---------------------------------------------------------------------
C
      SUBROUTINE SEMLOO
C     =================
C
      INTEGER IVAL
      LOGICAL SEMENV,SEMLU,SEMUNW,SEMXA1
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      REAL X
      INTEGER IPTR,NAME
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) THEN
         NAME = 0
      ELSE
C
C Read a name if present
C
         IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,NAME)) CONTINUE
         IF (ERROR .NE. 0) GOTO 10
      ENDIF
C
C LOOP processing
C
      IF (FORLEV .EQ. INPFOR(INPLEV)) THEN
         ERROR = 111
         GOTO 10
      ELSE IF (NAME .NE. 0 .AND. NAME .NE. FORVAR(FORLEV)) THEN
         ERROR = 105
         GOTO 10
      ENDIF
C
      IF (FORCNT(FORLEV) .GT. 0) THEN
         FORCNT(FORLEV) = FORCNT(FORLEV) - 1
         X = FOREND(FORLEV)
         IF (FORCNT(FORLEV) .NE. 0) THEN
            X = X - (FORCNT(FORLEV) * FORSTP(FORLEV))
         ENDIF
C
C Set FOR variable (as local, temporary VRBLEV unwind to find var)
C
         NAME = VRBLEV
         VRBLEV = FORLOC(FORLEV)
         IF (SEMLU(2,FORVAR(FORLEV),X)) THEN
            VRBLEV = NAME
            GOTO 10
         ELSE
            VRBLEV = NAME
         ENDIF
C
C Restore environment
C
         IF (SEMENV(FORDEV(FORLEV),
     +              FORSLT(FORLEV),FORLIN(FORLEV))) GOTO 10
         LASTSC = FOROFF(FORLEV)
      ELSE
         IF (SEMUNW(FORLOC(FORLEV))) CONTINUE
         FORLEV = FORLEV - 1
      ENDIF
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
