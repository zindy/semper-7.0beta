C Semper 6 processing module SEMNXT
C---------------------------------------------------------------------
C
C      SUBROUTINE SEMNXT
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles commands BREAK and NEXT
C
C---------------------------------------------------------------------
C
      SUBROUTINE SEMNXT
C     =================
C
      INTEGER IVAL
      LOGICAL SEMENV,SEMUNW,SEMXA1
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR,NBREAK
      PARAMETER (NDOLLR=-11201,NBREAK=3925)
C
      REAL X
      INTEGER IPTR,NAME
      INTEGER LABNUM,LABPTR,LABENT
      PARAMETER (LABENT=4)
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) THEN
         NAME = 0
      ELSE
C
C Read a name if present
C
         IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,NAME)) CONTINUE
         IF (ERROR .NE. 0) GOTO 20
      ENDIF
C
C BREAK/NEXT processing
C
   10 IF (FORLEV .GT. INPFOR(INPLEV)) THEN
         IF (NAME .NE. 0 .AND. NAME .NE. FORVAR(FORLEV)) THEN
            IF (SEMUNW(FORLOC(FORLEV))) GOTO 20
            FORLEV = FORLEV - 1
            GOTO 10
         ENDIF
      ELSE
C
C Not found or mismatched
C
         ERROR = 110
         GOTO 20
      ENDIF
C
C If break then clear counter
C
      IF (VERB .EQ. NBREAK) FORCNT(FORLEV) = 0
C
C Re-establish environment to point to LOOP
C
      LABNUM = FORLSL(FORLEV)
      LABPTR = LABNUM*LABENT
      IF (.NOT.SEMENV(FORDEV(FORLEV),FORSLT(FORLEV),
     +                LINDEX(LABPTR+2))) LASTSC = LINDEX(LABPTR+3)-1
C
   20 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
