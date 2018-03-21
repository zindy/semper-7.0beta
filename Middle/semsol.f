C Semper 6 system module SEMSOL
C
      LOGICAL FUNCTION SEMSOL()
C
C Generate sequence of carriage-control characters to ensure the start
C of a new line for subsequent terminal output
C
      LOGICAL UIFSSU,SEMTCR,SEMTLF
C
      INCLUDE 'COMMON'
C
      SEMSOL = .TRUE.
C
C If UIF is running, reset cursor within scrolling window
C
      IF (UIF .NE. 0.) THEN
         IF (UIFSSU()) GOTO 10
      ENDIF
C
C Output CR if one has not already been output
C
      IF (.NOT.TERCR) THEN
         IF (SEMTCR()) GOTO 10
      ENDIF
C
C Output LF if one has not already been output
C
      IF (.NOT.TERLF) THEN
         IF (SEMTLF()) GOTO 10
      ENDIF
C
C Reset CR and LF flags
C
      TERCR = .FALSE.
      TERLF = .FALSE.
C
      SEMSOL = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
