C Semper 6 processing module EDSTUB
C
      SUBROUTINE EDSTUB
C
C Decides if LIST command is for a MACRO or a PROGRAM
C This will disappear when PROGRAM EDIT is written and numbered
C macros are removed.
C
      LOGICAL VARSET
      INTEGER NNAME,NDEVIC,NPROGR,NALL
C
C Packed names
C
      PARAMETER (NNAME=22453,NDEVIC=6622,NPROGR=26335,NALL=2092)
C
      IF (VARSET(NDEVIC) .OR. VARSET(NNAME) .OR.
     +    VARSET(NPROGR) .OR. VARSET(NALL)) THEN
         CALL PRDWRT
      ELSE
         CALL EDITOR
      ENDIF
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
