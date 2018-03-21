C Semper 6 system module SEMSEL
C
      LOGICAL FUNCTION SEMSEL(LPN)
C
C Simple picture selection routine.  Does exactly as it is told,
C storing the picture number corresponding to LPN for later use by
C the command interpreter to set SELECT.  If LPN = 0, automatic
C picture selection by the command interpreter is disabled
C
      INTEGER LPN
C
      INCLUDE 'COMMON'
C
      SEMSEL=.TRUE.
C
C Disable resetting of SELECT if LPN = 0
C
      IF(LPN.EQ.0) THEN
         PICSEL=-1
C
C Otherwise, check for valid LP number and record picture number for
C selection at the end of the current command
C
      ELSE
C
C Fault bad LP number
C
         IF(LPN.LT.0.OR.LPN.GT.NLPS) GOTO 20
C
C Fault picture opened before this command or non-existent picture
C
         IF(BASEWH.GT.WHEN(LPN)) GOTO 20
C
C Everything is O.K. - store corresponding picture number for setting
C                      SELECT on return to the command interpreter
C
         PICSEL=1000*DEVN(LPN)+PICN(LPN)
      ENDIF
C
      SEMSEL=.FALSE.
C
   10 RETURN
C
C Error detected
C
   20 ERROR=77
      IDMESS='Bad logical picture number in call to SEMSEL'
      GOTO 10
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
