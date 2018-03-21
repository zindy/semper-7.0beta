C Semper 6 system module SEMMON
C
      LOGICAL FUNCTION SEMMON(TEXT,MODULE,ICHAN)
C
      CHARACTER*(*) TEXT,MODULE
      INTEGER ICHAN
C
C Outputs a line of text to the monitor output stream.  MODULE passes
C the name of the subroutine calling SEMMON and ICHAN passes the
C monitor channel number.
C
      LOGICAL SEMECH
C
      INCLUDE 'COMMON'
C
      SEMMON=.TRUE.
C
C Output text to physical output streams enabled for log output if
C output to monitor channel ICHAN is enabled
C
      IF (ICHAN.GE.1.AND.ICHAN.LE.NMCHAN) THEN
         IF (MCHANL(ICHAN)) THEN
            IF (SEMECH(NOSMON,TEXT)) GOTO 10
         ENDIF
      ENDIF
C
      SEMMON=.FALSE.
C
   10 RETURN
      IDUMMY = ICHAR(MODULE(1:1))
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
