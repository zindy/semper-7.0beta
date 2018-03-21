C Semper 6 system module SEMFIL
C
      LOGICAL FUNCTION SEMFIL(DEVICE,TEXT,N)
C
      INTEGER DEVICE,N
      CHARACTER*(*) TEXT
C
C Outputs a line of N characters to the log file assigned as Semper
C device DEVICE.  If a maximum record length is defined for the file,
C the text is truncated on output as required.
C
      LOGICAL FILCIO
C
      INTEGER NCHA
C
      INCLUDE 'COMMON'
C
C Determine number of characters to output according to maximum
C record length (if any)
C
      IF (DVWID(DEVICE).GT.0) THEN
         NCHA=MIN(N,DVWID(DEVICE))
      ELSE
         NCHA=N
      ENDIF
C
C Output text to the log file
C
      SEMFIL=FILCIO(DVHAN(DEVICE),2,TEXT,NCHA)
C
      RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
