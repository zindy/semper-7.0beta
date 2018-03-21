C Semper 6 processing module CLS
C
      SUBROUTINE CLS
C
C Clears the terminals scrolling area and leaves the cursor at the
C home position
C
      LOGICAL SEMTFL,SEMSOP,UIFSCS
C
C
      INCLUDE 'COMMON'
C
C Flush any outstanding output
C
      IF (SEMTFL()) GOTO 30
C
C See if UIF is running
C
      IF (UIF .NE. 0.) THEN
C
C Clear UIF's scrolling area
C
         IF (UIFSCS()) GOTO 30
      ELSE
C
C Clear the full X11 terminal screen
C
         CALL SX11CT(.TRUE.,.TRUE.)
      ENDIF
C
C Reset the pagination count
C
      IF (SEMSOP()) GOTO 30
C
C Suppress output of any carriage-control characters before the next
C record to be output to the terminal
C
      TERCR = .TRUE.
      TERLF = .TRUE.
C
   30 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
