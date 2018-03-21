C Semper 6 system module FSFLUS
C
      LOGICAL FUNCTION FSFLUS()
C
C Resets the graphics buffer flush flag REQFSF and calls for the
C graphics buffer to be flushed.
C
      LOGICAL FSFL61
C
      INCLUDE 'COMMON'
C
C Reset graphics buffer flush flag
C
      REQFSF=.FALSE.
C
C Request for graphics buffer to be flushed, i.e. for its contents to be
C transmitted through to the display hardware
C
      FSFLUS=FSFL61(1,ERROR)
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
