C Semper 6 system module FSELEC
C
      LOGICAL FUNCTION FSELEC()
C
C If frame coordinates, set variables FS and CFRAME.
C If partition/picture coordinates, set variable DISPLA.
C The information must previously have been set up by a call to FSINIT.
C
      INCLUDE 'COMMON'
C
      FSELEC=.TRUE.
C
C Check for frame coordinates as opposed to partition/picture coords.
C
      IF (FSPAR.EQ.0) THEN
         FS=REAL(FSDEV)
         CFRAME=REAL(FSFRA)
      ELSE
         DISPLA=REAL(1000*FSDEV+FSPAR)
      ENDIF
C
      FSELEC=.FALSE.
C
      RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
