C Semper 6 subsidiary module SETPPR
C
      LOGICAL FUNCTION SETPPR ( )
C
C     ===========================
C
C
C     Set up the paging prompt depending on whether there are mouse
C     buttons or not.
C
      LOGICAL EQGETD
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INTEGER NOBUTS, IDUM
C
C     See how many mouse buttons there are
C
      SETPPR = .TRUE.
C
      IF ( EQGETD ( MBUT, NOBUTS, IDUM, IDUM ) ) THEN
         ERROR = 161
         GOTO 10
      ENDIF
C
C     Construct the page prompt string depending on how many mouse
C     buttons we have
C
      IF (NOBUTS.LT.1) THEN
         TERSTR(1:13)='SPACE => page'
         TERPLN=13
         TERLBN=0
      ELSE
         IF (NOBUTS.EQ.1) THEN
            TERSTR(1:23)='SPACE or BUTTON => page'
            TERPLN=23
         ELSE
            TERSTR(1:21)='SPACE or LEFT => page'
            TERPLN=21
         ENDIF
         TERLBN=1
      ENDIF
C
      IF (NOBUTS.LT.2) THEN
         TERSTR(TERPLN+1:TERPLN+16)=', RETURN => line'
         TERPLN=TERPLN+16
         TERPBN=0
      ELSE
         TERSTR(TERPLN+1:TERPLN+25)=', RETURN or RIGHT => line'
         TERPLN=TERPLN+25
         TERPBN=2
      ENDIF
C
      IF (TERQUI) THEN
         IF (NOBUTS.LT.3) THEN
            TERSTR(TERPLN+1:TERPLN+12)=', Q => quit.'
            TERPLN=TERPLN+12
            TERQBN=0
         ELSE
            TERSTR(TERPLN+1:TERPLN+22)=', Q or MIDDLE => quit.'
            TERPLN=TERPLN+22
            TERPBN=3
            TERQBN=2
         ENDIF
      ELSE
         TERPLN=TERPLN+1
         TERSTR(TERPLN:TERPLN)='.'
         IF (TERPBN.EQ.2.AND.NOBUTS.GE.3) TERPBN=3
         TERQBN=0
      ENDIF
C
      SETPPR = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
