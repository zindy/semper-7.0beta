C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQSETD ( SOURCE, IOP, I1, I2, I3 )
C       ---------------------------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT - Defines the event source whose data
C                        are to be set.
C
C       integer iop : INPUT - The opertaion to be carried out on the
C                     source
C
C       integer i1, i2, i3 : INPUT - Data on the source.
C
C       eqgetd is used to set information about the given source.
C       The source is selected via SOURCE.  eqgetd returns TRUE if any
C       argument is invalid or some unexpected error is encountered.
C       I1, I2, I3 are used to set information about the physical
C       source as follows:
C
C       For BREAK:
C          NO ACTION
C
C       For KEYBOARD:
C          NO ACTION
C
C       For POINTER:
C          If IOP = OSETL
C             I1 contains the new pointer lock state (0=off,1=on).
C          If IOP = OSETG
C             I1 contains the new pointer X gearing.
C             I2 contains the new pointer Y gearing.
C          If IOP = OSETS
C             I1 contains the new pointer X sensitivity.
C             I2 contains the new pointer Y sensitivity.
C
C          A gearing or sensitivity value of -1 is assumed to mean
C          'leave unchanged'.
C
C       For SWITCHES:
C          NO ACTION
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQSETD ( SOURCE, IOP, I1, I2, I3 )
C     ===================================================
C
      INTEGER SOURCE
      INTEGER IOP
      INTEGER I1, I2, I3
C
      INCLUDE 'EVENTS'
      INCLUDE 'EQCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Switch on the type of source.
C
      IDUMMY = I3
      IF ( SOURCE .EQ. MBREAK ) THEN
         STATUS = .FALSE.
      ELSE IF ( SOURCE .EQ. MKEY ) THEN
         STATUS = .FALSE.
      ELSE IF ( SOURCE .EQ. MPOINT ) THEN
         IF ( IOP .EQ. OSETL ) THEN
C
C           Set pointer lock
C
            STATUS = .FALSE.
         ELSE IF ( IOP .EQ. OSETG ) THEN
C
C           Set pointer gearing
C
            IF ( I1 .NE. -1 ) THEN
               PXGEAR = I1
            ENDIF
            IF ( I2 .NE. -1 ) THEN
               PYGEAR = I2
            ENDIF
         ELSE IF ( IOP .EQ. OSETS ) THEN
C
C           Set pointer sensitivity
C
            IF ( I1 .NE. -1 ) THEN
               PNTXSN = I1
            ENDIF
            IF ( I2 .NE. -1 ) THEN
               PNTYSN = I2
            ENDIF
         ELSE
C
C           Unknown operation
C
            STATUS = .TRUE.
         ENDIF
      ELSE IF ( SOURCE .EQ. MBUT ) THEN
         STATUS = .FALSE.
      ELSE
C
C        Unknown source
C
         STATUS = .TRUE.
      ENDIF
C
C     All done
C
      EQSETD = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
