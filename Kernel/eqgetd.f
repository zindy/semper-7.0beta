C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQGETD ( SOURCE, I1, I2, I3 )
C       ----------------------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT - Defines the event source whose data
C                        are to be returned
C
C       integer i1, i2, i3 : OUTPUT - Data on the source.
C
C       eqgetd is used to return information about the given source.
C       The source is selected via SOURCE.  eqgetd returns TRUE if any
C       argument is invalid or some unexpected error is encountered.
C       I1, I2, I3 are used to return information about the physical
C       source as follows:
C
C       For BREAK:
C          I1 contains zero if BREAK cannot be detected.
C
C       For KEYBOARD:
C          I1 contains the minimum detectable function key number.
C          I2 contains the maximum detectable function key number.
C          I3 contains zero if there are no detectable cursor keys.
C
C       For POINTER:
C          I1 contains a value indication if the pointer is echoed by
C             hardware:
C                ECHNON for no hardware echo
C                ECHDSP for hardware echo on the display
C                ECHHST for hardware echo on the host
C                ECHALL for hardware echo on both
C
C       For SWITCHES:
C          I1 contains the number of switches
C          I2 is zero if switch closure cannot be detected
C          I3 is zero if switch open cannot be detected
C             (If I2 and I3 are both zero, ony 'clicks' are detected)
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQGETD ( SOURCE, I1, I2, I3 )
C     ==============================================
C
      INTEGER SOURCE
      INTEGER I1, I2, I3
C
      INCLUDE 'EVENTS'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Switch on the type of source.
C
      STATUS = .FALSE.
      IF ( SOURCE .EQ. MBREAK ) THEN
         CALL EQXBDA ( I1, I2, I3 )
      ELSE IF ( SOURCE .EQ. MKEY ) THEN
         CALL EQXKDA ( I1, I2, I3 )
      ELSE IF ( SOURCE .EQ. MPOINT ) THEN
         CALL EQXPDA ( I1, I2, I3 )
      ELSE IF ( SOURCE .EQ. MBUT ) THEN
         CALL EQXSDA ( I1, I2, I3 )
      ELSE
C
C        Unknown source
C
         STATUS = .TRUE.
      ENDIF
C
C     All done
C
      EQGETD = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
