C       SUBROUTINE UVJUST
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF JUSTIFICATION verb.  Allows the definition
C       of the positioning point for all panels, elements etc.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVJUST
C     =================
C
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Function return status
C
      LOGICAL STATUS
C
C     Positioning point
C
      INTEGER HORPOS, VERPOS
C
C     Packed names for keys and options etc.
C
      INTEGER XTOP, XLEFTP, XBOTTO, XRIGHT
C
      PARAMETER (XTOP=-617,XLEFTP=19406,XBOTTO=3820,XRIGHT=29167)
C
C     CALLED FUNCTIONS:
C
C     Checks for conflicting options
C
      LOGICAL CONOPT
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Sets the positioning point in UIF
C
      LOGICAL UIFIJU
C
C     Check for conflicting options
C
      IF ( CONOPT ( XLEFTP, XRIGHT ) ) THEN
         STATUS = .TRUE.
      ELSE IF ( CONOPT ( XTOP, XBOTTO ) ) THEN
         STATUS = .TRUE.
      ELSE
         STATUS = .FALSE.
      ENDIF
      IF ( .NOT. STATUS ) THEN
C
C        Set the default positioning point
C
         HORPOS = CENTRE
         VERPOS = CENTRE
C
C        Get the options, and act on them
C
         IF ( OPT ( XLEFTP ) ) THEN
            HORPOS = LEFT
         ENDIF
         IF ( OPT ( XRIGHT ) ) THEN
            HORPOS = RIGHT
         ENDIF
         IF ( OPT ( XTOP ) ) THEN
            VERPOS = TOP
         ENDIF
         IF ( OPT ( XBOTTO ) ) THEN
            VERPOS = BOTTOM
         ENDIF
C
C        Set the positioning point
C
         STATUS = UIFIJU ( HORPOS, VERPOS )
         IF ( STATUS ) CALL UIFAPE
      ENDIF
C
C     All done.
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
