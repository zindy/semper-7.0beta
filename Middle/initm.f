C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION INITM ( )
C       --------------------------
C
C       PARAMETERS:
C
C       Initialises the dynamic memory system for use.  Sets all the
C       store available to char(0), and initialises local tables.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION INITM ( )
C     ==========================
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
      INCLUDE 'STORECOM'
      INCLUDE 'LOGINDEX'
C
C     LOCAL VARIABLES:
C
C     Loop counter
C
      INTEGER I
C
C     Check we are not alrady initialised
C
      IF ( .NOT. ISINIT ) THEN
C
C        Initialise the highest logical and physical indices used
C
         HILIND = 0
         HIPIND = 1
C
C        Set all store to zeros
C
         DO 10 I = 1, MXSTOR
            CSTORE(I:I) = CHAR( 0)
   10    CONTINUE
C
C        Record the fact we are initialised
C
         ISINIT = .TRUE.
         INITM = .FALSE.
      ELSE
C
C        Error - system already initialised
C
         UIFERR = MEMISI
         INITM = .TRUE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
