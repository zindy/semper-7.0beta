C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION MSPACE ( SIZE )
C       --------------------------------
C
C       PARAMETERS:
C
C       integer size : OUTPUT - Amount of space remaining
C
C       Returns the amount of free memory available, in characters.
C       Will return 0 if either allocated physical store or the logical
C       index table are exhausted.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION MSPACE ( SIZE )
C     ================================
C
      INTEGER SIZE
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
      INTEGER I
C
C     TRUE if a coalesced / free logical index slot is free
      LOGICAL SLOT
C
C     Check system is initialised
C
      IF ( ISINIT ) THEN
C
C        Initialise the free size to the amount of unallocated memory
C
         SIZE = MXSTOR - HIPIND + 1
C
C        Now walk along the logical index table, adding in any free
C        space
C
         SLOT = .FALSE.
         DO 10 I = 1, HILIND
            IF ( LSTAB(I) .LE. 0 ) THEN
               SIZE = SIZE-LSTAB(I)
               SLOT = .TRUE.
            ENDIF
   10    CONTINUE
C
C        Now check if the logical index table is full: if so, no free
C        space
C
         IF ( .NOT. SLOT .AND. HILIND .EQ. MXLTAB ) THEN
            SIZE = 0
         ENDIF
         MSPACE = .FALSE.
      ELSE
C
C         Error - system not initialised
C
         UIFERR = MEMINI
         MSPACE = .TRUE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
