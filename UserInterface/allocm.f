C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION ALLOCM ( NBYTES, LINDEX )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer nbytes : INPUT - Number of bytes to be allocated
C
C       integer lindex : OUTPUT - Logical index to block of memory
C                                 allocated
C
C       Returns lindex as a logical index to the required number of
C       bytes of memory.  To access this memory, a call to pindex will
C       return the physical index into /storecom/cstore which can then
C       be used direct.  The logical to physical translation should
C       always be done just before the use of the physical index, as
C       garbage collection may cause logical to physical mappings to
C       change.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION ALLOCM ( NBYTES, LINDEX )
C     ==========================================
C
      INTEGER NBYTES, LINDEX
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
      INCLUDE 'STORECOM'
      INCLUDE 'LOGINDEX'
C
C     CALLED FUNCTIONS:
C
C     Actually allocates space.
C
      LOGICAL ALLOCX
C
C     Compresses store
C
      LOGICAL COMPM
C
      ALLOCM = .TRUE.
C
C     Check system is initialised
C
      IF ( ISINIT ) THEN
C
C        Check for allocation of zero or too much memory - error
C
         IF ( NBYTES .GT. 0 .AND. NBYTES .LE. MXSTOR ) THEN
C
C           See if we can do a straight allocation
C
            ALLOCM = ALLOCX ( NBYTES, LINDEX )
            IF ( ALLOCM ) THEN
C
C              It failed.  Compress the memory, and try again
C
               ALLOCM = COMPM ( )
               IF ( .NOT. ALLOCM ) THEN
                  ALLOCM = ALLOCX ( NBYTES, LINDEX )
               ENDIF
            ENDIF
         ELSE
C
C           Error - request for invalid memory size
C
            UIFERR = MEMBSI
         ENDIF
      ELSE
C
C         Error - system not initialised
C
          UIFERR = MEMINI
      ENDIF
C
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
