C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION PINDEX ( LINDEX, IND )
C       ---------------------------------------
C
C       PARAMETERS:
C
C       integer lindex : INPUT - Logical index to block of memory
C
C       integer ind : OUTPUT - Physical index to block of memory
C                              pointed to by lindex
C
C       Returns in ind the physical index corresponding to logical
C       index lindex.  Ind may then by used to access
C       /storecom/cstore direct.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION PINDEX ( LINDEX, IND )
C     =======================================
C
      INTEGER LINDEX, IND
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
      INCLUDE 'STORECOM'
      INCLUDE 'LOGINDEX'
C
      PINDEX = .TRUE.
C
C       Check system is initialised
C
      IF ( ISINIT ) THEN
C
C        Check to see if the logical index is valid
C
         IF ( LINDEX .GT. 0 .AND. LINDEX .LE. HILIND ) THEN
C
C           Now check it is in use
C
            IF ( LITAB(LINDEX) .GT. 0 .AND. LSTAB(LINDEX) .GT. 0 ) THEN
               IND    = LITAB(LINDEX)
               PINDEX = .FALSE.
            ELSE
C
C              Error - invalid logical index
C
               UIFERR = MEMBLI
            ENDIF
         ELSE
C
C           Error - invalid logical index
C
            UIFERR = MEMBLI
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
