C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION FREEM ( LINDEX )
C       ---------------------------------
C
C       PARAMETERS:
C
C       integer lindex : INPUT - Logical index to block of memory to be
C                                freed
C
C       Frees the block of memory referred to by logical index lindex.
C       Lindex may not be used again after freem on lindex has been
C       called.  Memory blocks are coalesced if possible for reuse
C       if adjacent blocks are also free.  All freed memory is set to
C       char(0)
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FREEM ( LINDEX )
C     =================================
C
      INTEGER LINDEX
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
C     Physical index of logical index
C
      INTEGER IND
C
C     Size of the block being freed
C
      INTEGER SIZE
C
C     Logical & physical indices of the blocks before and after
C     the one being freed
C
      INTEGER PREVL, NEXTL, NEXTP
C
      FREEM = .TRUE.
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
            IND  = LITAB(LINDEX)
            SIZE = LSTAB(LINDEX)
            IF ( IND .GT. 0 .AND. SIZE .GT. 0 ) THEN
C
C              Yes.  Check if we can coalesce the block with any on
C              either side of it.  First find the logical index of
C              the next block
C
                NEXTL = 0
                NEXTP = IND + SIZE
                DO 10 I = 1, HILIND, ENTSIZ
                   IF ( LSTAB(I) .LT. 0 ) THEN
                      IF ( LITAB(I) .EQ. NEXTP ) THEN
                        NEXTL = I
                        GOTO 20
                      ENDIF
                   ENDIF
   10           CONTINUE
   20           CONTINUE
C
C               And the previous block
C
                PREVL = 0
                DO 30 I = 1, HILIND, ENTSIZ
                   IF ( LSTAB(I) .LT. 0 ) THEN
                      IF ( LITAB(I) - LSTAB(I) .EQ. IND ) THEN
                         PREVL = I
                         GOTO 40
                      ENDIF
                   ENDIF
   30           CONTINUE
   40           CONTINUE
C
C               Anything to coalesce?
C               ---------------------
C
                IF ( PREVL .NE. 0 .OR. NEXTL .NE. 0 ) THEN
C
C                  Yup. Work out the size of the coalesed block
C
                   SIZE = LSTAB(LINDEX)
                   IF ( PREVL .NE. 0 ) SIZE = SIZE-LSTAB(PREVL)
                   IF ( NEXTL .NE. 0 ) SIZE = SIZE-LSTAB(NEXTL)
C
C                  Now decide on the start of the new bit
C
                   IND = LITAB (LINDEX)
                   IF ( PREVL .NE. 0 ) IND = LITAB(PREVL)
C
C                     Now reset the logical indices accordingly.
C                     Following blocks always coalesced away.
C
                   IF ( NEXTL .NE. 0 ) THEN
                      LITAB(NEXTL) = 0
                      LSTAB(NEXTL) = 0
                   ENDIF
C
C                  If there was a previous block, it now is the start,
C                  and is of the new size.  The block we were asked to
C                  free is coalesced away.
C
                   IF ( PREVL .NE. 0 ) THEN
                      LSTAB(PREVL) = -SIZE
                      LITAB(LINDEX) = 0
                      LSTAB(LINDEX) = 0
                   ELSE
C
C                     No previous block, so the block we freed now
C                     is of the new size
C
                      LSTAB(LINDEX) = -SIZE
                   ENDIF
                ELSE
C
C                  Nothing to coalesce, so just negate the size,
C                  so block can be reused.
C
                   LSTAB(LINDEX) = -LSTAB(LINDEX)
                ENDIF
C
C               Set freed space to zeros
C
                DO 50 I = IND, IND + SIZE - 1
                   CSTORE(I:I) = CHAR( 0)
   50           CONTINUE
                FREEM = .FALSE.
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
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
