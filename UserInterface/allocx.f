C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION ALLOCX ( NBYTES, LINDEX )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer nbytes : INPUT - Number of bytes to be allocated
C
C       integer lindex : OUTPUT - Logical index to block of memory
C                                 allocated
C
C       Does the actual work of allocation.  Checks first to see if
C       freed blocks of memory can be reused.  If so, then the smallest
C       freed block which is big enough will be used, and the remainder
C       of the free block will be pointed at by a new logical table
C       entry as freed store so it can be reused.  If not, then a new
C       logical index and physical block must be allocated.  First a
C       check is made to see if any logical entries freed by having
C       their physical blocks coalesced can be used. If not, then a
C       check is made that free logical indices exist, and the next free
C       one is selected for use.  The amount of free physical memory is
C       then checked, an if sufficient exists, its use is recorded.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION ALLOCX ( NBYTES, LINDEX )
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
C     LOCAL VARIABLES:
C
C     Loop counter
C
      INTEGER I
C
C     Physical index of logical index
C
      INTEGER PIND
C
C     Size of freed block, and minimum size freed block found fo re-use
C
      INTEGER SIZE, MINSIZ
C
C     Logical index of smallest freed block
C
      INTEGER BESTIN
C
C     Logical index of new block created when block is reused
C
      INTEGER NEWIND
C
C     New logical index created for caller
C
      INTEGER NINDEX
C
C     Look at all the used logical indices, so see if one of
C     sufficient size is free
C
      ALLOCX = .TRUE.
      BESTIN = 0
      MINSIZ = MXSTOR
      DO 10 I = 1, HILIND, ENTSIZ
C
C        continue if first time in...
C
         IF ( HILIND .LT. 1 ) GOTO 10
C
         SIZE = LSTAB ( I )
         IF ( SIZE .LT. 0 ) THEN
C
C           Found a freed index.  Check the size of it
C
            SIZE = -SIZE
            IF ( SIZE .GE. NBYTES ) THEN
C
C              Slot is big enough.  Record it so we can use the
C              smallest one we find
C
               IF ( SIZE .LT. MINSIZ ) THEN
                  MINSIZ = SIZE
                  BESTIN = I
C
C                 And if it was EXACTLY the size we wanted, use it!
C
                  IF ( SIZE .EQ. NBYTES ) GOTO 20
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE
   20 CONTINUE
C
C     Found one?
C
      IF ( BESTIN .NE. 0 ) THEN
C
C        OK!  Set the return value to the logical index for the block
C        found, save the physical index into the logical index slot,
C        and reset the size of the block positive
C
         SIZE = -LSTAB(BESTIN)
         LSTAB(BESTIN) = SIZE
         NINDEX = BESTIN
C
C        Now, if we have a logical index slot free, point another
C        logical index at the remainder of the store we have left
C        from the old block so it can be reused. First find the
C        logical index slot to use (providing there is any space to
C        recover!)
C
         IF ( SIZE - NBYTES .GT. 0 ) THEN
C
C           Any coalesced slots to use?
C
            DO 30 NEWIND = 1, HILIND, ENTSIZ
               IF ( LITAB (NEWIND) .EQ. 0 ) GOTO 40
   30       CONTINUE
   40       CONTINUE
            IF ( NEWIND .EQ. HILIND + 1 ) THEN
C
C              Couldn't reuse a slot, so make a new one
C
               IF ( HILIND + ENTSIZ .LE. MXLTAB ) THEN
                  HILIND = HILIND + 1
                  NEWIND = HILIND
               ELSE
C
C                 Logical index table full, so can't reclaim
C
                  NEWIND = 0
               ENDIF
            ENDIF
C
C           Found a slot?  If not, we can't reclaim the space
C
            IF ( NEWIND .NE. 0 ) THEN
C
C              Size of allocated block is now really size required
C
               LSTAB(BESTIN) = NBYTES
C
C              Set up new logical index data
C
               LITAB(NEWIND) = LITAB(BESTIN) + NBYTES
               LSTAB(NEWIND) = NBYTES - SIZE
C
C              Set reclaimed space to zeros
C
               PIND = LITAB(NEWIND)
               SIZE = -LSTAB(NEWIND)
               DO 50 I = PIND, PIND + SIZE - 1
                  CSTORE(I:I) = CHAR( 0)
   50          CONTINUE
            ENDIF
         ENDIF
         ALLOCX = .FALSE.
      ELSE
C
C        Must allocate a new block.  First check if we can
C        find a coalesced block entry to use
C
         DO 60 I = 1, HILIND, ENTSIZ
            IF ( LITAB (I) .EQ. 0 ) GOTO 70
   60    CONTINUE
   70    CONTINUE
         IF ( I .NE. HILIND + 1 ) THEN
C
C           Found a coalesced entry.  Reuse it
C
            NINDEX = I
         ELSE
C
C           Must allocate a new block.  First check if we can fit the
C           new logical index, and the block, into the available space
C
            IF ( HILIND + ENTSIZ .LE. MXLTAB ) THEN
C
C              Yes.  Set size info. into block header, and physical
C              address into logical address slot
C
               HILIND = HILIND + 1
               NINDEX = HILIND
            ELSE
C
C              Logical index table is full.  Nothing more we can do
C
               UIFERR = MEMLIF
               GOTO 80
            ENDIF
         ENDIF
C
C        Ok, got our logical table entry.  Check that we have room
C        in the physical store
C
         IF ( HIPIND + NBYTES - 1 .LE. MXSTOR ) THEN
C
C           OK, all fits.  Set size and physical index into
C           logical index tables
C
            LITAB(NINDEX) = HIPIND
            LSTAB(NINDEX) = NBYTES
            HIPIND = HIPIND + NBYTES
            ALLOCX = .FALSE.
         ELSE
C
C           Oh dear.  Store may need compression. Fail.
C
            UIFERR = MEMFUL
C
C           Readjust the highest used logical index if we were
C           about to use it
C
            IF ( NINDEX .EQ. HILIND ) HILIND = HILIND - 1
         ENDIF
      ENDIF
C
C     All done...
C
   80  CONTINUE
C
C     If it worked, copy the logical index we used out for the caller
C
      IF ( .NOT. ALLOCX ) LINDEX = NINDEX
C
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
