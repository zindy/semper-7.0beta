C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION COMPM ( )
C       --------------------------
C
C       PARAMETERS:
C
C       Compresses store so that all fragmented free memory is moved
C       out to the end of the allocated area so it can be used.  The
C       logical to physical mappings are changed for any blocks which
C       are moved, and logicial indices of free blocks removed are set
C       to zero for reuse.  This subroutine is mainly called when
C       allocation fails to try and free extra space, but it can be
C       called at any time.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION COMPM ( )
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
C     Physical indices of copy to and from positions
      INTEGER TOIND, FROIND
C
C     Size of free or full blocks
      INTEGER SIZE
C
C     Loop counters
      INTEGER I, J
C
C     Logical index of block being moved
      INTEGER LINDEX
C
C     Check system is initialised
C
      IF ( ISINIT ) THEN
C
C        Find the start index of the first bit of space to compress out.
C
         TOIND = 0
         DO 10 I = 1, HILIND, ENTSIZ
            IF ( LSTAB ( I ) .LT. 0 ) THEN
               TOIND = LITAB(I)
               SIZE  = -LSTAB(I)
               FROIND = TOIND
C
C              This is about to disappear, so zero logical indices
C
               LITAB(I) = 0
               LSTAB(I) = 0
               GOTO 20
            ENDIF
   10    CONTINUE
   20    CONTINUE
C
C        If no free space, nothing to do.  We really are full up.
C
         IF ( TOIND .NE. 0 ) THEN
C
C           Toind is now the physical index of the first bit of free
C           space.  Walk along the store from this index up, moving
C           occupied store down, and updating the corresponding
C           logical indices.
C
   30       CONTINUE
C
C              Find the start of the next block to copy down
C
               FROIND = FROIND + SIZE
               DO 50 I = 1, HILIND, ENTSIZ
                  IF ( LITAB(I) .EQ. FROIND ) THEN
                     SIZE = LSTAB(I)
                     IF ( SIZE .LT. 0 ) THEN
C
C                       Free block to skip.  Increment the copy from
C                       index, and set the logical index of the block
C                       we are copying over the top of to zero so it
C                       can be reused
C
                        FROIND = FROIND - SIZE
                        LITAB(I) = 0
                        LSTAB(I) = 0
C
C                       Now find the block to copy
C
                        DO 40 J = 1, HILIND, ENTSIZ
                           IF ( LITAB(J) .EQ. FROIND ) THEN
                              LINDEX = J
                              SIZE = LSTAB(J)
                              GOTO 60
                           ENDIF
   40                   CONTINUE
C
C                       Here if fell through loop, i.e. no
C                       following block.  Nothing more to do
C
                        GOTO 70
                     ELSE
C
C                       i is now the logical index of the next block,
C                       froind is the physical index to copy down from
C
                        LINDEX = I
                        GOTO 60
                     ENDIF
                  ENDIF
   50          CONTINUE
C
C              Here if we fall through the loop.  Nothing left to do
C
               GOTO 70
   60       CONTINUE
C
C           Update the logical index
C
            LITAB(LINDEX) = TOIND
C
C           and perform the copy
C
            CSTORE(TOIND:TOIND+SIZE-1) = CSTORE(FROIND:FROIND+SIZE-1)
            TOIND = TOIND + SIZE
C
C           Try for the next block
C
            GOTO 30
   70       CONTINUE
C
C           Fill to the end of space with zeros
C
            DO 80 I = TOIND, MXSTOR
               CSTORE(I:I) = CHAR( 0)
   80       CONTINUE
C
C           And re-adjust the highest used physical index
C
            HIPIND = TOIND
         ENDIF
         COMPM = .FALSE.
      ELSE
C
C         Error - system not initialised
C
         UIFERR = MEMINI
         COMPM = .TRUE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
