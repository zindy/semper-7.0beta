C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQREAD ( SOURCE, METHOD, ENTRY, I1, I2, I3 )
C       -------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT/OUTPUT - Source to read from
C
C       integer method : INPUT - Access method to use
C
C       integer entry : INPUT/OUTPUT - Queue entry to examine
C
C       integer i1, i2, i3 : OUTPUT - The information read from the
C                            queue
C
C       Function takes information from the queue specified by SOURCE
C       and returns it in I1, I2, and I3 as required.
C
C       If SOURCE is given as MANYS the actual source used is returned
C       in SOURCE.
C
C       METHOD is used to specify the queue access method.
C
C       ENTRY is used to indicate which entry to examine, (this is only
C       valid for method QLOOK), entry zero refers to the start of the
C       queue.  ENTRY returns zero if there was no entry to read or a
C       non-zero value if the read was sucessful (for access method
C       QLOOK this value is used to indicate the next entry to examine).
C       To scan a queue, make a call with method QLOOK for the queue
C       head and then use method QNEXT until ENTRY is returned as zero.
C       Method QSNAP will look at the actual physical source (if
C       possible); this is probably most useful to read the current
C       switch state or pointer position.  If QSNAP is used with source
C       MANYS, and the BREAK and KEYBOARD queues are empty, then the
C       POINTER and SWITCH sources will be physically snapshotted (if
C       in state QRUN).  Pointer will be snapshotted in preference to
C       SWITCHES.  EQREAD will return .TRUE. if any arguments are
C       invalid or if an error occurs.
C
C       I1, I2 and I3 are used as follows:
C
C       For BREAK:
C          I1 contains the number of BREAKs detected.
C
C       For KEYBOARD:
C          I1 contains the internal ASCII character code of the key
C          pressed for a simple key, or one of the following values:
C          (defined in include file ICSET)
C
C          KBRET  - Return/enter key
C          KBTAB  - TAB key
C
C          KBDEL  - Local delete character
C          KBKILL - Local delete line
C
C          KBINS  - Insert/overstrike toggle
C          KBHOME - Cursor to start of line
C          KBEND  - Cursor to end of line
C
C          KBREFL - Refresh line
C
C          KBUP   - Cursor up
C          KBDOWN - Cursor down
C          KBLEFT - Cursor left
C          KBRITE - Cursor right
C
C          KBFUNC+n - Function key n
C
C          Any other single key strokes with value < 255 and any
C          unrecognised escape sequences are passed straight through.
C          Any other key strokes are ignored.
C
C       For POINTER:
C          I1 contains the change in X coordinate
C          I2 contains the change in Y coordinate
C
C       For SWITCHES:
C          I1 contains the switch closure number or zero
C          I2 contains the switch open number or zero
C          I3 contains a bit packed (lsb=switch1) closure set
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQREAD ( SOURCE, METHOD, ENTRY, I1, I2, I3 )
C
C     =============================================================
C
      INTEGER SOURCE
      INTEGER METHOD
      INTEGER ENTRY
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
C     CALLED FUNCTIONS:
C
C     Finds a suitable source, whren source is MANYS
C
      LOGICAL EQXFSO
C
C     Reads entry from the break queue
C
      LOGICAL EQXBRR
C
C     Reads entry from the keyboard queue
C
      LOGICAL EQXKER
C
C     Reads entry from the poiner queue
C
      LOGICAL EQXPOR
C
C     Reads entry from the switches queue
C
      LOGICAL EQXSWR
C
C     See if we need to find a source
C
      STATUS = .FALSE.
      IF ( SOURCE .EQ. MANYS ) THEN
C
C        Find the source
C
         STATUS = EQXFSO ( SOURCE, METHOD )
      ENDIF
C
C     Here when we know which source we are to read.  Deal with it.
C
      IF ( .NOT. STATUS ) THEN
         IF ( SOURCE .EQ. MBREAK ) THEN
C
C           Read break queue
C
            STATUS = EQXBRR ( ENTRY, METHOD, I1 )
         ELSE IF ( SOURCE .EQ. MKEY ) THEN
C
C           Read keyboard queue
C
            STATUS = EQXKER ( ENTRY, METHOD, I1 )
         ELSE IF ( SOURCE .EQ. MPOINT ) THEN
C
C           Read pointer queue
C
            STATUS = EQXPOR ( ENTRY, METHOD, I1, I2 )
         ELSE IF ( SOURCE .EQ. MBUT ) THEN
C
C           Read switches queue
C
            STATUS = EQXSWR ( ENTRY, METHOD, I1, I2, I3 )
         ELSE
C
C           Error - invalid source
C
            STATUS = .TRUE.
         ENDIF
      ENDIF
C
C     If we haven't found anything, poll all devices for next time
C     NOTE WE DO NOT CHANGE THE RETURN STATUS
C
      IF ( STATUS ) THEN
C
C        Poll break queue
C
         IF ( BRKSTA .EQ. QRUN ) CALL EQXBPL
C
C        Poll keyboard queue
C
         IF ( KEYSTA .EQ. QRUN ) CALL EQXKPL
C
C        Poll pointer queue
C
         IF ( PNTSTA .EQ. QRUN ) CALL EQXPPL
C
C        Poll switches queue
C
         IF ( SWTSTA .EQ. QRUN ) CALL EQXSPL
      ENDIF
C
C     All done
C
      EQREAD = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXFSO ( SOURCE, METHOD )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer source : INPUT/OUTPUT - Source to read from
C
C       integer method : INPUT - Access method to use
C
C       Given a SOURCE containing MANYS, and access method METHOD,
C       returns SOURCE as the source to use for reading.  Returns
C       .FALSE. if such a source is found, otherwise .TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXFSO ( SOURCE, METHOD )
C
C     ==========================================
C
      INTEGER SOURCE
      INTEGER METHOD
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
C     Check break queue first - found source if queue running and
C     break count not zero
C
      STATUS = .TRUE.
      IF ( BRKSTA .EQ. QRUN ) THEN
         IF ( BRKCNT .NE. 0 ) THEN
            SOURCE = MBREAK
            STATUS = .FALSE.
         ENDIF
      ENDIF
C
C     Next keyboard - found source if queue running and read pointer
C     not equal read pointer
C
      IF ( STATUS ) THEN
         IF ( KEYSTA .EQ. QRUN ) THEN
            IF ( KQREAD .NE. KQINTO ) THEN
               SOURCE = MKEY
               STATUS = .FALSE.
            ENDIF
         ENDIF
      ENDIF
C
C     Next pointer - found source if queue running and read pointer
C     not equal read pointer
C
      IF ( STATUS ) THEN
         IF ( PNTSTA .EQ. QRUN ) THEN
C
C           If queue is empty, but snapshotting, use the pointer
C           queue anyway rather than the switches queue.
C
            IF ( PQREAD .NE. PQINTO .OR. METHOD .EQ. QSNAP ) THEN
               SOURCE = MPOINT
               STATUS = .FALSE.
            ENDIF
         ENDIF
      ENDIF
C
C     Lastly switches - found source if queue running and read pointer
C     not equal read pointer
C
      IF ( STATUS ) THEN
         IF ( SWTSTA .EQ. QRUN ) THEN
C
C           If queue is empty, but snapshotting, use the switches
C           queue anyway.
C
            IF ( SQREAD .NE. SQINTO .OR. METHOD .EQ. QSNAP ) THEN
               SOURCE = MBUT
               STATUS = .FALSE.
            ENDIF
         ENDIF
      ENDIF
C
      EQXFSO = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXBRR ( ENTRY, METHOD, I1 )
C       ---------------------------------------------
C
C       PARAMETERS:
C
C       integer entry : INPUT/OUTPUT - Entry to read from the queue
C
C       integer method : INPUT - Access method to use
C
C       integer i1 : OUTPUT - Queue data returned
C
C       Read entry ENTRY from the break queue, using access method
C       METHOD.  Returns break count in I1 if read is sucessful.
C       Function returns .FALSE. if sucessful, otherwise .TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXBRR ( ENTRY, METHOD, I1 )
C
C     =============================================
C
      INTEGER ENTRY
      INTEGER METHOD
      INTEGER I1
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
C     Check queue is running
C
      IF ( BRKSTA .EQ. QRUN ) THEN
C
C        If method is QLOOK, entry must be zero.
C
         STATUS = .FALSE.
         IF ( METHOD .EQ. QLOOK ) THEN
            IF ( ENTRY .NE. 0 ) THEN
               ENTRY = 0
               STATUS = .TRUE.
            ENDIF
         ENDIF
         IF ( .NOT. STATUS ) THEN
C
C           If we are taking, destroy the queue
C
            I1 = BRKCNT
            IF ( METHOD .EQ. QTAKE ) THEN
               BRKCNT = 0
C
C              Tell the event handler that we have read the breaks
C
               CALL EQXXZB
            ENDIF
C
C           Check break count
C
            IF ( I1 .NE. 0 ) THEN
C
C              Set entry saying it worked
C
               ENTRY = 1
            ELSE
C
C              Queue empty - return next entry as zero
C
               ENTRY = 0
            ENDIF
         ENDIF
      ELSE
C
C        Error - queue not running
C
         STATUS = .TRUE.
         ENTRY = 0
      ENDIF
C
      EQXBRR = STATUS
      RETURN
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXKER ( ENTRY, METHOD, I1 )
C       ---------------------------------------------
C
C       PARAMETERS:
C
C       integer entry : INPUT/OUTPUT - Entry to read from the queue
C
C       integer method : INPUT - Access method to use
C
C       integer i1 : OUTPUT - Queue data returned
C
C       Read entry ENTRY from the keyboard queue, using access method
C       METHOD.  Returns character in I1 if read is sucessful.  Function
C       returns .FALSE. if sucessful, otherwise .TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXKER ( ENTRY, METHOD, I1 )
C
C     =============================================
C
      INTEGER ENTRY
      INTEGER METHOD
      INTEGER I1
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
C     Queue length
C
      INTEGER LENGTH
C
C     Check queue is running
C
      STATUS = .TRUE.
      IF ( KEYSTA .EQ. QRUN ) THEN
C
C        Check if queue is empty
C
         IF ( KQREAD .NE. KQINTO ) THEN
C
C           Queue has contents.  Branch on access method
C
            IF ( METHOD .EQ. QTAKE ) THEN
C
C              Read and consume at read position
C
               ENTRY = 1
               KQREAD = IAND ( KQREAD + 1, KQMASK )
               I1 = KQUEUE(KQREAD)
               STATUS = .FALSE.
            ELSE IF ( METHOD .EQ. QLOOK ) THEN
C
C              Check entry is valid
C
               IF ( ENTRY .GE. 0 .AND. ENTRY .LT. KQSIZE ) THEN
C
C                 Check length of queue
C
                  LENGTH = KQINTO + KQSIZE - KQREAD
                  LENGTH = IAND ( LENGTH, KQMASK )
                  ENTRY = ENTRY + 1
                  IF ( ENTRY .LE. LENGTH ) THEN
C
C                    Get entry for caller
C
                     I1 = KQUEUE(IAND ( KQREAD + ENTRY, KQMASK ))
                     IF ( ENTRY - 1 .EQ. LENGTH ) THEN
                        ENTRY = 0
                     ENDIF
                     STATUS = .FALSE.
                  ELSE
C
C                    Attempting to look off the end of the queue.
C                    Set entry zero and sucess result.
C
                     ENTRY = 0
                     STATUS = .FALSE.
                  ENDIF
               ENDIF
            ELSE IF ( METHOD .EQ. QSNAP ) THEN
C
C              Snapshot - return last key read
C
               ENTRY = 1
               I1 = KQLAST
               STATUS = .FALSE.
            ENDIF
         ELSE
C
C           Queue empty - return last key read if snapshotting
C
            IF ( METHOD .EQ. QSNAP ) THEN
               ENTRY = 1
               I1 = KQLAST
               STATUS = .FALSE.
            ELSE
C
C              Just return next entry as zero
C
               ENTRY = 0
               STATUS = .FALSE.
            ENDIF
         ENDIF
      ELSE
C
C        Error - queue not running
C
          ENTRY = 0
      ENDIF
C
      EQXKER = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXPOR ( ENTRY, METHOD, I1, I2 )
C       -------------------------------------------------
C
C       PARAMETERS:
C
C       integer entry : INPUT/OUTPUT - Entry to read from the queue
C
C       integer method : INPUT - Access method to use
C
C       integer i1, i2 : OUTPUT - Queue data returned
C
C       Read entry ENTRY from the pointer queue, using access method
C       METHOD.  Returns X,Y delta in I1, I2 if read is sucessful.
C
C       Function returns .FALSE. if sucessful, otherwise .TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXPOR ( ENTRY, METHOD, I1, I2 )
C
C     =================================================
C
      INTEGER ENTRY
      INTEGER METHOD
      INTEGER I1, I2
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
C     Queue length
C
      INTEGER LENGTH
C
C     Queue entry to look at
C
      INTEGER QENTRY
C
C     Check queue is running
C
      STATUS = .TRUE.
      IF ( PNTSTA .EQ. QRUN ) THEN
C
C        Check if snapshotting - special action if so
C
         IF ( METHOD .NE. QSNAP ) THEN
C
C           Check if queue is empty
C
            IF ( PQREAD .NE. PQINTO ) THEN
C
C              Queue has contents.  Branch on access method
C
               IF ( METHOD .EQ. QTAKE ) THEN
C
C                 Read and consume at read position
C
                  ENTRY = 1
                  PQREAD = IAND ( PQREAD + 1, PQMASK )
                  I1 = PQUEUE(1,PQREAD)
                  I2 = PQUEUE(2,PQREAD)
                  STATUS = .FALSE.
               ELSE IF ( METHOD .EQ. QLOOK ) THEN
C
C                 Check entry is valid
C
                  IF ( ENTRY .GE. 0 .AND. ENTRY .LT. PQSIZE ) THEN
C
C                    Check length of queue
C
                     LENGTH = PQINTO + PQSIZE - PQREAD
                     LENGTH = IAND ( LENGTH, PQMASK )
                     ENTRY = ENTRY + 1
                     IF ( ENTRY .LE. LENGTH ) THEN
C
C                       Get entry for caller
C
                        QENTRY = IAND ( PQREAD + ENTRY, PQMASK )
                        I1 = PQUEUE(1,QENTRY)
                        I2 = PQUEUE(2,QENTRY)
                        IF ( ENTRY - 1 .EQ. LENGTH ) THEN
                           ENTRY = 0
                        ENDIF
                        STATUS = .FALSE.
                     ELSE
C
C                       Attempting to look off the end of the queue.
C                       Set entry zero and sucess result.
C
                        ENTRY = 0
                        STATUS = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
C
C              Queue is empty - return next entry as zero
C
               ENTRY = 0
               STATUS = .FALSE.
            ENDIF
         ELSE
C
C           Snapshot - poll the queue, return last delta
C
            CALL EQXPPL
            ENTRY = 1
            I1 = PNTXLD
            I2 = PNTYLD
            STATUS = .FALSE.
         ENDIF
      ELSE
C
C        Queue not running - return next entry as zero.
C
         ENTRY = 0
      ENDIF
C
      EQXPOR = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION EQXSWR ( ENTRY, METHOD, I1, I2, I3 )
C       -----------------------------------------------------
C
C       PARAMETERS:
C
C       integer entry : INPUT/OUTPUT - Entry to read from the queue
C
C       integer method : INPUT - Access method to use
C
C       integer i1, i2, i3 : OUTPUT - Queue data returned
C
C       Read entry ENTRY from the switches queue, using access method
C       METHOD.  Returns switch closure number in I1, switch open number
C       in I2 and switch closure set in I3 if read is sucessful.
C       Function returns .FALSE. if sucessful, otherwise .TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION EQXSWR ( ENTRY, METHOD, I1, I2, I3 )
C
C     =====================================================
C
      INTEGER ENTRY
      INTEGER METHOD
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
C     Queue length
C
      INTEGER LENGTH
C
C     Queue entry to look at
C
      INTEGER QENTRY
C
C     Check queue is running
C
      STATUS = .TRUE.
      IF ( SWTSTA .EQ. QRUN ) THEN
C
C        Check if snapshotting - special action if so
C
         IF ( METHOD .NE. QSNAP ) THEN
C
C           Check if queue is empty
C
            IF ( SQREAD .NE. SQINTO ) THEN
C
C              Queue has contents.  Branch on access method
C
               IF ( METHOD .EQ. QTAKE ) THEN
C
C                 Read and consume at read position
C
                  ENTRY = 1
                  SQREAD = IAND ( SQREAD + 1, SQMASK )
                  I1 = SQUEUE(1,SQREAD)
                  I2 = SQUEUE(2,SQREAD)
                  I3 = SQUEUE(3,SQREAD)
                  STATUS = .FALSE.
               ELSE IF ( METHOD .EQ. QLOOK ) THEN
C
C                 Check entry is valid
C
                  IF ( ENTRY .GE. 0 .AND. ENTRY .LT. SQSIZE ) THEN
C
C                    Check length of queue
C
                     LENGTH = SQINTO + SQSIZE - SQREAD
                     LENGTH = IAND ( LENGTH, SQMASK )
                     ENTRY = ENTRY + 1
                     IF ( ENTRY .LE. LENGTH ) THEN
C
C                       Get entry for caller
C
                        QENTRY = IAND ( SQREAD + ENTRY, SQMASK )
                        I1 = SQUEUE(1,QENTRY)
                        I2 = SQUEUE(2,QENTRY)
                        I3 = SQUEUE(3,QENTRY)
                        IF ( ENTRY - 1 .EQ. LENGTH ) THEN
                           ENTRY = 0
                        ENDIF
                        STATUS = .FALSE.
                     ELSE
C
C                       Attempting to look off the end of the queue.
C                       Set entry zero and sucess result.
C
                        ENTRY = 0
                        STATUS = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
C
C              Queue is empty - return next entry as zero
C
               ENTRY = 0
               STATUS = .FALSE.
            ENDIF
         ELSE
C
C           Snapshot - poll the queue, return data read
C
            CALL EQXSPL
            ENTRY = 1
            I1 = SWTLCL
            I2 = SWTLOP
            I3 = SWTLCS
            STATUS = .FALSE.
         ENDIF
      ELSE
C
C        Error - queue not running
C
         ENTRY = 0
      ENDIF
C
      EQXSWR = STATUS
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
