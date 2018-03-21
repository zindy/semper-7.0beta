C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXINP ( BROKE )
C       ---------------------------------
C
C       PARAMETERS:
C
C       logical broke : OUTPUT - 'The use broke' flag
C
C       Loops interrogating the raw event queues.  For each event queue
C       entry, the entry is read and then processed into zero or more
C       UIF event queue entries.  If a break queue entry occurs, the
C       loop is broken, and the funtion returns FALSE.  Thus any
C       application must perform suitable break checking (e.g. call to
C       ABANDN in Semper).
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIXINP ( BROKE )
C     =================================
C
      LOGICAL BROKE
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'EVENTS'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Pointer position delta read from event queue
C
      INTEGER DELX, DELY
C
C     Cumulative pointer position delta
C
      INTEGER DELTAX, DELTAY
C
C     Switch closed and open number from event queue
C
      INTEGER CLOSED, OPENED
C
C     Queue input has been detected on
C
      INTEGER QUEUE
C
C     Character read from the keyboard
C
      INTEGER ICH
C
C     'All done' flag
C
      LOGICAL DONE
C
C     Dummy variable
C
      INTEGER DUMVAR
C
C     Number of entries in the pointer queue
C
      INTEGER NQUEUE
C
C     Loop counter
C
      INTEGER I
C
C     CALLED FUNCTIONS:
C
C     Checks to see which event queue has information
C
      LOGICAL EQNEXT
C
C     Enquires the state of a queue
C
      LOGICAL EQNQRE
C
C     Reads from an event queue
C
      LOGICAL EQREAD
C
C     Processes button events
C
      LOGICAL UIXIPB
C
C     Processes pointer events
C
      LOGICAL UIXIPP
C
C     Processes keyboard events
C
      LOGICAL UIXIPK
C
C     Loop reading the queues until we get an action to process
C
      DONE  = .FALSE.
      BROKE = .FALSE.
   10 CONTINUE
         STATUS = EQNEXT ( QUEUE )
         IF ( .NOT. STATUS ) THEN
C
C           Read from the appropriate queue
C
            IF ( QUEUE .EQ. MBUT ) THEN
C
C              Button event.  Read it, and send it off for processing
C
               STATUS = EQREAD (MBUT,QTAKE,DUMVAR,CLOSED,OPENED,DUMVAR)
               IF ( .NOT. STATUS ) THEN
C
C                 Check it was a closed event, not opened
C
                  IF ( CLOSED .NE. 0 ) THEN
                     STATUS = UIXIPB ( CLOSED, DONE )
                     IF ( DONE .OR. STATUS ) GOTO 30
                  ENDIF
               ELSE
C
C                 Error - break out
C
                  GOTO 30
               ENDIF
            ELSE IF ( QUEUE .EQ. MPOINT ) THEN
C
C              Pointer event.  Just read all data in the queue, and act
C              on the last entry.
C
               STATUS = EQNQRE ( MPOINT, DUMVAR, DUMVAR, NQUEUE )
               IF ( .NOT. STATUS ) THEN
                  DELTAX = 0
                  DELTAY = 0
                  DO 20 I = 1, NQUEUE
                     STATUS = EQREAD ( MPOINT, QTAKE, DUMVAR,
     +                                 DELX, DELY, DUMVAR)
                     DELTAX = DELTAX + DELX
                     DELTAY = DELTAY + DELY
                     IF ( STATUS ) GOTO 30
20                CONTINUE
                  IF ( .NOT. STATUS ) THEN
C
C                    Process the new position
C
                     STATUS = UIXIPP ( DELTAX, DELTAY, DONE )
                     IF ( DONE .OR. STATUS ) GOTO 30
                     CALL APPWAI ( 0.02 )
                  ELSE
C
C                    Error - break out
C
                     GOTO 30
                  ENDIF
               ELSE
C
C                 Error - break out.
C
                  GOTO 30
               ENDIF
            ELSE IF ( QUEUE .EQ. MKEY ) THEN
C
C              Keyboard event.
C
               STATUS = EQREAD (MKEY,QTAKE,DUMVAR,ICH,DUMVAR,DUMVAR)
               IF ( .NOT. STATUS ) THEN
                  STATUS = UIXIPK ( ICH, DONE )
                  IF ( DONE .OR. STATUS ) GOTO 30
               ELSE
C
C                 Error - break out
C
                  GOTO 30
               ENDIF
            ELSE IF ( QUEUE .EQ. MBREAK ) THEN
C
C              Break event.  DONT read it, but return to let the caller
C              sort it out.
C
               BROKE  = .TRUE.
               STATUS = .FALSE.
               GOTO 30
            ENDIF
         ELSE
            CALL APPWAI ( 0.02 )
         ENDIF
C
C        Loop again
C
      GOTO 10
   30 CONTINUE
C
C     All done
C
      UIXINP = STATUS
      RETURN
C
C Copyright (C) 1988, 1989, 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
