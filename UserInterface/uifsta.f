C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFSTA ( )
C       ---------------------------
C
C       PARAMETERS:
C
C       Prints out the current UIF status: whether system is running,
C       initialised, maximum and currently used number of elements,
C       and dynamic memory usage.
C
C       Returns TRUE in case of failure, oterwiser FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIFSTA ( )
C     ===========================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Function return code
C
      LOGICAL STATUS
C
C     Number of bytes of dynamic memory available
C
      INTEGER SIZE
C
C     CALLED FUNCTIONS:
C
C     Prints out object data
C
      LOGICAL UIXODA
C
C     Gets the amount of dynamic memory available
C
      LOGICAL MSPACE
C
C     Writes information to console
C
      LOGICAL SEMCON
C
C     Announce ourselves
C
      STATUS = SEMCON ( 'Current UIF status:' )
      STATUS = SEMCON ( ' ' )
      IF ( .NOT. STATUS ) THEN
C
C        Initialised?
C
         IF ( UIFISI ) THEN
            WRITE ( RECORD,10 ) 'initialised'
         ELSE
            WRITE ( RECORD,10 ) 'not initialised'
         ENDIF
   10    FORMAT ( 'System is ', a, '.' )
         STATUS = SEMCON ( RECORD )
         IF ( .NOT. STATUS ) THEN
C
C           If not initialised, can't say any more
C
            IF ( UIFISI ) THEN
C
C              Running?
C
               IF ( UIFRUN ) THEN
                  WRITE ( RECORD,10 ) 'running'
               ELSE
                  WRITE ( RECORD,10 ) 'not running'
               ENDIF
               STATUS = SEMCON ( RECORD )
               IF ( .NOT. STATUS ) THEN
C
C                 Report data
C
C                 Panels....
C
                  STATUS = UIXODA ( 'panels', MINPAN, MAXPAN )
                  IF ( .NOT. STATUS ) THEN
C
C                    Cells....
C
                     STATUS = UIXODA ( 'cells', MINCEL, MAXCEL )
                     IF ( .NOT. STATUS ) THEN
C
C                       Menus....
C
                        STATUS = UIXODA ( 'menus', MINMEN, MAXMEN )
                        IF ( .NOT. STATUS ) THEN
C
C                          Textfields....
C
                           STATUS = UIXODA ( 'textfields', MINTEX,
     +                                        MAXTEX )
                           IF ( .NOT. STATUS ) THEN
C
C                             And the amount of dynamic memory available
C
                              STATUS = MSPACE ( SIZE )
                              IF ( .NOT. STATUS ) THEN
                                 WRITE ( RECORD,20 ) SIZE
                                 STATUS = SEMCON ( RECORD )
   20                            FORMAT ( 'There are ', i6,
     +                ' bytes of dynamic memory currently available' )
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      UIFSTA = STATUS
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION UIXODA ( INFO, OBJMIN, OBJMAX )
C     ================================================
C
      CHARACTER*(*) INFO
      INTEGER OBJMIN, OBJMAX
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'COMMON'
C
C     LOCAL VARIABLES:
C
C     Loop counter
C
      INTEGER I
C
C     Number of objects in use
C
      INTEGER INUSE
C
C     CALLED FUNCTIONS:
C
C     Writes information to console
C
      LOGICAL SEMCON
C
C     Walk through the object range requested, and report the maximum
C     number available, and the number in use
C
      INUSE = 0
      DO 10 I = OBJMIN, OBJMAX
          IF ( OBJNAM(I) .GE. 0 ) INUSE = INUSE + 1
   10 CONTINUE
C
C     And tell the world the news
C
      WRITE ( RECORD,20 ) INFO, OBJMAX - OBJMIN + 1, INUSE
   20 FORMAT ( 'Maximum number of ', a16, ' = ', i5,
     + ' (', i5, ' currently in use)' )
C
      UIXODA = SEMCON ( RECORD )
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
