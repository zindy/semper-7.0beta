      LOGICAL FUNCTION PUTFIL ( TYPE, VALUE )
C     =======================================
C
      INTEGER TYPE
      INTEGER VALUE(*)
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     index variable
C
      INTEGER IND
C
C     PARAMETERS:
C     number of integers that hold a real
C
      INTEGER INTINR
      PARAMETER(INTINR = LNREAL/LNINT)
C
C     CALLED FUNCTIONS:
C
C     Flushes out the uifbuf to disc
C
      LOGICAL FLUFIL
C
      IF ( TYPE .EQ. 1 .OR. TYPE .EQ. 2 ) THEN
C
C        If the uifbuf is full, flush it out
C
         IF ( UIFBPR .GE. UBUFSZ ) THEN
            STATUS = FLUFIL ( )
         ELSE
            STATUS = .FALSE.
         ENDIF
C
C        And save the data
C
         IF ( .NOT. STATUS ) THEN
            UIFBPR = UIFBPR + 1
            UIFBUF(UIFBPR) = VALUE(1)
         ENDIF
      ELSE IF ( TYPE .EQ. 3 ) THEN
C
C        If the uifbuf is full, flush it out
C
         IF ( UIFBPR + INTINR .GT. UBUFSZ ) THEN
            STATUS = FLUFIL ( )
         ELSE
            STATUS = .FALSE.
         ENDIF
C
C        And save the data
C
         IF ( .NOT. STATUS ) THEN
            DO 10 IND=1,INTINR
               UIFBPR = UIFBPR + 1
               UIFBUF(UIFBPR) = VALUE(IND)
   10       CONTINUE
         ENDIF
      ENDIF
C
      PUTFIL = STATUS
C
      RETURN
      END
C
      LOGICAL FUNCTION FLUFIL ( )
C     ===========================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     I/O status
C
      INTEGER IOS
C
C     Filename
C
      CHARACTER*80 FILENM
C
C     CALLED FUNCTIONS:
C
C     Diagnoses i/o errors
C
      LOGICAL SEMIOE
C
C     If the uifbuf is not empty, flush it out
C
      STATUS = .FALSE.
      IF ( UIFBPR .GT. 0 ) THEN
         WRITE ( RDWRTU,IOSTAT=IOS,ERR=10 ) UIFBUF
C
C        Re-zero the uifbuf pointer
C
         UIFBPR = 0
         GOTO 20
C
   10    INQUIRE ( UNIT=RDWRTU,NAME=FILENM )
         STATUS = SEMIOE ( IOS,RDWRTU,FILENM )
         IF (ERROR .EQ. 0) ERROR = 10
         UIFERR = ERROR - UIFEBA
         STATUS = .TRUE.
      ENDIF
C
   20 FLUFIL = STATUS
C
      RETURN
      END
