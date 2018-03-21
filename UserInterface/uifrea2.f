      LOGICAL FUNCTION GETFIL ( TYPE, VALUE )
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
C     PARAMETERS
C
C     number of integers that hold a real
C
      INTEGER INTINR
      PARAMETER (INTINR=LNREAL/LNINT)
C
C     CALLED FUNCTIONS:
C
C     Reads data into uifbuf from disc
C
      LOGICAL REAFIL
C
      IF ( TYPE .EQ. 1 .OR. TYPE .EQ. 2 ) THEN
C
C        If the uifbuf is empty, read into it
C
         IF ( UIFBPR + 1 .GT. UBUFSZ ) THEN
            STATUS = REAFIL ( )
         ELSE
            STATUS = .FALSE.
         ENDIF
C
C        And pass back the data
C
         IF ( .NOT. STATUS ) THEN
            UIFBPR = UIFBPR + 1
            VALUE(1) = UIFBUF(UIFBPR)
         ENDIF
      ELSE IF ( TYPE .EQ. 3 ) THEN
C
C        If the uifbuf is empty, read into it
C
         IF ( UIFBPR + INTINR .GT. UBUFSZ ) THEN
            STATUS = REAFIL ( )
         ELSE
            STATUS = .FALSE.
         ENDIF
C
C        And pass back the data
C
         IF ( .NOT. STATUS ) THEN
            DO 10 IND = 1,INTINR
               UIFBPR = UIFBPR + 1
               VALUE(IND) = UIFBUF(UIFBPR)
   10       CONTINUE
         ENDIF
      ENDIF
C
      GETFIL = STATUS
C
      RETURN
      END
C
      LOGICAL FUNCTION REAFIL ( )
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
C     Diagnoses file i/o error
C
      LOGICAL SEMIOE
C
      STATUS = .FALSE.
      IF ( UIFBPR .GE. UBUFSZ-1 ) THEN
         READ ( RDWRTU, ERR=10, END=10, IOSTAT=IOS ) UIFBUF
C
C        Re-zero the uifbuf pointer
C
         UIFBPR = 0
         GOTO 20
C
   10    CONTINUE
         INQUIRE ( UNIT=RDWRTU, NAME=FILENM )
         STATUS = SEMIOE ( IOS, RDWRTU, FILENM )
         IF (ERROR .EQ. 0) ERROR = 10
         UIFERR = ERROR - UIFEBA
         STATUS = .TRUE.
      ENDIF
C
   20 REAFIL = STATUS
C
      RETURN
      END
