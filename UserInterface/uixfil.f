      LOGICAL FUNCTION OPEFIL ( NAME, LENGTH, WRITE )
C
C     ===============================================
C
      CHARACTER*(*) NAME
      INTEGER LENGTH
      LOGICAL WRITE
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Temporary full file path names
C
      CHARACTER*(FILMAX) FULNAM,PATHNM
C
C     I/O status
C
      INTEGER IOS
C
C     File exists flag
C
      LOGICAL EXISTS
C
C     CALLED FUNCTIONS:
C
C     Builds a file path name
C
      LOGICAL FILMAK
C
C     Searches for a file
C
      LOGICAL FILSEA
C
C     Deletes a file via FORTRAN i/o
C
      LOGICAL FORTDE
C
C     Diagnoses I/O error
C
      LOGICAL SEMIOE
C
C     Finds non-blank length of string
C
      INTEGER LNBLNK
C
      STATUS = .FALSE.
      PATHNM = NAME(1:LENGTH)
      IF ( WRITE ) THEN
C
C        Build the path name
C
         IF ( FILMAK(PATHNM,'.uif',FULNAM) ) GOTO 20
      ELSE
         FULNAM = PATHNM
      ENDIF
C
C     See if it exists
C
      IF ( FILSEA(FULNAM,'.uif',PATHNM,EXISTS) ) GOTO 20
C
      IF ( WRITE ) THEN
C
C     If file exists then delete it
C
         IF ( EXISTS ) THEN
            IF ( FORTDE ( RDWRTU, PATHNM, IOS ) ) GOTO 10
         ELSE
            PATHNM = FULNAM
         ENDIF
C
C        Open the file for unformatted IO
C
         OPEN ( UNIT=RDWRTU,FILE=PATHNM,STATUS='NEW',ERR=10,IOSTAT=IOS,
     +          FORM='UNFORMATTED' )
      ELSE
C
C        Open for read
C
         IF ( EXISTS ) THEN
            OPEN ( UNIT=RDWRTU,FILE=PATHNM,ERR=10,STATUS='OLD',
     +             IOSTAT=IOS,
     +             FORM='UNFORMATTED' )
            REWIND ( UNIT=RDWRTU,IOSTAT=IOS,ERR=10 )
         ELSE
            ERROR = 130
            IOS = LNBLNK(FULNAM)
            IDMESS = FULNAM(1:IOS)
            GOTO 20
         ENDIF
      ENDIF
      GOTO 30
C
C     Here if error opening
C
   10 IF (SEMIOE(IOS,RDWRTU,PATHNM)) GOTO 20
   20 IF (ERROR .EQ. 0) ERROR = 10
      UIFERR = ERROR - UIFEBA
      STATUS = .TRUE.
      CLOSE ( RDWRTU,ERR=30 )
C
   30 CONTINUE
      OPEFIL = STATUS
C
      RETURN
      END
C
      LOGICAL FUNCTION CLOFIL ( NAME, LENGTH, DELETE )
C
C     ================================================
C
      CHARACTER*(*) NAME
      INTEGER LENGTH
      LOGICAL DELETE
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
      INCLUDE 'COMMON'
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
C     CALLED FUNCTIONS:
C
C     Diagnoses I/O errors
C
      LOGICAL SEMIOE
C
C     Either close or close and delete file
C
      STATUS = .FALSE.
      IF ( DELETE ) THEN
         CLOSE ( UNIT=RDWRTU,STATUS='DELETE',IOSTAT=IOS,ERR=10 )
      ELSE
         CLOSE ( UNIT=RDWRTU,STATUS='KEEP',IOSTAT=IOS,ERR=10 )
      ENDIF
      GOTO 30
C
C     Here if error closing
C
   10 IF (SEMIOE(IOS,RDWRTU,NAME(1:LENGTH))) GOTO 20
   20 IF (ERROR .EQ. 0) ERROR = 10
      UIFERR = ERROR - UIFEBA
      STATUS = .TRUE.
C
   30 CLOFIL = STATUS
C
      RETURN
      END
C
      SUBROUTINE UIXSER(I)
      INTEGER I(*)
C
C The following format for the serial number MUST NOT be changed as
C it is required for the Production system serial number patcher
C
C      CALL SEMICS('Serial Number SIGRXV11004ESY000000',I,34)
      CALL SEMICS ('Semper Build  7.0-Beta01-July-2005',I,34)
      RETURN
      END
