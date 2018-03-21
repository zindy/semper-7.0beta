C Semper 6 system module SEMZZZ
C----------------------------------------------------------------------
C
C       FUNCTION SEMZZZ ( NAME, DEFNAM )
C       --------------------------------
C
C       PARAMETERS:
C
C       character*(*) name : OUTPUT - string to return file name
C
C       character*(*) defnam : INPUT - default part of file name
C
C       Creates a file name based on the current date and time.  Does
C       not check to see if the file with that name exists or not.  The
C       file name will have a fixed first part (zero or more Z's), then
C       up to four more characters based on the seconds and minutes of
C       the time on entry. Any required default fields (e.g. extension)
C       should be supplied in defnam. The directory prefix will be
C       built using GETTMP if no default is supplied.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION SEMZZZ(NAME,DEFNAM)
C
C     ====================================
C
      CHARACTER*(*) NAME,DEFNAM
C
      INCLUDE 'COMMON'
C
C     CALLED FUNCTIONS:
C
C Function to build a filename
C
      LOGICAL FILMAK
C
C Function to decode file name components
C
      LOGICAL FILDCD
C
C Function to acquire temporary directory name
C
      LOGICAL GETTMP
C
C
C Function returns non-blank string length
C
      INTEGER LNBLNK
C
C     LOCAL VARIABLES:
C
C     Buffer for reading date and time
C
      INTEGER TIMBUF(7)
C
C     Parameters for picking minutes and seconds fields
C
      INTEGER MINS, SECS
      PARAMETER (MINS=5,SECS=6)
C
C     Loop counter
C
      INTEGER I
C
C     Buffers for building file name into
C
      INTEGER TNAME(4)
      CHARACTER*255 PREFIX
      CHARACTER*80 MYNAME
      CHARACTER*30 EXTEN
C
      CHARACTER*8 ZNAME
C
C     Length of fixed portion of file name
C
      INTEGER LENGTH
C
      SEMZZZ = .TRUE.
C
C     Get the current date and time
C
      CALL MCTIME ( TIMBUF )
C
C     Put a static front bit on the file name (Z's)
C
      LENGTH = LEN(ZNAME) - 4
      DO 10 I = 1, LENGTH
         ZNAME(I:I) = 'z'
   10 CONTINUE
C
C     Make up the rest of the file name from the seconds and minutes
C     fields of the time.
C
      TNAME(1) = MOD (TIMBUF(SECS), 26)
      TNAME(2) = TIMBUF(SECS) / 26
      TNAME(3) = MOD (TIMBUF(MINS), 26)
      TNAME(4) = TIMBUF(MINS) / 26
C
      DO 20 I = 1,4
         LENGTH = LENGTH + 1
         ZNAME(LENGTH:LENGTH) = CHAR((TNAME(I)+KLCA))
   20 CONTINUE
C
      IF (FILDCD(DEFNAM,PREFIX,MYNAME,EXTEN)) GOTO 30
      I = LNBLNK(PREFIX)
C
C Find the temporary file directory if non given yet
C
      IF (I .EQ. 0) THEN
         IF (GETTMP(PREFIX)) GOTO 30
         I = LNBLNK(PREFIX)
      ENDIF
C
      IF (I .EQ. 0) THEN
         PREFIX = MYNAME
      ELSE
         PREFIX(I+1:) = MYNAME
      ENDIF
C
      I = LNBLNK(PREFIX)
      IF (I .EQ. 0) THEN
         PREFIX = EXTEN
      ELSE
         PREFIX(I+1:) = EXTEN
      ENDIF
C
      SEMZZZ = FILMAK(ZNAME,PREFIX,NAME)
C
   30 RETURN
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
