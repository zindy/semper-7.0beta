C Semper 6 primitive module FILDIR
C
      LOGICAL FUNCTION FILDIR(PREFIX)
C
C Returns the current device/directory prefix
C
      CHARACTER*(*) PREFIX
C
      CHARACTER*255 CWD
      INTEGER LNBLNK,I
C
      INCLUDE 'COMMON'
C
      CWD=' '
      CALL GETCWD(CWD)
C
      IEND=255
10    I = LNBLNK(CWD(1:IEND))
C     Trap junk at the end of the string
      ILAST = ICHAR(CWD(I:I))
      IF((IEND.GT.1) .and. (ILAST.EQ.0))then
        IEND=IEND-1
        GOTO 10
      ENDIF
C      write(6,*)'Got ',i,'file ',cwd(1:i)
      ii=ichar(cwd(i:i))
C      write(6,*)'Terminator ''',cwd(i:i),''' int ',ii
      IF (LEN(PREFIX) .LT. I) THEN
         ERROR = 137
         FILDIR = .TRUE.
      ELSE
         IF (CWD(I:I) .NE. '/') THEN
            I = I + 1
            CWD(I:I) = '/'
         ENDIF
         PREFIX = CWD(1:I)
         FILDIR = .FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1989-1994:  Synoptics Ltd,  All Rights Reserved
C
      END
