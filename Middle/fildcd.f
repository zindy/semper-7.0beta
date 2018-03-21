C Semper 6 primitive module FILDCD
C
      LOGICAL FUNCTION FILDCD(PATH,PREFIX,NAME,EXTEN)
C
C Breaks the filename in PATH into components
C
      CHARACTER*(*) PATH,PREFIX,NAME,EXTEN
C
      INTEGER LNBLNK
C
C
      INCLUDE 'COMMON'
C
      INTEGER ILEN,IND,LENNAM
      LOGICAL EFOUND,PFOUND
      CHARACTER*1 CH
C
      FILDCD = .TRUE.
C
C Clear components initially
C
      PREFIX = ' '
      NAME = ' '
      EXTEN = ' '
      LENNAM = LEN(NAME)
C
      EFOUND = .FALSE.
      PFOUND = .FALSE.
      ILEN = LNBLNK(PATH)
      IND = ILEN
C      write(6,*)'Input FILDCD ',PATH(1:ILEN) !LDM
C
   10 IF (IND .GT. 0) THEN
         CH = PATH(IND:IND)
C
C Check for extension point first
C
         IF (CH .EQ. '.') THEN
C         write(6,*)'Found for ind ',ind,ilen      !LDM
C
C If not found before then note position and copy extension
C
            IF (.NOT.EFOUND) THEN
               EFOUND = .TRUE.
               IF ((ILEN-IND) .GT. LEN(EXTEN)) GOTO 80
C               write(6,*)'Setting extension ',ind,ilen !LDM
               EXTEN = PATH(IND:ILEN)
C
C Hide extension in original string and get next character
C
               IND = IND - 1
               ILEN = IND
               GOTO 10
            ENDIF
         ENDIF
C
C Now check for prefix point
C
         IF (CH .EQ. '/') THEN
C
C Copy prefix and name (if any)
C
            PFOUND = .TRUE.
            IF (IND .GT. LEN(PREFIX)) GOTO 80
            PREFIX = PATH(1:IND)
            IF ((ILEN-IND-1) .GT. LENNAM) GOTO 80
C            write(6,*)'Setting prefix ',ind,ilen
            IF (IND .LT. ILEN) NAME = PATH(IND+1:ILEN)
         ELSE
C
C Get next character
C
            IND = IND - 1
            GOTO 10
         ENDIF
      ENDIF
C
      IF (.NOT.PFOUND) THEN
         IF (ILEN .GT. LENNAM) GOTO 80
         IF (ILEN .GT. 0) NAME = PATH(1:ILEN)
      ENDIF
C
C
C      write(6,*)'Exit prefix ',prefix
C      write(6,*)'Exit extension ',exten
C      write(6,*)'Name ',name
      FILDCD = .FALSE.
C
   70 RETURN
C
C Component too large
C
   80 ERROR = 137
      GOTO 70
C
C Copyright (C) 1989-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
