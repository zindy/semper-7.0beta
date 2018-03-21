C Semper 6 primitive module FILOPN
C
      LOGICAL FUNCTION FILOPN(NAM,DEF,TYPE,INTENT,VERS,
     +                        RECL,PATH,HANDLE)
C
C Opens a named text file on any available Fortran unit and returns
C this as the file handle
C
C Intent is 1 for READ
C           2 for WRITE
C           3 for READ/WRITE
C
C Vers   is ignored for READ
C
C        otherwise:
C           1 for replace (delete if applicable) old version
C           2 for fault old (exact) version if it exists
C           3 for re-use old version if it exists
C
C Recl   is record length (ignored at present)
C
C Type is 1 for character i/o, 2 for binary i/o
C
      CHARACTER*(*) NAM,DEF,PATH
      INTEGER TYPE,INTENT,VERS,RECL,HANDLE
C
      INTEGER LNBLNK
      LOGICAL FILMAK,FILSEA,SEMIOE
C
      INTEGER PATLEN,IND,IOSIOE
C
      LOGICAL*4 OPENF
      INTEGER IOS
      LOGICAL OPENED
C
      CHARACTER*255 OLDPAT
      CHARACTER*7 STATUS
C
      INCLUDE 'COMMON'
C
      FILOPN = .TRUE.
C
C Find a free Fortran unit
C
      DO 10 IND = 10,100
C
C See if this unit is open
C
         INQUIRE (UNIT=IND,OPENED=OPENF,ERR=40,IOSTAT=IOS)
         OPENED=OPENF
C
C If it is not open, try to open the file dynamically on this unit
C
         IF (.NOT.OPENED) THEN
C
C Return Fortran unit number as file handle
C
            HANDLE = IND
            GOTO 20
         ENDIF
   10 CONTINUE
C
C Out of handles
C
      ERROR = 134
      GOTO 30
C
   20 IF (INTENT .EQ. 1) THEN
C
C Open for READ - file must exist
C
         IF (FILSEA(NAM,DEF,PATH,OPENED)) GOTO 30
         IF (.NOT.OPENED) THEN
            IDMESS = NAM
            ERROR = 130
            GOTO 30
         ENDIF
C
C Construct full path
C
         PATLEN = LNBLNK(PATH)
C
C File found - now try to open READONLY
C
         OPEN (UNIT=HANDLE,FILE=PATH(1:PATLEN),STATUS='OLD',
     +         ERR=40,FORM='FORMATTED',
     +         IOSTAT=IOS)
      ELSE IF (INTENT .EQ. 2 .OR. INTENT .EQ. 3) THEN
C
C Open for write - break up supplied names
C
         IF (FILMAK(NAM,DEF,PATH)) GOTO 30
C
C Construct full path
C
         PATLEN = LNBLNK(PATH)
C
         IF (FILSEA(PATH(1:PATLEN),' ',OLDPAT,OPENED)) GOTO 30
C
         IF (OPENED) THEN
            IF (VERS .EQ. 1) THEN
C
C delete/replace old version
C
               STATUS = 'NEW'
               OPEN (UNIT=HANDLE,FILE=PATH(1:PATLEN),STATUS='OLD',
     +               FORM='FORMATTED',ERR=40,IOSTAT=IOS)
               CLOSE (UNIT=HANDLE,STATUS='DELETE',ERR=40,IOSTAT=IOS)
            ELSE IF (VERS .EQ. 3) THEN
               STATUS = 'OLD'
            ELSE
C
C (Vers = 2) fault old version
C
               IDMESS = PATH(1:PATLEN)
               ERROR = 135
               GOTO 30
            ENDIF
         ELSE
            STATUS='NEW'
         ENDIF
C
C Open for WRITE
C
         OPEN (UNIT=HANDLE,FILE=PATH(1:PATLEN),STATUS=STATUS,
     +         ERR=40,FORM='FORMATTED',
     +         IOSTAT=IOS)
      ELSE
C
C Unknown intent
C
         ERROR = INTERR
         IDERR = TYPE
         IDERR2 = INTENT
         IDMESS = 'FILOPN'
         GOTO 30
      ENDIF
C
      FILOPN = .FALSE.
   30 RETURN
C
   40 IOSIOE = IOS
      IF (SEMIOE(IOSIOE,HANDLE,PATH(1:PATLEN))) GOTO 30
      GOTO 30
C
C Copyright (C) 1989-1996:  Synoptics Ltd,  All Rights Reserved
C
      IDUMMY = RECL
      END
