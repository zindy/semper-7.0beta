C Semper 6 system routine SEMIN2
C
      LOGICAL FUNCTION SEMIN2(LINE,UCLINE,FILENM,KEYWRD,DEFNAM,FOUND)
C
C UCLINE contains a (possibly pseudo) command line.
C LINE contains a lowercase version for matching
C KEYWRD is the keyword being scanned for in LINE.
C FILENM is passed back as the name to use (even if not found).
C DEFNAM is the default extension.
C FOUND indicates if the file is found anywhere on the PATH
C
C Function returns .TRUE. if serious error in FILSEA
C
      CHARACTER*(*) LINE,UCLINE,FILENM,KEYWRD,DEFNAM
      LOGICAL FOUND
      INTEGER LNBLNK
C
      LOGICAL FILSEA
C
      CHARACTER*255 TEMP,RSTR,PATHNM,OPTION
      INTEGER IND,INS,IPTR1,IPTR2
C
      SEMIN2 = .TRUE.
C
      CALL SEMIN4(UCLINE,LINE)
      RSTR= 'semper'
      INS = LNBLNK(KEYWRD)
      OPTION = '/'//KEYWRD(1:INS)
      INS = INS + 1
      IND = INDEX(LINE,OPTION(1:INS))
C      write(6,*)'Ind, ins are ',ind,ins
C      write(6,*)'Keyword ',keywrd
C      write(6,*)'Option ',option(1:INS)
      IF (IND .EQ. 0) THEN
         OPTION(1:1) = '-'
         IND = INDEX(LINE,OPTION(1:INS))
C         write(6,*)'zero length ',ind,option(1:INS) !LDM
      ENDIF
      IF (IND .NE. 0) THEN
         IPTR1 = IND+INS
         TEMP = UCLINE(IPTR1:)
C         write(6,*)' Temp is ' !LDM
C         write(6,*)TEMP !LDM
C
C Scan to end of string
C
   20    IF (TEMP(1:1) .NE. ' ' .AND.
     +       TEMP(1:1) .NE. '=') THEN
             TEMP = TEMP(2:)
             IF (LNBLNK(TEMP).NE.0) GOTO 20
         ENDIF
C
C Skip any separators
C
   30    IF (TEMP(1:1) .EQ. ' ' .OR.
     +       TEMP(1:1) .EQ. '=') THEN
             TEMP = TEMP(2:)
             IF (LNBLNK(TEMP).NE.0) GOTO 30
         ENDIF
C
C Scan for likely terminators
C
         IPTR1 = INDEX(TEMP,' ')
         IPTR2 = INDEX(TEMP,'-')
         IF (IPTR2 .NE. 0 .AND. IPTR2 .LT. IPTR1) IPTR1 = IPTR2
C
         UCLINE(IND:) = TEMP(IPTR1:)
         CALL SEMILC(UCLINE,LINE)
C         write(6,*)'Setting RSTR ', TEMP(1:IPTR1-1) !LDM
         RSTR = TEMP(1:IPTR1-1)
      ENDIF
C
C      write(6,*)'Checking File' !LDM
C      write(6,*)'Defname ',defnam !LDM
      ILEN=LNBLNK(RSTR)
C     ILEN2=LNBLNK(DEFNAM)
C     if(ILEN2 .gt. 0)RSTR(ILEN+1:ILEN+ILEN2)=defnam(1:ILEN2)
C     ILEN=ILEN+ILEN2
C      write(6,*)'Name looking for ',ILEN,RSTR(1:ILEN) !LDM
      IF (.NOT.FILSEA(RSTR(1:ILEN),DEFNAM,PATHNM,FOUND)) THEN
C         write(6,*)'In found section ',found !LDM
         IF (FOUND) THEN
            FILENM = PATHNM
         ELSE
C
C Set default name
C
            IPTR1 = LNBLNK(RSTR)
            IPTR2 = LNBLNK(DEFNAM)
            FILENM = RSTR(1:IPTR1)//DEFNAM(1:IPTR2)
         ENDIF
         SEMIN2 = .FALSE.
      ENDIF
C        write(6,*)'Pathnm ',pathnm
C        write(6,*)'Returned from semin2 ',filenm,'--' !LDM
C        write(6,*)'Status ',semin2 !LDM
      RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE SEMIN4(LINE,LCLINE)
      CHARACTER*(*) LINE,LCLINE
C
C Routine packs down multiple spaces to ease searching
C
      CHARACTER*255 TEMP
      INTEGER N,LNBLNK,I,I2
C
   10 N = LNBLNK(LINE)
      I = INDEX(LINE,'  ')
      IF (I .GT. 0 .AND. I .LT. N) THEN
         I2 = I + 2
   20    IF (LINE(I2:I2) .EQ. ' ') THEN
            I2 = I2 + 1
            GOTO 20
         ENDIF
         TEMP = LINE(I2:)
         LINE(I+1:) = TEMP
         GOTO 10
      ENDIF
      CALL SEMILC(LINE,LCLINE)
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 local system routine SEMIN5
C
      LOGICAL FUNCTION SEMIN5(LINE,UCLINE,KEYWRD,IVALUE,
     +                        MINVAL,MAXVAL,DEFVAL)
C
C UCLINE contains a (possibly pseudo) command line.
C LINE contains a lowercase version for matching
C KEYWRD is the keyword being scanned for in LINE.
C IVALUE is used to return the value.
C MINVAL,MAXVAL are the allowed range for RVALUE.
C DEFVAL is used if KEYWRD is not found.
C
C Function returns .TRUE. if value is incorrectly specified
C or out of range warning fails to write.
C
      CHARACTER*(*) LINE,UCLINE,KEYWRD
      INTEGER IVALUE,MINVAL,MAXVAL,DEFVAL
C
      INTEGER LNBLNK
C
      LOGICAL SEMDIA
C
      CHARACTER*255 TEMP,OPTION
      INTEGER*4 I4N
      INTEGER IND,INS,IPTR1,IPTR2,ICH
C
      INCLUDE 'COMMON'
C
      SEMIN5 = .TRUE.
C
      CALL SEMIN4(UCLINE,LINE)
C
      INS = LNBLNK(KEYWRD)
      OPTION = '/'//KEYWRD(1:INS)
      INS = INS + 1
      IND = INDEX(LINE,OPTION(1:INS))
      IF (IND .EQ. 0) THEN
         OPTION(1:1) = '-'
         IND = INDEX(LINE,OPTION(1:INS))
      ENDIF
C
      IF (IND .NE. 0) THEN
         IPTR1 = IND+INS
         TEMP = UCLINE(IPTR1:)
C
C Scan to end of string
C
   10    IF (TEMP(1:1) .NE. ' ' .AND.
     +       TEMP(1:1) .NE. '=') THEN
             TEMP = TEMP(2:)
             IF (LNBLNK(TEMP).NE.0) GOTO 10
         ENDIF
C
C Skip any separators
C
   20    IF (TEMP(1:1) .EQ. ' ' .OR.
     +       TEMP(1:1) .EQ. '=') THEN
             TEMP = TEMP(2:)
             IF (LNBLNK(TEMP).NE.0) GOTO 20
         ENDIF
C
C Scan for likely terminators
C
         IPTR1 = INDEX(TEMP,' ')
         IPTR2 = INDEX(TEMP,'-')
         IF (IPTR2 .NE. 0 .AND. IPTR2 .LT. IPTR1) IPTR1 = IPTR2
C
         UCLINE(IND:) = TEMP(IPTR1:)
         IPTR1 = IPTR1 - 1
         CALL SEMILC(UCLINE,LINE)
C
C TEMP(1:IPTR1) contains a string that should be all numeric
C               so scan and decode it. (Note that we can not use
C               READ(TEMP(...),*,... as it is not standard FORTRAN!!
C
         I4N = 0
         DO 40 IND = 1,IPTR1
            ICH = ICHAR(TEMP(IND:IND))
            IF (ICH .GE. KZERO .AND. ICH .LE. KNINE) THEN
               I4N = (I4N * 10) + (ICH - KZERO)
            ELSE
               WRITE(OPTION,30) KEYWRD(1:INS-1)
   30          FORMAT('Value for ',A,' must be integer')
               IF (SEMDIA(OPTION(1:INS+25),NDIWAR)) GOTO 60
               GOTO 60
            ENDIF
   40    CONTINUE
C
         IF (I4N .LT. MINVAL) THEN
            IVALUE = MINVAL
   50       FORMAT('Value for ',A,' reset to ',I4)
            WRITE(OPTION,50) KEYWRD(1:INS-1),MINVAL
            IF (SEMDIA(OPTION(1:INS+23),NDIWAR)) GOTO 60
         ELSE IF (I4N .GT. MAXVAL) THEN
            IVALUE = MAXVAL
            WRITE(OPTION,50) KEYWRD(1:INS-1),MAXVAL
            IF (SEMDIA(OPTION(1:INS+23),NDIWAR)) GOTO 60
         ELSE
            IVALUE = I4N
         ENDIF
      ELSE
         IVALUE = DEFVAL
      ENDIF
      SEMIN5 = .FALSE.
   60 RETURN
C
C Copyright (C) 1990-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
