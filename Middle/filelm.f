C Semper 6 primitive module FILELM
C
      LOGICAL FUNCTION FILELM(PATHS,ELEMNT,PREFIX)
C
C Returns element ELEMNT of PATHS in PREFIX, function yields TRUE
C if ELEMNT is out of range
C
      CHARACTER*(*) PATHS,PREFIX
      INTEGER ELEMNT
C
      INTEGER ICNT,IND,ILEN,PRELEN,PREPTR
      CHARACTER*1 CH
C
      INCLUDE 'COMMON'
C
      INTEGER LNBLNK
C
C
      FILELM = .TRUE.
      PREFIX = ' '
      ICNT = ELEMNT
C
      PRELEN = LEN(PREFIX)
      ILEN = LNBLNK(PATHS)
      IND = 1
   10 IF (IND .LE. ILEN) THEN
         IF (ICNT .LE. 1) THEN
C
C Copy this element
C
            PREPTR = 1
   20       CH = PATHS(IND:IND)
            IND = IND + 1
            IF (CH .NE. ';') THEN
               PREFIX(PREPTR:PREPTR) = CH
               PREPTR = PREPTR + 1
               IF (PREPTR .GT. PRELEN) THEN
                  IDMESS = 'Internal error in FILELM - element too long'
                  ERROR = 77
                  GOTO 40
               ENDIF
               IF (IND .LE. ILEN) GOTO 20
            ENDIF
            FILELM = .FALSE.
C
C Check prefix ends with suitable character
C
            CH = PREFIX(PREPTR-1:PREPTR-1)
            IF (CH .NE. '/') THEN
               PREFIX(PREPTR:) = '/'
            ENDIF
         ELSE
C
C Skip this element
C
   30       CH = PATHS(IND:IND)
            IND = IND + 1
            IF (CH .NE. ';' .AND. IND .LE. ILEN) GOTO 30
            ICNT = ICNT - 1
            GOTO 10
         ENDIF
      ELSE
         IDMESS = 'Internal error in FILELM - path exhausted'
         ERROR = 77
      ENDIF
C
   40 RETURN
C
C Copyright (C) 1989-1992 :  Synoptics Ltd,  All Rights Reserved
C
      END
