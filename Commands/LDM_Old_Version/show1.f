C Semper 6 subsidiary module SHOW1
C
      LOGICAL FUNCTION SHOW1(LPATH)
      LOGICAL LPATH
C
      LOGICAL SEMCON,SHOWNL,FILDIR,FILELM,GETPAT
C
      CHARACTER*2048 PATHS
      INTEGER ICOUNT,IND
C
      INCLUDE 'COMMON'
C
      SHOW1 = .TRUE.
C
      IF (LPATH) THEN
C
C Code for PATH option
C --------------------
C
         IF (SHOWNL('File search path prefix list:')) GOTO 20
         RECORD = ' '
         IF (FILDIR(RECORD(4:))) GOTO 20
         IF (SEMCON(RECORD)) GOTO 20
         IF (GETPAT(PATHS,ICOUNT)) GOTO 20
         DO 10 IND = 1,ICOUNT
            IF (FILELM(PATHS,IND,RECORD(4:))) GOTO 20
            IF (SEMCON(RECORD)) GOTO 20
   10    CONTINUE
      ENDIF
C
      SHOW1 = .FALSE.
C
   20 RETURN
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
