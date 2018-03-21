C Semper 6 system module SEMECH
C
      LOGICAL FUNCTION SEMECH(STREAM,TEXT)
C
      INTEGER STREAM
      CHARACTER*(*) TEXT
C
C Echo text from the logical output stream given by STREAM to the
C physical output streams (standard output, standard error and any
C log files) currently enabled by the ECHO command.  Any trailing
C blanks are discarded.
C
      INTEGER LNBLNK
      LOGICAL SEMSOU,SEMSER,SEMFIL,SEMBRK
C
      INTEGER I,N
C
      INCLUDE 'COMMON'
C
      SEMECH=.TRUE.
C
C Determine significant length of text string
C
      N=LNBLNK(TEXT)
C
C Output text to standard output stream?
C
      IF (SOUFLG(STREAM)) THEN
         IF (SEMSOU(TEXT,N)) GOTO 20
      ENDIF
C
C Output text to standard error stream?
C
      IF (SERFLG(STREAM)) THEN
         IF (SEMSER(TEXT,N)) GOTO 20
      ENDIF
C
C Output text to any log files?
C
      DO 10 I=1,NDVS
         IF(MEDN(I).EQ.MEDFL) THEN
            IF (DVTYP(I).EQ.FLTTEX) THEN
               IF (FILFLG(I,STREAM)) THEN
                  IF (SEMFIL(I,TEXT,N)) GOTO 20
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE
C
C Check for abandon request
C
      IF (SEMBRK( )) GOTO 20
C
      SEMECH=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
