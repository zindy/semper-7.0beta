C Semper 6 subsidiary module SHOW7
C
      LOGICAL FUNCTION SHOW7(LECHO)
      LOGICAL LECHO
C
      LOGICAL SEMCON,SEMTPS,SHOWNL
C
      INCLUDE 'COMMON'
C
      INTEGER DEVICE,I,N,IW,IL
      LOGICAL ECHFLG(6)
C
      SHOW7 = .TRUE.
C
      IF (LECHO) THEN
C
C Code for ECHO status option
C ---------------------------
C
         IF (SHOWNL('Echo to standard output (terminal):')) GOTO 90
         CALL SHOW4(SOUFLG,RECORD,N)
         IF (SEMCON(RECORD(1:N))) GOTO 90
         IF (SHOWNL('Echo to standard error:')) GOTO 90
         CALL SHOW4(SERFLG,RECORD,N)
         IF (SEMCON(RECORD(1:N))) GOTO 90
         DO 30 DEVICE=1,NDVS
            IF (MEDN(DEVICE).EQ.MEDFL) THEN
               IF (DVTYP(DEVICE).EQ.FLTTEX) THEN
                  WRITE (RECORD,10) DEVICE
   10             FORMAT ('Echo to device:',I4)
                  IF (SEMCON(RECORD)) GOTO 90
                  DO 20 I=1,6
                     ECHFLG(I) = FILFLG(DEVICE,I)
   20             CONTINUE
                  CALL SHOW4(ECHFLG,RECORD,N)
                  IF (SEMCON(RECORD(1:N))) GOTO 90
               ENDIF
            ENDIF
   30    CONTINUE
      ELSE
C
C Code for terminal PAGE settings
C -------------------------------
C
         IF (SEMTPS(IW,IL)) GOTO 90
         IF (SHOWNL('Terminal page settings:')) GOTO 90
         WRITE (RECORD,40) TERWID
   40    FORMAT ('   User defined page width  ',I4)
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,50) IW
   50    FORMAT ('        Current page width  ',I4)
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,60) TERLEN
   60    FORMAT ('   User defined page length ',I4)
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,70) IL
   70    FORMAT ('        Current page length ',I4)
         IF (SEMCON(RECORD)) GOTO 90
         WRITE (RECORD,80) TERASP
   80    FORMAT ('   Aspect ratio ',F6.2)
         IF (SEMCON(RECORD)) GOTO 90
C
         RECORD(4:19) = 'Terminal output '
         IF (TERWRA) THEN
            RECORD(20:31) = 'wraps-around'
         ELSE
            RECORD(20:31) = 'is truncated'
         ENDIF
         IF (SEMCON(RECORD(1:31))) GOTO 90
         RECORD(4:25) = 'Page prompt is turned '
         IF (TERPRO) THEN
            RECORD(26:29) = 'on'
         ELSE
            RECORD(26:29) = 'off'
         ENDIF
         IF (SEMCON(RECORD(1:29))) GOTO 90
         RECORD(4:38) = 'Quit in response to page prompt is '
         IF (TERQUI) THEN
            RECORD(39:46) = 'enabled'
         ELSE
            RECORD(39:46) = 'disabled'
         ENDIF
         IF (SEMCON(RECORD(1:46))) GOTO 90
      ENDIF
C
      SHOW7 = .FALSE.
C
   90 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
