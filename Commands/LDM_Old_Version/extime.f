C Semper 6 processing module EXTIME
C
      SUBROUTINE EXTIME
C
C Prints elapsed time to the terminal (unless option NOVERIFY is set).
C Time reference is stored in array DTBUFF in common block SEMTIM.
C The array is initialised at start-up by SEMINI and subsequently by
C this command if option RESET is set.  Also sets variable T to elapsed
C time in seconds.
C
      LOGICAL OPT,OPTNO,SEMCON,SEMLU
C
      INTEGER DATIME(7),IHOUR,IMINU,ISECO,ICSEC
      REAL    TIME
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NRESET,NVERIF,NT
      PARAMETER (NRESET=29019, NVERIF=-3419, NT=-1)
C
C If option RESET is set, re-initialise time reference array DTBUFFC
C
      IF (OPT(NRESET)) THEN
         CALL MCTIME(DTBUFF)
C
C Otherwise, print elapsed time
C
      ELSE
C
C Fetch current time
C
         CALL MCTIME(DATIME)
C
C Determine time difference, ignoring date information
C
         IHOUR=DATIME(4)-DTBUFF(4)
         IMINU=DATIME(5)-DTBUFF(5)
         ISECO=DATIME(6)-DTBUFF(6)
         ICSEC=DATIME(7)-DTBUFF(7)
C
C Make necessary adjustment for time wrap-around at midnight
C
         IF (IHOUR.LT.0) IHOUR=IHOUR+24
C
C Determine total time difference in seconds
C
         TIME=60.0*REAL(60*IHOUR+IMINU)+REAL(ISECO)+REAL(ICSEC)/100.0
C
C Print elasped time to console output stream (unless NOVERIFY option
C is set)
C
         IF (.NOT.OPTNO(NVERIF)) THEN
            WRITE (RECORD,10) TIME
            IF (SEMCON(RECORD)) RETURN
         ENDIF
C
C Set variable T to elased time in seconds
C
         IF (SEMLU(1,NT,TIME)) RETURN
      ENDIF
C
      RETURN
C
   10 FORMAT ('Time =',F10.2,' seconds')
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
