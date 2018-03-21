C Semper 6 system module SEMEND
C
      SUBROUTINE SEMEND
C
C     INTEGER LNBLNK
      LOGICAL EQSETS,EQTERM,FSFL61,FSEX61,OPT,SEMDIA,SEMSIG
      LOGICAL SEMDCR,SEMLU,SEMRET,SEMUNW,SEMTFL,SEMTCR,SEMTLF
      LOGICAL DISC
      CHARACTER*6 EXNAME
      CHARACTER*120 MESS
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INTEGER*4 I4ZERO,I4ONE
      PARAMETER (I4ZERO=0,I4ONE=1)
C
      INTEGER IDEV,MEDIUM,WORK(LNWORK+1)
      EQUIVALENCE (WORK,RB5)
C
      INTEGER NDEVIC,NVERIF,NDEASS
      PARAMETER (NDEVIC=6622,NVERIF=-3419,NDEASS=6601)
C
      ERROR = 0
      VERB = 0
C
C Shut down event management
C
      EXNAME = 'EQSETS'
      IF (EQSETS(MPOINT,QCLOSE)) CALL ENDERR(EXNAME)
      IF (EQSETS(MKEY,QCLOSE))   CALL ENDERR(EXNAME)
      IF (EQSETS(MBUT,QCLOSE))   CALL ENDERR(EXNAME)
C
C Close all libraries
C
      IF (SEMRET(0)) CALL ENDERR('SEMRET')
C
C Dump current row
C
      IF (SEMDCR(0)) CALL ENDERR('SEMDCR')
C
C Destroy all local variables
C
      IF (SEMUNW(0)) CALL ENDERR('SEMUNW')
C
C Make device variable local
C
      EXNAME = 'SEMLU'
      IF (SEMLU(3,NDEVIC,0.0)) CALL ENDERR(EXNAME)
C
C Unless VERIFY is set to YES, set VERIFY to NO
C
      IF (.NOT.OPT(NVERIF)) THEN
         IF (SEMLU(2,NVERIF,0.0)) CALL ENDERR(EXNAME)
      ENDIF
C
      VERB = NDEASS
      DO 10 IDEV = 1,NDVS
C
C Deassign all devices
C
         MEDIUM = MEDN(IDEV)
         IF (MEDIUM .NE. 0) THEN
            IF (MEDIUM .EQ. MEDDS) THEN
               IF (FSFL61(2,ERROR)) CALL ENDERR('FSFL61')
            ENDIF
            IF (SEMLU(1,NDEVIC,REAL(IDEV))) CALL ENDERR(EXNAME)
            CALL DEASS
            IF (ERROR .NE. 0) CALL ENDERR('DEASS')
         ENDIF
   10 CONTINUE
C
C Signal termination exit
C
      IF (SEMSIG(SSIGEX,MESS)) THEN
         CALL ENDERR('SEMSIG')
         IF (SEMDIA(MESS,NDIERR)) THEN
            ERROR = 0
         ENDIF
      ENDIF
C
C Terminate the session
C
C ****** CHANGE ******
C Insert here any code needed locally at session termination
C (e.g. by the primitive modules, or for file closure)
C
C Deassign and delete the work buffer
C
C
C Work buffer = virtual memory array
C
C Free the memory associated with the work buffer
C
      CALL MCDC61(10,0,I4ZERO,0,RB1,ERROR)
      IF (ERROR .NE. 0) CALL ENDERR('MCDC61')
C
C Shut down disc cache - flushes out all data from the disc cache and
C frees all the memory allocated for cache table and buffers
C
      IF (DISC(4,0,I4ZERO,RB1,I4ZERO,0,0)) CALL ENDERR('DISC')
C
C Shut down framestore port
C
      IF (FSEX61(ERROR)) CALL ENDERR('FSEX61')
C
C Safe to close break queue now
C
      IF (EQSETS(MBREAK,QCLOSE)) CALL ENDERR('EQSETS')
      IF (EQTERM(1))             CALL ENDERR('EQTERM')
C
C Force start-of-line
C
      IF (.NOT.TERCR.AND.TERXCR) THEN
         IF (SEMTCR()) CALL ENDERR('SEMTCR')
      ENDIF
C
      IF (.NOT.TERLF.AND.TERXLF) THEN
         IF (SEMTLF()) CALL ENDERR('SEMTLF')
      ENDIF
C
C Flush out all output to terminal
C
      IF (SEMTFL()) CALL ENDERR('SEMTFL')
C
C (Silently) close known I/O units
C
      CLOSE(RUNFLE,ERR=20)
   20 CLOSE(RDWRTU,ERR=30)
   30 CLOSE(RDWRT2,ERR=40)
   40 CLOSE(ERMESU,ERR=50)
   50 CONTINUE
C
C Terminate execution of Semper
C
      CALL SEMXIT(.FALSE.)
C
C Final STOP - just in case
C
      STOP
C
C ****** ****** ******
C
C Copyright (C) 1987,1988,1989,1991:  Synoptics Ltd, All Rights Reserved
C
      END
C
      SUBROUTINE ENDERR(STRING)
C
      CHARACTER*(*) STRING
C
      LOGICAL SEMDIA
C
      INCLUDE 'COMMON'
C
      INTEGER N,LNBLNK
C
      WRITE(RECORD,10) ERROR,STRING
   10 FORMAT('Error ',I3,' during termination call to ',A)
      IF (SEMDIA(RECORD,NDIFAT)) GOTO 20
C
      IF (ERROR .NE. 0) THEN
         CALL SEMERR(RECORD)
         N = LNBLNK(RECORD)
         IF (N .NE. 0) THEN
            IF (SEMDIA(RECORD(1:N),NDIFAT)) GOTO 20
         ENDIF
      ENDIF
C
   20 ERROR = 0
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE SEMXIT ( FAIL )
C
C     ==========================
C
      LOGICAL FAIL
C
      INTEGER*4 I4ZERO,I4ONE
      PARAMETER (I4ZERO=0,I4ONE=1)
C
C
C X11 shut down
C
      CALL SX11EX
      CALL WAITS(3.0)
      IF (.NOT.FAIL) THEN
         CALL EXIT(I4ZERO)
      ELSE
         CALL EXIT(I4ONE)
      ENDIF
      STOP
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
