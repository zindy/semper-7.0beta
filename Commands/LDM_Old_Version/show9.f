C Semper 6 processing module SHOW9
C
      LOGICAL FUNCTION SHOW9(SYN)
C
C Scans the command descriptor table for instances of a given item
C
      INTEGER SYN(*)
C
      INTEGER IVAL
      LOGICAL SEMCON,SEMLU,SEMXA1,SHOWNL,SHOW8
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      CHARACTER*3 UNPACKF
C
      INCLUDE 'COMMON'
C
      REAL X,Y
      INTEGER I,J,K,LASTVB,LASTKY,NAME,NUMKEY,NUMOPT,NUMSYN
      INTEGER N,NVBPTR,VBPTR,UVBPTR,SAVERR
C
C Packed names
C
      INTEGER NUSAGE,NBYTE,NINTEG,NFP,NCOMPL,NERASE,NRE,NIM,NVIEW
      PARAMETER (NUSAGE=-2362)
      PARAMETER (NBYTE=4220,NINTEG=14980,NFP=10240,NCOMPL=5413)
      PARAMETER (NERASE=8721,NRE=29000,NIM=14920,NVIEW=-3566)
C
      LOGICAL FOUND
      CHARACTER*3 TEST
C
      INTEGER KEYB(LNBUF/LNINT),OPTB(LNBUF/LNINT),SYNB(LNBUF/LNINT)
      EQUIVALENCE (KEYB,RB1),(OPTB,RB2),(SYNB,RB3)
C
C This should really be done by calling SEMFVD
C
      INTEGER GENOPT(0:NGOPTS),GENKEY(0:NGKEYS)
      DATA GENOPT / 0,NERASE,NBYTE,NINTEG,NFP,NCOMPL,NRE,NIM,NVIEW /
      DATA GENKEY / 0 /
C
      SHOW9 = .TRUE.
C
C Read name from command line
C
      I = IVAL(NUSAGE)
      IF (I .EQ. 0) GOTO 100
C
      IF (SEMXA1(1,LINBUF,COMLIM,I,X,NAME)) GOTO 100
C
      IF (NAME .EQ. 0) THEN
         ERROR = 17
         GOTO 100
      ENDIF
C
      FOUND = .FALSE.
      RECORD = '''xxx'' is used in this session:'
      RECORD(2:4) = UNPACKF(NAME)
      IF (SHOWNL(RECORD(1:30))) GOTO 100
C
C Check variable table
C
      SAVERR = ERROR
      IF (SEMLU(-1,NAME,X)) THEN
         FOUND = .TRUE.
         RECORD(1:8) = ' - as a '
         IF (SEMLU(1,NAME,X)) THEN
            RECORD(9:30) = 'protected (read only)'
            I = 31
         ELSE
            IF (SEMLU(0,NAME,Y)) THEN
               RECORD(9:27) = 'fixed (always set)'
               I = 28
            ELSE
               IF (SEMLU(1,NAME,X)) GOTO 100
               I = 9
            ENDIF
         ENDIF
         RECORD(I:) = 'variable'
         IF (SEMCON(RECORD)) GOTO 100
      ENDIF
      ERROR = SAVERR
C
C Now check general options
C
      IF (NGOPTS .NE. 0) THEN
         DO 10 I = 1,NGOPTS
            IF (NAME .EQ. GENOPT(I)) THEN
               FOUND = .TRUE.
               IF (SEMCON(' - as a general option')) GOTO 100
               GOTO 30
            ENDIF
   10    CONTINUE
      ENDIF
C
C Now check general keys
C
      IF (NGKEYS .NE. 0) THEN
C        Change, so gfortran does not issue a warning
         IHACKER = MIN(1,NGKEYS)
         DO 20 I = 1,IHACKER !NGKEYS
            IF (NAME .EQ. GENKEY(I)) THEN
               FOUND = .TRUE.
               IF (SEMCON(' - as a general key')) GOTO 100
               GOTO 30
            ENDIF
   20    CONTINUE
      ENDIF
C
   30 NUMKEY = 0
      NUMOPT = 0
C
C Not found yet - scan table
C
      LASTVB = SYN(1)
      LASTKY = SYN(2)
      UVBPTR = 5
C
C Search processing command list
C
   40 NVBPTR = SYN(UVBPTR)
C
C Double test to avoid possible overflow for 16 bit integers
C
      IF (NVBPTR .LE. 0) THEN
         IF (NVBPTR .LT. -20000) THEN
            UVBPTR = NVBPTR + 25000
            IF (UVBPTR .NE. 0) GOTO 40
            IF (NUMOPT .NE. 0) THEN
               FOUND = .TRUE.
               IF (SEMCON(' - as an option to:')) GOTO 100
               IF (SHOW8(OPTB,NUMOPT)) GOTO 100
            ENDIF
            IF (NUMKEY .NE. 0) THEN
               FOUND = .TRUE.
               IF (SEMCON(' - as a key to:')) GOTO 100
               IF (SHOW8(KEYB,NUMKEY)) GOTO 100
            ENDIF
            IF (.NOT.FOUND) THEN
               IF (SEMCON(' - nowhere')) GOTO 100
            ENDIF
            SHOW9 = .FALSE.
            GOTO 100
         ENDIF
      ENDIF
C
C Decode command descriptor
C
      K = 0
      N = 0
      VBPTR = UVBPTR
      UVBPTR = UVBPTR + 1
      NUMSYN = 0
C
      IF (NVBPTR .EQ. NAME) THEN
         FOUND = .TRUE.
         IF (SEMCON(' - as a command')) GOTO 100
      ENDIF
C
C Store any synonyms and get pointer to end of command descriptor
C
   50 TEST = UNPACKF(NVBPTR)
      IF (TEST(1:1) .NE. '$') THEN
         NUMSYN = NUMSYN + 1
         SYNB(NUMSYN) = NVBPTR
      ENDIF
C
      VBPTR = VBPTR + 1
      NVBPTR = SYN(VBPTR)
C
C Double test again
C
      IF (NVBPTR.GT.0) GOTO 50
      IF (NVBPTR.GT.-20000) GOTO 50
      NVBPTR = NVBPTR+25000
C
C End of this command descriptor?
C
   60 VBPTR = VBPTR + 1
      IF (VBPTR.GE.NVBPTR) GOTO 40
C
C Pick up next item
C
      I = SYN(VBPTR)
C
C Double test again
C
      IF (I.LE.0) THEN
         IF (I.LT.-20000) GOTO 80
      ENDIF
C
C Option
C
      IF (I.NE.NUSAGE) THEN
         IF (NAME .EQ. I) THEN
            IF (NUMSYN .NE. 0) THEN
               DO 70 J = 1,NUMSYN
                  NUMOPT = NUMOPT + 1
                  OPTB(NUMOPT) = SYNB(J)
   70          CONTINUE
            ENDIF
         ENDIF
      ENDIF
      GOTO 60
C
C Pointer
C
   80 I=I+25000
      IF (I .LT. 0) THEN
C
C Routine call
C
      ELSE IF (I .EQ. 0) THEN
C
C Return from continuation
C
         VBPTR = K
         NVBPTR = N
      ELSE IF (I.LE.LASTVB) THEN
C
C Continuation command: save link
C
         K = VBPTR
         N = NVBPTR
         VBPTR = I + 1
         NVBPTR = SYN(VBPTR)+25000
      ELSE IF (I.LE.LASTKY) THEN
C
C Key
C
         IF (NAME .EQ. SYN(I)) THEN
            IF (NUMSYN .NE. 0) THEN
               DO 90 J = 1,NUMSYN
                  NUMKEY = NUMKEY + 1
                  KEYB(NUMKEY) = SYNB(J)
   90          CONTINUE
            ENDIF
         ENDIF
      ENDIF
      GOTO 60
C
C Return
C
  100 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
