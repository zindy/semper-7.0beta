C Semper 6 processing module PORDER
C
      SUBROUTINE PORDER
C
C Change priority order of program device searches
C
C Packed names
C
      INCLUDE 'COMMON'
C
      LOGICAL OPTNO,SEMCON,SEMDIA,SEMMED,VARSET
      INTEGER IVAL,PRIOS(NDVS),MEDIUM,I,IP,J,N,ND
C
      INTEGER MXORDR
      PARAMETER (MXORDR=9)
C
      INTEGER NDOLR1,NDL2,NDL3,NDL4,NDL5,NDL6,NDL7,NDL8,NDL9,NVERIF
      PARAMETER (NDOLR1=-12441,NDL2=-12473,NDL3=-12474,NDL4=-12475)
      PARAMETER (NDL5=-12476,NDL6=-12477,NDL7=-12478,NDL8=-12479)
      PARAMETER (NDL9=-12480,NVERIF=-3419)
C
      EQUIVALENCE (RB2,PRIOS)
C
      INTEGER ORDNAM(MXORDR)
      DATA ORDNAM / NDOLR1,NDL2,NDL3,NDL4,NDL5,NDL6,NDL7,NDL8,NDL9 /
C
      IF (PTNUMB .EQ. 0) THEN
         IF (SEMDIA('No program devices found',NDIWAR)) GOTO 80
      ELSE IF (PTNUMB .EQ. 1) THEN
         WRITE(RECORD,10) PTPRIO(1)
         IF (SEMDIA(RECORD,NDIWAR)) GOTO 80
C
   10    FORMAT('Only one program device (',I2,' ) found')
      ELSE
         N = 0
         ND = 1
C
C Verify device
C
   20    IF (VARSET(ORDNAM(ND))) THEN
            IP = IVAL(ORDNAM(ND))
            IF (SEMMED(IP,MEDIUM)) GOTO 80
            IDERR = IP
            IF (.NOT.(MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM)) THEN
               ERROR = 29
               GOTO 80
            ELSE IF (DVTYP(IP) .NE. FLTRUN) THEN
               ERROR = 35
               GOTO 80
            ELSE IF (N .NE. 0) THEN
C
C Has it already been quoted ?
C
               DO 30 I=1,N
                  IF (PRIOS(I) .EQ. IP) THEN
                      ERROR = 162
                      IDERR = IP
                      GOTO 80
                  ENDIF
   30          CONTINUE
            ENDIF
            N = N + 1
            PRIOS(N) = IP
            IF (ND .LT. MXORDR) THEN
               ND = ND + 1
               GOTO 20
            ENDIF
         ENDIF
C
C Here when user has exhausted himself
C
         IF (N .NE. 0) THEN
            IF (N .NE. PTNUMB) THEN
C
C Collect unspecified devices
C
               DO 50 I = 1,PTNUMB
                  IP = PTPRIO(I)
                  DO 40 J = 1,N
                     IF (IP .EQ. PRIOS(J)) GOTO 50
   40             CONTINUE
C
C Okay, this one should be included
C
                  N = N + 1
                  PRIOS(N) = IP
   50          CONTINUE
            ENDIF
C
            DO 60 I = 1,PTNUMB
               PTPRIO(I) = PRIOS(I)
   60       CONTINUE
         ENDIF
C
C Print program search order unless option NOVERIFY is set
C
         IF (.NOT.OPTNO(NVERIF)) THEN
            WRITE (RECORD,70) (PTPRIO(I),I=1,PTNUMB)
            IF (SEMCON(RECORD)) GOTO 80
         ENDIF
C
   70    FORMAT ('Program device search order is ',9(I2,:,','))
      ENDIF
C
   80 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
