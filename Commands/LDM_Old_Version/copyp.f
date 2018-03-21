C Semper 6 processing module COPYP
C
      SUBROUTINE COPYP
C
C Copies FROM,FR2 to TO ff
C  or
C Copies programs, allowing name changes during the copy
C
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL OPTNO,SEMCON,SEMKTX,SEMROW,SEMMED,SEMOPN,VARSET
      LOGICAL PRUFLS,PRUIND,PRUSLT,PRUINF,PRUCOP,PRUENT
C
      INCLUDE 'COMMON'
C
      INTEGER*4 I4N,TXTB,LABB,FRADDR,FRSIZE,NDSLOT,I41
      PARAMETER (I41=1)
C
      INTEGER I,ILL,ISL,ITL,ITY,J,K,LEN2,LENOUT
      INTEGER MEDIUM,N,N1,N2,N3,NCOL,NLAY,NROW
      INTEGER LABEL(LNLAB4)
      INTEGER OUT(PRNACT),OUT2(PRNACT)
C
      INTEGER CLASS,FORM,ERRBAK,TOFORM,DEVICE,DEV2
      LOGICAL FOUND,LPROGR,LAS,LDEV,LVERIF
C
      CHARACTER*(PRNACT) PROG,PRAS
C
C Packed name
C
      INTEGER NFROM,NFR2,NTO,NVERIF,NDEVIC,NPROGR,NAS
      PARAMETER (NFROM=10335,NFR2=10352,NTO=-601,NVERIF=-3419)
      PARAMETER (NDEVIC=6622,NPROGR=26335,NAS=2360)
C
      EQUIVALENCE (RB1,LABEL,OUT),(RB1(PRNACT+1),OUT2)
C
C See if verification required (default = yes)
C
      LVERIF=.NOT.OPTNO(NVERIF)
C
C Test for program copy first
C
      LPROGR=VARSET(NPROGR)
      LAS=VARSET(NAS)
      LDEV=VARSET(NDEVIC)
      IF (LPROGR .OR. LAS .OR. LDEV) THEN
         LENOUT=PRNACT
         IF (SEMKTX(NPROGR,'Program name (as textstring): ',
     +              OUT,LENOUT,.FALSE.)) GOTO 110
         IF (LENOUT .EQ. 0) GOTO 110
C
         CALL SEMCHS(PROG,OUT,LENOUT)
C
C If device not specified, or AS quoted get new name
C
         LAS = LAS .OR. .NOT. LDEV
         IF (LAS) THEN
            LEN2=PRNACT
            IF (SEMKTX(NAS,'New program name (as textstring): ',
     +                 OUT2,LEN2,.FALSE.)) GOTO 110
            IF (LEN2 .EQ. 0) GOTO 110
         ENDIF
C
C Find a first entry
C
         DEVICE=0
         IF (PRUENT(DEVICE,ISL,LENOUT,OUT)) GOTO 110
         IF (ISL .EQ. 0) THEN
C
C Not found ?
C
            ERROR = 125
            IDMESS = PROG(1:LENOUT)
            GOTO 110
         ENDIF
C
C
C Found source - now find out where it's going
C
         IF (LDEV) THEN
            DEV2 = IVAL(NDEVIC)
         ELSE
            DEV2 = DEVICE
         ENDIF
C
         IF (SEMMED(DEV2,MEDIUM)) GOTO 110
         IF (PROTN(DEV2) .NE. 0) THEN
            ERROR = 41
            IDERR = DEV2
            GOTO 110
         ENDIF
C
C Check new name does not already exist on device
C
         IF (.NOT.LAS) THEN
            DO 10 I = 1,LENOUT
               OUT2(I) = OUT(I)
   10       CONTINUE
            LEN2 = LENOUT
         ENDIF
C
         IF (PRUENT(DEV2,I,LEN2,OUT2)) GOTO 110
         IF (I .NE. 0) THEN
            ERROR = 126
            CALL SEMCHS(IDMESS,OUT2,LEN2)
            GOTO 110
         ENDIF
C
C Find size of source picture and space on destination
C
         IF (PRUIND(1,DEVICE,ISL,TXTB,LABB,ITY,
     +                       ITL,ILL,LENOUT,OUT)) GOTO 110
         N1 = 0
         N2 = 0
         IF (PRUSLT(DEV2,N1,N2,FRADDR,FRSIZE,NDSLOT,
     +              LEN2,OUT2)) GOTO 110
         I4N = ITL+ILL
         IF (FRSIZE .LE. I4N) THEN
            ERROR = 101
            IDERR = DEV2
            GOTO 110
         ENDIF
C
C Copy blocks
C
         IF (PRUCOP(DEVICE,DEV2,TXTB,FRADDR,ITL)) GOTO 100
         TXTB = FRADDR
         FRSIZE = FRSIZE - ITL
         FRADDR = FRADDR + ITL
         IF (PRUCOP(DEVICE,DEV2,LABB,FRADDR,ILL)) GOTO 100
         LABB = FRADDR
         FRSIZE = FRSIZE - ILL
         FRADDR = FRADDR + ILL
C
C Write directory header
C
         IF (PRUIND(2,DEV2,N1,TXTB,LABB,ITY,
     +                ITL,ILL,LEN2,OUT2)) GOTO 110
C
C Update free space info
C
         IF (PRUINF(2,DEV2,FRADDR,FRSIZE,NDSLOT)) GOTO 110
         IF (PRUFLS(0)) GOTO 110
C
         IF (LVERIF) THEN
            CALL SEMCHS(PROG,OUT,LENOUT)
            CALL SEMCHS(PRAS,OUT2,LEN2)
            WRITE(RECORD,20) PROG(1:LENOUT),PRAS(1:LEN2),DEV2
            IF (SEMCON(RECORD)) GOTO 110
         ENDIF
C
   20    FORMAT('Copied program ''',A,''' as ''',A,''' on device',I3)
C
         GOTO 110
      ENDIF
C
C Begin loop over source pictures
C
      N1=IVALPN(NFROM)
      N2=IVALPN(NFR2)
      IF (N2.LT.N1) THEN
         IDERR = NFROM
         ERROR = 3
         GOTO 110
      ENDIF
      N3=IVALPN(NTO)
      FOUND=.FALSE.
C
C Open source picture
C
      N=N1
   30 ERRBAK=0
      IF (SEMOPN(1,N,NCOL,NROW,NLAY,CLASS,FORM,LP1)) THEN
         IF (ERROR.NE.30 .OR. N1.EQ.N2) GOTO 110
         ERROR=0
         GOTO 90
      ENDIF
      FOUND=.TRUE.
C
C Find output
C
      IDERR=N3
      DEVICE=N3/1000
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 110
C
C Open output, anticipating error 27 (output to tape)
C
      LP2=LP1
      TOFORM=SEMFRM(FORM)
      IF (SEMOPN(2,N3,NCOL,NROW,NLAY,CLASS,TOFORM,LP2)) THEN
         IF (ERROR.NE.27) GOTO 110
         ERROR=0
      ENDIF
      N3=N3+1
C
C Loop over row copies
C
      I4N=2
      DO 60 K=1,NLAY
         DO 50 J=1,NROW
            IF (SEMROW(1,RB1,TOFORM,J,K,LP1)) GOTO 100
            IF (SEMROW(2,RB1,TOFORM,J,K,LP2)) GOTO 110
   50    CONTINUE
   60 CONTINUE
C
C Bump BASEWH to leave PCBs available
C
      BASEWH=CURRWH
C
C Preserve range record if same form
C
      IF (FORM .EQ. TOFORM) WSTAT(LP2)=0
C
C End of picture loop
C
   90 ERROR=ERRBAK
      IF (ERROR.EQ.0) THEN
         N=N+1
         IF (N.LE.N2) GOTO 30
C
C Nothing found?
C
         IF (.NOT.FOUND) ERROR=58
      ENDIF
      GOTO 110
C
C Error returns
C
  100 CONTINUE
C
  110 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
