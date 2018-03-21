C Semper 6 processing module INPRAW
C
      SUBROUTINE INPRAW
C
C Provides command INPUT RAW:
C  Reads pictures from a sequential binary file that is
C  dynamically opened.
C
      REAL VAL
      INTEGER IVAL,IVALPN,LNBLNK,SEMFRMF !,IPACK
      LOGICAL FILSEA,FILSTR,OPT,SEMLAB,SEMLNF,SEMOPN,SEMROW,VARSET
      LOGICAL MRDBIN
C
      INTEGER CLASS,FORM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
      CHARACTER*4 DFNAM
C
      INTEGER FD,J,K,N,NF,IXFR,NROW,NLAY,NCOL
C
      REAL SKIP
      INTEGER*4 I4SKIP
C
      INTEGER SIZES(3)
      INTEGER IB1(LNBUF/LNINT),LABEL(256),TITLE(LBTT2-LBTT1+1)
C
      LOGICAL EXISTS,LSWAP,CNVRTI2,CNVRTI4
C
      EQUIVALENCE (RB1,IB1,LABEL),(TITLE,RB6)
      INTEGER*4 IB4(LNBUF/4)
      INTEGER*2 IB2(LNBUF/2)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C
C     Pick up options and mandatory keys
C
      LSWAP = OPT(31321)
      IF (VARSET(30786)) THEN
         SIZES(1) = IVAL(30786)
         SIZES(2) = SIZES(1)
         SIZES(3) = 1
         IF (VARSET(30792)) THEN
            SIZES(2) = IVAL(30792)
            IF (VARSET(30793)) THEN
               SIZES(3) = IVAL(30793)
            ENDIF
         ENDIF
      ELSE
C
C     Must have SIZE key for raw input
C
         IDERR = 30786
         ERROR = 122
         GOTO 90
      ENDIF
C
C     Fetch file name from key NAME, prompting if key absent
C
      DFNAM = '.bin'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 90
      IF (NF .EQ. 0) GOTO 90
C
C     See if file exists on the path if reading
C
      IF ( FILSEA ( FILE(1:NF), DFNAM, FILENM, EXISTS ) ) GOTO 90
      IF ( EXISTS ) THEN
         NF  = LNBLNK ( FILENM )
      ELSE
C
C     Error - non-existent file requested for read
C
         ERROR  = 130
         IDMESS = FILE(1:NF)
         GOTO 90
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 1, FD, FILENM(1:NF) ) ) GOTO 40
C
      NCOL = SIZES(1)
      NROW = SIZES(2)
      NLAY = SIZES(3)
C
      CNVRTI2=.false.
      CNVRTI4=.false.
      FORM = SEMFRMF(NFMBYT)
C     If form is negative, we need to convert
      IF(FORM.LT.0)then
        IF(FORM.EQ.-2)THEN
                FORM=NFMINT
                IXFR=2
                CNVRTI2=.true.
        ELSE
                FORM=NFMINT
                IXFR=4
                CNVRTI4=.true.
        ENDIF
      ELSE
        IF (SEMLNF(FORM,IXFR)) GOTO 80
      ENDIF
C
C     Force short integers (necessary for I4 Semper Ports e.g. Alliant)!
C
C     LDM: I don't believe this
      IF (FORM .EQ. NFMINT) then
         IXFR = 2
         CNVRTI2=.true.
      ENDIF
      IXFR = IXFR * NCOL
C
C     Skip header if SKIP is specified
C        - use I4 to enable large skips such as those found in
C          indexed TIFF files
C
      IF (VARSET(30849)) THEN
         SKIP = VAL(30849)+0.5
         I4SKIP = SKIP
   10    IF (I4SKIP .GT. 0) THEN
C
C     Read a row buffer's worth at a time
C
            IF (I4SKIP .GE. LNBUF) THEN
               K = SHORTF(LNBUF)
               IF ( EIKBYA ( 1, FD, RB1, K ) ) GOTO 70
               I4SKIP = I4SKIP - LNBUF
               GOTO 10
            ENDIF
            K = SHORTF(I4SKIP)
            IF ( EIKBYA ( 1, FD, RB1, K ) ) GOTO 70
         ENDIF
      ENDIF
      CLASS = NCLIMA
C
C     Open picture
C
      LP1 = 0
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +           FORM,LP1)) GOTO 80
C
C     Form title from input file name
C
      N = MIN(NF,LBTT2 - LBTT1)
      LABEL(LBNCTT) = N
      CALL SEMICS(FILENM,LABEL(LBTT1),N)
C
C     Update label if possible
C
      IF (N.NE.0 .AND. LBLINC) THEN
         IF (SEMLAB(2,LABEL,LP1)) GOTO 80
      ENDIF
C
C     Loop over layers
C
      DO 30 K=1,NLAY
C
C     Loop over rows
C
         DO 20 J=1,NROW
C
C     Read source row from file
C
            IF(CNVRTI2)THEN
                IF (MRDBIN(FD,IXFR,IB2,FORM,LSWAP)) GOTO 70
                DO JJ=1,NCOL
                        IB1(JJ)=IB2(JJ)
                ENDDO
            ELSE IF(CNVRTI4)THEN
                IF (MRDBIN(FD,IXFR,IB4,FORM,LSWAP)) GOTO 70
                DO JJ=1,NCOL
                        IB1(JJ)=IB4(JJ)
                ENDDO
            ELSE
                IF (MRDBIN(FD,IXFR,IB1,FORM,LSWAP)) GOTO 70
            ENDIF
C
C     Store source row in LP1
C
            IF (SEMROW(2,RB1,FORM,J,K,LP1)) GOTO 80
   20    CONTINUE
   30 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 50
      GOTO 90
C
C     Deal with UNIX I/O errors
C
   40 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 60
C
   50 IDMESS = 'Error closing file '//FILENM(1:NF)
   60 ERROR = 77
      GOTO 90
C
   70 IDMESS = 'Error reading file '//FILENM(1:NF)
      ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
   80 IF ( EIKCLO ( FD ) ) GOTO 90
C
C     All done
C
   90 RETURN
C
C Copyright (C) 1990-1995 Synoptics Ltd,  All Rights Reserved
C
      END
