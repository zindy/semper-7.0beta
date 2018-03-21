C Semper 6 processing module OUTRAW
C
      SUBROUTINE OUTRAW(LNEW)
      LOGICAL LNEW
C
C Provides commands OUTPUT RAW:
C  Writes pictures to a sequential binary file that is
C  dynamically opened.
C
      INTEGER IPACK,IVALPN,LNBLNK
      LOGICAL FILMAK,FILSEA,FILSTR,MWRBIN,OPT,OUTNEW,SEMLNF,SEMROW
C
      INTEGER CLASS,FORM
      CHARACTER*4 DFNAM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
C
      INTEGER FD,J,K,IXFR,NF,NROW,NLAY,NCOL
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
      INTEGER*2 TITLE(LBTT2-LBTT1+1)
C
      LOGICAL EXISTS,LSWAP
C
      EQUIVALENCE (RB1,IB1,LABEL),(TITLE,RB6)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKCLO
C
      LSWAP = OPT(31321)
C
C Fetch dimensions, etc.
C
      NCOL = NCOLS(LP1)
      NROW = NROWS(LP1)
      NLAY = NLAYS(LP1)
      CLASS= CLASSN(LP1)
      IF (CLASS .NE. NCLIMA) GOTO 60
      FORM = FORMN(LP1)
C
      IF (SEMLNF(FORM,IXFR)) GOTO 80
C
C Force short integers
C
C LDM: I don't believe this is correct
C      IF (FORM .EQ. NFMINT) IXFR = 2
      IXFR = IXFR * NCOL
C
      DFNAM = '.bin'
C
C     Fetch file name from key NAME, prompting if key absent
C
      CALL OUTDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 90
      IF (NF .EQ. 0) GOTO 90
C
C     Build full pathname, and see if file exists
C
      IF ( FILMAK ( FILE, DFNAM, FILENM ) ) GOTO 90
      NF = LNBLNK ( FILENM )
      FILE = FILENM(1:NF)
C
      IF ( FILSEA ( FILENM(1:NF), DFNAM, FILE, EXISTS ) ) GOTO 90
C
C     If file already exists, delete it if NEW requested, otherwise
C     raise an error.
C
      IF ( EXISTS ) THEN
         IF ( OUTNEW ( LNEW, FILE, FILENM(1:NF) ) ) GOTO 90
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 2, FD, FILENM(1:NF) ) ) GOTO 30
C
C Loop over layers
C
      DO 20 K=1,NLAY
C
C Loop over rows
C
         DO 10 J=1,NROW
C
C Read source row from LP1
C
            IF (SEMROW(1,RB1,FORM,J,K,LP1)) GOTO 80
C
C Write source row to file
C
            IF (MWRBIN(FD,IXFR,IB1,FORM,LSWAP)) GOTO 70
   10    CONTINUE
   20 CONTINUE
C
C Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 40
      GOTO 90
C
C     Deal with EIKxxx errors - first those where no EIKCLO is required
C
   30 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 50
C
   40 IDMESS = 'Error closing file '//FILENM(1:NF)
   50 ERROR = 77
      GOTO 90
C
C     Errors where a EIKCLO should be attempted
C
C     Wrong class for output
C
   60 ERROR = 6
      IDERR = IVALPN(10335)
      GOTO 80
C
   70 IDMESS = 'Error writing file '//FILENM(1:NF)
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
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
