C Semper 6 processing module INPUNF
C
      SUBROUTINE INPUNF
C
C Provides command INPUT:
C  Reads pictures from a sequential Semper binary file that is
C  dynamically opened.
C  Uses PC format for physical files for NFS compatability.
C
      INTEGER IVALPN,SEMFRM,LNBLNK !,IPACK
      LOGICAL FILSEA,FILSTR,SEMLAB,SEMLNF,SEMOPN,SEMROW
      LOGICAL MRDBIN
C
      INTEGER CLASS,FORM
      INTEGER*2 SWTEMP(6)
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILE,FILENM
      CHARACTER*4 DFNAM
C
      INTEGER FD,J,K,L,N,NF,IXFR,NROW,NLAY,NCOL
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
      INTEGER*2 TITLE(LBTT2-LBTT1+1)
C
      LOGICAL EXISTS
C
      EQUIVALENCE (RB1,IB1,LABEL),(TITLE,RB6)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKCLO
C
C     Fetch file name from key NAME, prompting if key absent
C
      DFNAM = '.pic'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 80
      IF (NF .EQ. 0) GOTO 80
C
C     See if file exists on the path if reading
C
      IF ( FILSEA ( FILE(1:NF), DFNAM, FILENM, EXISTS ) ) GOTO 80
      IF ( EXISTS ) THEN
         NF  = LNBLNK ( FILENM )
      ELSE
C
C        Error - non-existent file requested for read
C
         ERROR  = 130
         IDMESS = FILE(1:NF)
         GOTO 80
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 1, FD, FILENM(1:NF) ) ) GOTO 30
C
C Semper 6 style Binary dump form
C
      IF ( MRDBIN ( FD,12,SWTEMP,NFMINT,.FALSE. ) ) GOTO 60
      NCOL  = SWTEMP(1)
      NROW  = SWTEMP(2)
      NLAY  = SWTEMP(3)
      CLASS = SWTEMP(4)
      FORM  = SWTEMP(5)
      N     = SWTEMP(6)
      IF (SEMLNF(FORM,IXFR)) GOTO 70
C
C Force short integers
C
      IF (FORM .EQ. NFMINT) IXFR = 2
      IXFR = IXFR * NCOL
      L = N/1000
      N = N - (L*1000)
C
C Open picture
C
      LP1=0
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +             SEMFRM(FORM),LP1)) GOTO 70
C
C Read title if present
C
      IF (N .NE. 0) THEN
         J = N * 2
         IF ( MRDBIN ( FD, J, LABEL(LBNCTT+1),
     +                 NFMINT, .FALSE. ) ) GOTO 60
         LABEL(LBNCTT) = N
      ENDIF
C
C Read picture label if present
C
      IF (L.NE.0) THEN
         IF ( MRDBIN ( FD, 512, LABEL,
     +                 NFMINT, .FALSE. ) ) GOTO 60
      ENDIF
C
C Update label if possible
C
      IF ((N.NE.0.OR.L.NE.0) .AND. LBLINC) THEN
         IF (SEMLAB(2,LABEL,LP1)) GOTO 70
      ENDIF
C
C Loop over layers
C
      DO 20 K=1,NLAY
C
C Loop over rows
C
         DO 10 J=1,NROW
C
C Read source row from file
C
            IF (MRDBIN(FD,IXFR,IB1,FORM,.FALSE.)) GOTO 60
C
C Store source row in LP1
C
            IF (SEMROW(2,RB1,FORM,J,K,LP1)) GOTO 70
   10    CONTINUE
   20 CONTINUE
C
C Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 40
      GOTO 80
C
C     Deal with UNIX I/O errors
C
   30 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 50
C
   40 IDMESS = 'Error closing file '//FILENM(1:NF)
   50 ERROR = 77
      GOTO 80
C
   60 IDMESS = 'Error reading file '//FILENM(1:NF)
      ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
   70 IF ( EIKCLO ( FD ) ) GOTO 80
C
C     All done
C
   80 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
