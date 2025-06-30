C Semper 6 processing module OUTUNF
C
      SUBROUTINE OUTUNF(LNEW)
      LOGICAL LNEW
C
C Provides commands OUTPUT (no options):
C  Writes pictures to a Semper binary file that is dynamically opened.
C  Uses PC format for physical files for NFS compatability.
C
      INTEGER IPACK,LNBLNK
      LOGICAL FILMAK,FILSEA,FILSTR,MWRBIN,OPT,OUTNEW,SEMLNF,SEMROW
C
      INTEGER CLASS,FORM
      INTEGER*2 SWTEMP(6)
      CHARACTER*4 DFNAM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
C
      INTEGER FD,I,IXFR,J,K,L,N,NF,NROW,NLAY,NCOL
C
      INTEGER*2 IB1(LNBUF/LNINT),LABEL(256)
      INTEGER*2 TITLE(LBTT2-LBTT1+1),SLABEL(256)
C
      LOGICAL EXISTS
C
      EQUIVALENCE (RB1,IB1,LABEL),(RB4,SLABEL),(TITLE,RB6)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKCLO
C
      DFNAM = '.pic'
C
C     Fetch file name from key NAME, prompting if key absent
C
      CALL OUTDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 100
      IF (NF .EQ. 0) GOTO 100
C
C     Build full pathname, and see if file exists
C
      IF ( FILMAK ( FILE, DFNAM, FILENM ) ) GOTO 100
      NF = LNBLNK ( FILENM )
      FILE = FILENM(1:NF)
C
      IF ( FILSEA ( FILENM(1:NF), DFNAM, FILE, EXISTS ) ) GOTO 100
C
C     If file already exists, delete it if NEW requested, otherwise
C     raise an error.
C
      IF ( EXISTS ) THEN
         IF ( OUTNEW ( LNEW, FILE, FILENM(1:NF) ) ) GOTO 100
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 2, FD, FILENM(1:NF) ) ) GOTO 50
C
C     Fetch dimensions, etc.
C
      NCOL = NCOLS(LP1)
      NROW = NROWS(LP1)
      NLAY = NLAYS(LP1)
      CLASS= CLASSN(LP1)
      FORM = FORMN(LP1)
C
C     See if picture label and title available
C
      L = 0
      IF (LBLINC) THEN
         IF (.NOT.OPT(-2173)) L = 1
         N = LABEL(LBNCTT)
      ELSE
         N = 0
      ENDIF
C
      IF (SEMLNF(FORM,I)) GOTO 90
      IF (FORM .EQ. NFMINT) I = 2
      IXFR = I * NCOL
C
C     Output header line
C
      SWTEMP(1) = NCOL
      SWTEMP(2) = NROW
      SWTEMP(3) = NLAY
      SWTEMP(4) = CLASS
      SWTEMP(5) = FORM
      SWTEMP(6) = N+(L*1000)
      IF ( MWRBIN ( FD,12,SWTEMP,NFMINT,.FALSE. ) ) GOTO 80
C
C     Write picture title if available
C
      IF (N .NE. 0) THEN
C
C        Copy from LABEL as MWRBIN can destroy data
C
         DO 10 J = 1,N
            TITLE(J) = LABEL(LBNCTT+J)
   10    CONTINUE
         J = N * 2
         IF ( MWRBIN ( FD,J,TITLE,NFMINT,.FALSE. ) ) GOTO 80
      ENDIF
C
C     Write picture label if available
C
      IF (L .NE. 0) THEN
C
C        Copy from LABEL as MWRBIN can destroy data
C
         DO 20 J = 1,256
            SLABEL(J) = LABEL(J)
   20    CONTINUE
         J = 512
         IF ( MWRBIN ( FD,J,SLABEL,NFMINT,.FALSE. ) ) GOTO 80
      ENDIF
C
C     Loop over layers
C
      DO 40 K=1,NLAY
C
C        Loop over rows
C
         DO 30 J=1,NROW
C
C           Read source row from LP1
C
            IF (SEMROW(1,RB1,FORM,J,K,LP1)) GOTO 90
C
C           Write source row to file
C
            IF (MWRBIN(FD,IXFR,IB1,FORM,.FALSE.)) GOTO 80
   30    CONTINUE
   40 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 60
      GOTO 100
C
C     Deal with EIKxxx errors - first those where no EIKCLO is required
C
   50 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 70
C
   60 IDMESS = 'Error closing file '//FILENM(1:NF)
   70 ERROR = 77
      GOTO 100
C
C     Errors where a EIKCLO should be attempted
C
   80 IDMESS = 'Error writing file '//FILENM(1:NF)
      ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
   90 IF ( EIKCLO ( FD ) ) GOTO 100
C
C     All done
C
  100 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
