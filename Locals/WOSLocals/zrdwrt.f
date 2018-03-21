C Semper 6 processing module ZRDWRT
C
      SUBROUTINE ZRDWRT
C
C Provides commands ZGET,ZPUT
C  Reads/writes pictures from/to a Sarastro binary file set that is
C dynamically opened.
C  The directory name is supplied by means of the text key NAME.
C
      INTEGER IVALPN
      LOGICAL FILSTR,OPT,SEMLAB,SEMOPN,SEMROW,ZCHOPF,ZFEXA
C
      INTEGER ITO,CLASS,INFORM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) DIRECT,FILENM
C
      INTEGER FD,NF,J,K,N,IOP,NROW,NLAY,NCOL
C
      INTEGER LABEL(256),TITLE(LBTT2-LBTT1+1)
C
      LOGICAL LOUTPU,LNEW
C
      EQUIVALENCE (RB1,LABEL),(TITLE,RB6)
C
C     Packed names
C
      INTEGER NNAME,NZPUT,NTO,NNEW,NFROM
      PARAMETER (NNAME=22453,NNEW=22623)
      PARAMETER (NZPUT=-10262,NTO=-601,NFROM=10335)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C
C     Fetch file set name from key NAME, prompting if key absent
C
      LOUTPU = VERB .EQ. NZPUT
C
      IF (FILSTR('Directory name',DIRECT,NF,LOUTPU)) GOTO 80
      IF (NF .EQ. 0) GOTO 80
C
C     Switch code on command name
C
      IF (LOUTPU) THEN
C
C Code for ZPUT
C -------------
         LNEW = OPT(NNEW)
         IOP = 2
C
C        Fetch dimensions, etc.
C
         NCOL = NCOLS(LP1)
         NROW = NROWS(LP1)
         NLAY = NLAYS(LP1)
         CLASS= CLASSN(LP1)
C
C        Check class is valid
C
         IF (CLASS .NE. NCLIMA) THEN
            ERROR = 6
            IDERR = IVALPN(NFROM)
            GOTO 70
         ENDIF
         INFORM = NFMBYT
      ELSE
C
C Code for ZGET
C -------------
         ITO = IVALPN(NTO)
         LNEW = .FALSE.
         IOP = 1
C
         NCOL = 256
         NROW = 256
         IF (ZCHOPF(DIRECT,NLAY)) GOTO 80
         CLASS = NCLIMA
         INFORM = NFMBYT
C
C        Open picture
C
         LP1=0
         IF (SEMOPN(2,ITO,NCOL,NROW,NLAY,CLASS,INFORM,LP1)) GOTO 70
C
C        Form title from input file name set
C
         N = MIN(NF,LBTT2 - LBTT1)
         LABEL(LBNCTT) = N
         CALL SEMICS(DIRECT,LABEL(LBTT1),N)
C
C        Update label if possible
C
         IF (N.NE.0 .AND. LBLINC) THEN
            IF (MEDN(DEVN(LP1)).NE.MEDTP) THEN
               IF (SEMLAB(2,LABEL,LP1)) GOTO 70
            ENDIF
         ENDIF
      ENDIF
C
C     Loop over layers
C
      DO 20 K=1,NLAY
         IF (ZFEXA(DIRECT,K,FILENM,NF,LOUTPU,LNEW)) GOTO 80
C
C        Try to open the file
C
         IF ( EIKOPE ( IOP, FD, FILENM(1:NF) ) ) GOTO 30
C
C        Loop over rows
C
         DO 10 J=1,NROW
C
C        Switch code on command name
C
            IF (LOUTPU) THEN
C
C              Read source row from LP1
C
               IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 70
C
C              Write source row to file
C
               IF (EIKBYA(IOP,FD,RB1,NCOL)) GOTO 40
            ELSE
C
C              Read source row from file
C
               IF (EIKBYA(IOP,FD,RB1,NCOL)) GOTO 40
C
C              Store source row in LP1
C
               IF (SEMROW(2,RB1,INFORM,J,K,LP1)) GOTO 70
            ENDIF
   10    CONTINUE
C
C        Close file
C
         IF ( EIKCLO ( FD ) ) GOTO 50
   20 CONTINUE
      GOTO 80
C
   30 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 60
C
   40 IF ( IOP .EQ. 1 ) THEN
         IDMESS = 'Error reading file '//FILENM(1:NF)
      ELSE
         IDMESS = 'Error writing file '//FILENM(1:NF)
      ENDIF
      ERROR = 77
      GOTO 70
C
   50 IDMESS = 'Error closing file '//FILENM(1:NF)
   60 ERROR = 77
      GOTO 80
C
C     Closing a file after an error (ignore any error on closing file)
C
   70 IF ( EIKCLO ( FD ) ) GOTO 80
C
C     All done
C
   80 RETURN
C
C Copyright (C) 1990,1992 Synoptics Ltd,  All Rights Reserved
C
      END
C Function ZFEXA
C
      LOGICAL FUNCTION ZFEXA(DIRECT,N,FILENM,NF,LOUT,LNEW)
      CHARACTER*(*) DIRECT,FILENM
      INTEGER N,NF
      LOGICAL LOUT,LNEW
C
      INTEGER LNBLNK
      LOGICAL EIKEXI,EIKDEL,FILMAK,FILSEA
C
      INCLUDE 'COMMON'
C
      LOGICAL EXISTS
      CHARACTER*80 FILE
      CHARACTER*5 SERIAL
      CHARACTER*1 DFNAM
C
      ZFEXA = .TRUE.
      DFNAM = ' '
C
      IF (N .LT. 10) THEN
         WRITE(SERIAL,10) N
   10    FORMAT(I1)
      ELSE IF (N .LT. 100) THEN
         WRITE(SERIAL,20) N
   20    FORMAT(I2)
      ELSE IF (N .LT. 1000) THEN
         WRITE(SERIAL,30) N
   30    FORMAT(I3)
      ELSE IF (N .LT. 10000) THEN
         WRITE(SERIAL,40) N
   40    FORMAT(I4)
      ELSE
         WRITE(SERIAL,50) N
   50    FORMAT(I5)
      ENDIF
C
      NF = LNBLNK(DIRECT)
      FILE = DIRECT(1:NF)//'/'//SERIAL
      NF = LNBLNK(FILE)
C
C     See if file exists
C
      IF (LOUT) THEN
C
C        Build full pathname, and see if file exists
C
         IF ( FILMAK ( FILE, DFNAM, FILENM ) ) GOTO 60
         NF = LNBLNK ( FILENM )
C
         IF ( EIKEXI ( FILENM(1:NF), EXISTS ) ) GOTO 60
C
C        If file already exists, delete it if NEW requested,
C        otherwise raise an error.
C
         IF ( EXISTS ) THEN
            IF ( LNEW ) THEN
               IF ( EIKDEL ( FILENM(1:NF) ) ) GOTO 70
               EXISTS = .FALSE.
            ELSE
               ERROR  = 135
               IDMESS = FILENM(1:NF)
               GOTO 60
            ENDIF
         ENDIF
      ELSE
C
C        See if file exists on the path if reading
C
         IF ( FILSEA ( FILE(1:NF), DFNAM, FILENM,
     +                    EXISTS ) ) GOTO 60
         IF ( EXISTS ) THEN
            NF  = LNBLNK ( FILENM )
         ELSE
C
C           Error - non-existent file requested for read
C
            ERROR  = 130
            IDMESS = FILE(1:NF)
            GOTO 60
         ENDIF
      ENDIF
      ZFEXA = .FALSE.
   60 RETURN
C
C     Deal with UNIX I/O errors
C
   70 IDMESS = 'Error deleting existing file '//FILENM(1:NF)
      ERROR = 77
      GOTO 60
C
C Copyright (C) 1990 Synoptics Ltd,  All Rights Reserved
C
      END
C Function ZCHOPF
C
      LOGICAL FUNCTION ZCHOPF(DIRECT,N)
      CHARACTER*(*) DIRECT
      INTEGER N
C
      LOGICAL ZFEXA
C
      INCLUDE 'COMMON'
C
      INTEGER BSTEP,NF,SAVERR
      LOGICAL UP,FOUND
      CHARACTER*80 FILENM
C
      SAVERR = ERROR
      N = 4095
      UP = .FALSE.
      FOUND = .FALSE.
      ZCHOPF = .TRUE.
   10 ERROR = 0
      IF (ZFEXA(DIRECT,N,FILENM,NF,.FALSE.,.FALSE.)) THEN
         IF (ERROR .NE. 130) GOTO 30
         IF (FOUND) THEN
            UP = .FALSE.
            IF (BSTEP .EQ. 1) THEN
               N = N - 1
               GOTO 20
            ENDIF
            BSTEP = BSTEP - (BSTEP/2)
         ELSE
            IF (N .EQ. 1) THEN
               IDMESS = 'No files found for '//DIRECT
               ERROR = 77
               GOTO 30
            ELSE
               N = N / 2
               GOTO 10
            ENDIF
         ENDIF
      ELSE
         UP = .TRUE.
         IF (FOUND) THEN
            IF (BSTEP .NE. 1) BSTEP = BSTEP - (BSTEP/2)
         ELSE
            FOUND = .TRUE.
            BSTEP = N - (N/2)
         ENDIF
      ENDIF
      IF (UP) THEN
         N = N + BSTEP
      ELSE
         N = N - BSTEP
      ENDIF
      GOTO 10
C
   20 ZCHOPF = .FALSE.
      ERROR = SAVERR
   30 RETURN
C
C Copyright (C) 1990 Synoptics Ltd,  All Rights Reserved
C
      END
