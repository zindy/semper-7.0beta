C Semper 6 processing module SRDWRT
C
      SUBROUTINE SRDWRT
C     =================
C
C Provides verbs SGET, SPUT:  Reads/Writes Stanford image files.
C Reads/writes pictures from/to a sequential file that is dynamically
C opened.  The file name is supplied by means of the text key NAME.
C
      LOGICAL OPT,GETRNG,SEMTIT,SEMOPN,SEMROW
      LOGICAL FILMAK,FILSEA,FILSTR
      LOGICAL EIKEXI,EIKDEL,EIKOPE,EIKCHA,EIKLIN,EIKBYA,EIKCLO
      INTEGER IVALPN,SEMFRM,LNBLNK
C
      INTEGER CLASS,FORM,INFORM,I,J,N,NF,NPIC,NCOL,NROW,NLAY,NBYTES
      INTEGER*4 NCOL4,NROW4,NPIX4,NBIT4,NCOM4
      REAL PMIN,PMAX
C
      CHARACTER*28  DUMMY
      CHARACTER*976 TITLE
      INTEGER FD,IOP
C
      LOGICAL LWRITE
      LOGICAL EXISTS
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILE
      CHARACTER*(FILMAX) PATHNM
C
      INTEGER IB1(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1)
C
C Packed names
C
      INTEGER NTO,NFROM,NNEW,NSPUT
      PARAMETER ( NTO=-601, NFROM=10335, NNEW=22623, NSPUT=31061 )
C
      LWRITE = VERB .EQ. NSPUT
C
C Fetch file name from key NAME, prompting if key absent
C
      IF ( FILSTR ( ' ', FILE, NF, LWRITE ) ) GOTO 50
      IF ( NF .EQ. 0 ) GOTO 50
C
C Branch on whether reading or writing file
C
      IF ( LWRITE ) THEN
C
C Build full pathname, and see if file exists
C
         IF ( FILMAK ( FILE, ' ', PATHNM ) ) GOTO 50
         NF = LNBLNK ( PATHNM )
C
         IF ( EIKEXI ( PATHNM(1:NF), EXISTS ) ) GOTO 50
C
C If file already exists, delete it if NEW requested, otherwise
C raise an error.
C
         IF ( EXISTS ) THEN
            IF ( OPT ( NNEW ) ) THEN
               IF ( EIKDEL ( PATHNM(1:NF) ) ) GOTO 60
               EXISTS = .FALSE.
            ELSE
               ERROR  = 135
               IDMESS = PATHNM(1:NF)
               GOTO 50
            ENDIF
         ENDIF
         IOP = 2
      ELSE
C
C See if file exists on the path if reading
C
         IF ( FILSEA ( FILE(1:NF), ' ', PATHNM,
     +                 EXISTS ) ) GOTO 50
         IF ( EXISTS ) THEN
            NF  = LNBLNK ( PATHNM )
            IOP = 1
         ELSE
C
C Error - non-existent file requested for read
C
            ERROR  = 130
            IDMESS = FILE(1:NF)
            GOTO 50
         ENDIF
      ENDIF
C
C Try to open the file
C
      IF ( EIKOPE ( IOP, FD, PATHNM(1:NF) ) ) GOTO 70
C
C Switch code on whether reading or writing file header info.
C
      IF ( LWRITE ) THEN
C
C Code for SPUT
C -------------
C
C Fetch picture size and class
C
         NCOL = NCOLS(LP1)
         NROW = NROWS(LP1)
         NLAY = NLAYS(LP1)
         CLASS = CLASSN(LP1)
C
C Fault bad size for picture
C
         IF ( NCOL .GT. 512 .OR. NROW .GT. 512 ) THEN
            ERROR = 5
            IDERR = IVALPN(NFROM)
            GOTO 110
         ENDIF
C
C Fault bad picture class
C
         IF ( CLASS .NE. NCLIMA ) THEN
            ERROR = 6
            IDERR = IVALPN(NFROM)
            GOTO 110
         ENDIF
C
C Fault multi-layer pictures
C
         IF ( NLAY .NE. 1 ) THEN
            ERROR = 62
            IDERR = NSPUT
            GOTO 110
         ENDIF
C
C Set up output image dimensions
C
         NCOL4 = NCOL
         NROW4 = NROW
         NPIX4 = NCOL4 * NROW4
C
C Fetch picture range
C
         IF ( GETRNG ( PMIN, PMAX, LP1 ) ) GOTO 110
C
C Fault bad data range
C
         IF ( PMIN .LT. 0.0 .OR. PMAX .GT. 65535.0 ) THEN
            ERROR = 77
            IDMESS = 'Bad data range for output picture'
            GOTO 110
         ENDIF
C
C Deduce bit packing and data form from range
C
         IF ( PMAX .LE. 255.0 ) THEN
            NBIT4 = 8
            INFORM = NFMBYT
         ELSE IF ( PMAX .LE. 4095.0 ) THEN
            NBIT4 = 12
            INFORM = NFMINT
         ELSE
            NBIT4 = 16
            INFORM = NFMFP
         ENDIF
C
C Only non-composite images output
C
         NCOM4 = 0
C
C Set rest of header data to zero
C
         DO 10 I = 1,28
            DUMMY(I:I) = CHAR ( 0 )
   10    CONTINUE
C
C Fetch picture title
C
         IF ( SEMTIT ( 1, TITLE, LP1 ) ) GOTO 110
C
C Terminate title string with null
C
         N = LNBLNK ( TITLE )
C
         TITLE(N+1:N+1) = CHAR ( 0 )
C
C Output header line
C
         IF ( EIKLIN ( 2, FD, NCOL4 ) ) GOTO 90
         IF ( EIKLIN ( 2, FD, NROW4 ) ) GOTO 90
         IF ( EIKLIN ( 2, FD, NPIX4 ) ) GOTO 90
         IF ( EIKLIN ( 2, FD, NBIT4 ) ) GOTO 90
         IF ( EIKLIN ( 2, FD, NCOM4 ) ) GOTO 90
         IF ( EIKCHA ( 2, FD, DUMMY ) ) GOTO 90
         IF ( EIKCHA ( 2, FD, TITLE ) ) GOTO 90
      ELSE
C
C Code for SGET
C -------------
C
C Read header line
C
         IF ( EIKLIN ( 1, FD, NCOL4 ) ) GOTO 80
         IF ( EIKLIN ( 1, FD, NROW4 ) ) GOTO 80
         IF ( EIKLIN ( 1, FD, NPIX4 ) ) GOTO 80
         IF ( EIKLIN ( 1, FD, NBIT4 ) ) GOTO 80
         IF ( EIKLIN ( 1, FD, NCOM4 ) ) GOTO 80
         IF ( EIKCHA ( 1, FD, DUMMY ) ) GOTO 80
         IF ( EIKCHA ( 1, FD, TITLE ) ) GOTO 80
C
C Fault bad header data
C
         IF ( NCOL4 .LT. 1 .OR. NCOL4 .GT. 512 .OR.
     +        NROW4 .LT. 1 .OR. NROW4 .GT. 512 .OR.
     +        NPIX4 .NE. NCOL4 * NROW4 .OR.
     +        .NOT. ( NBIT4 .EQ. 0 .OR. NBIT4 .EQ. 8 .OR.
     +                NBIT4 .EQ. 12 .OR. NBIT4 .EQ. 16 ) .OR.
     +        NCOM4 .NE. 0 ) THEN
            ERROR = 77
            IDMESS = 'Bad file header'
            GOTO 110
         ENDIF
C
C Deduce picture form from bit packing value
C
         IF ( NBIT4 .EQ. 0 .OR. NBIT4 .EQ. 8 ) THEN
            INFORM = NFMBYT
         ELSE IF ( NBIT4 .EQ. 12 ) THEN
            INFORM = NFMINT
         ELSE
            INFORM = NFMFP
         ENDIF
C
C Open picture
C
         NPIC = IVALPN ( NTO )
         NCOL = NCOL4
         NROW = NROW4
         NLAY = 1
         CLASS = NCLIMA
         FORM = SEMFRM ( INFORM )
         LP1 = 0
         IF ( SEMOPN ( 2, NPIC, NCOL, NROW, NLAY, CLASS, FORM, LP1 ) )
     +      GOTO 110
C
C Null terminates title string - set null and rest of title to blanks
C
         N = INDEX ( TITLE, CHAR ( 0 ) )
C
         IF ( N .GT. 0 ) TITLE(N:) = ' '
C
C Store picture title
C
         IF ( SEMTIT ( 2, TITLE, LP1 ) ) GOTO 110
      ENDIF
C
C Determine number of bytes in each input/output row
C
      IF ( INFORM .EQ. NFMBYT ) THEN
         NBYTES = NCOL
      ELSE
         NBYTES = 2 * NCOL
      ENDIF
C
C Loop over rows reading or writing data
C
      DO 40 J = 1, NROW
C
C Switch code on verb name
C
         IF ( LWRITE ) THEN
C
C Read source row from LP1
C
            IF ( SEMROW ( 1, RB1, INFORM, J, 1, LP1 ) ) GOTO 110
C
C Convert unsigned floating-point value to signed 16-bit integer value
C
            IF ( INFORM .EQ. NFMFP ) THEN
               DO 20 I = 1,NCOL
                  IF ( RB1(I) .GT. 32767.0 ) THEN
                     IB1(I) = INT ( RB1(I) - 65536.0 )
                  ELSE
                     IB1(I) = INT ( RB1(I) )
                  ENDIF
   20          CONTINUE
            ENDIF
C
C Write source row to file
C
            IF ( EIKBYA ( 2, FD, RB1, NBYTES ) ) GOTO 90
         ELSE
C
C Read source row from file
C
            IF ( EIKBYA ( 1, FD, RB1, NBYTES ) ) GOTO 80
C
C Convert signed 16-bit integer value to unsigned floating-point value
C
            IF ( INFORM .EQ. NFMFP ) THEN
               DO 30 I = NCOL,1,-1
                  IF ( IB1(I) .LT. 0 ) THEN
                     RB1(I) = REAL ( IB1(I) ) + 65536.0
                  ELSE
                     RB1(I) = REAL ( IB1(I) )
                  ENDIF
   30          CONTINUE
            ENDIF
C
C Store source row in LP1
C
            IF ( SEMROW ( 2, RB1, INFORM, J, 1, LP1 ) ) GOTO 110
         ENDIF
   40 CONTINUE
C
C Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 100
C
   50 RETURN
C
C Deal with UNIX I/O errors
C
   60 ERROR  = 77
      IDMESS = 'Error deleting existing file '//PATHNM(1:NF)
      GOTO 50
C
   70 ERROR = 77
      IDMESS = 'Error opening file '//PATHNM(1:NF)
      GOTO 50
C
   80 ERROR = 77
      IDMESS = 'Error reading file '//PATHNM(1:NF)
      GOTO 50
C
   90 ERROR = 77
      IDMESS = 'Error writing file '//PATHNM(1:NF)
      GOTO 50
C
  100 ERROR  = 77
      IDMESS = 'Error closing file '//PATHNM(1:NF)
      GOTO 50
C
C Closing a file after an error (ignore any error on closing file)
C
  110 IF ( EIKCLO ( FD ) ) GOTO 50
      GOTO 50
C
C Copyright (C) 1989-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
