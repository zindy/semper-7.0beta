C Semper 6 processing module ERDWRT
C
      SUBROUTINE ERDWRT
C     =================
C
C Provides verbs EGET, EPUT:  Reads/Writes Type 2 EIKONIX picture files.
C Reads/writes pictures from/to a sequential file that is dynamically
C opened.  The file name is supplied by means of the text key NAME.
C
      LOGICAL FILMAK,FILSEA,FILSTR,OPT,SEMLAB,SEMOPN,SEMROW
      INTEGER IVALPN,SEMFRM,LNBLNK
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,I,IC0,J,K,L,NCOL,NF,NLAY,NROW
      INTEGER NBPPIX,IDATE(6)
C
      CHARACTER*18 CDATE
      CHARACTER*28 ETITLE,CTITLE
      INTEGER FD,IOP
C
      LOGICAL LWRITE,EXISTS
C
      CHARACTER*(FILMAX) FILE,PATHNM
C
      LOGICAL*1 LB1(LNBUF/LNINT)
      INTEGER LABEL(256)
C
      EQUIVALENCE (RB1,LB1,LABEL)
C
C     Packed names
C
      INTEGER NEPUT,NTO,NFROM,NNEW
      PARAMETER (NEPUT=8661,NTO=-601,NFROM=10335,NNEW=22623)
C
C     'C' functions
C
      LOGICAL EIKEXI,EIKDEL,EIKOPE,EIKCHA,EIKINT,EIKBYA,EIKCLO
C
C Initialise ETITLE
C
      ETITLE(1:27) = 'EIKONIX Picture File Type 2'
      ETITLE(28:28) = CHAR(0)
C
      IC0 = ICHAR('0')
C
      LWRITE = VERB .EQ. NEPUT
C
C     Fetch file name from key NAME, prompting if key absent
C
      IF ( FILSTR ( ' ', FILE, NF, LWRITE ) ) GOTO 80
      IF ( NF .EQ. 0 ) GOTO 80
C
C     Branch on whether reading or writing file
C
      IF ( LWRITE ) THEN
C
C        Build full pathname, and see if file exists
C
         IF ( FILMAK ( FILE, ' ', PATHNM ) ) GOTO 80
         NF = LNBLNK ( PATHNM )
C
         IF ( EIKEXI ( PATHNM(1:NF), EXISTS ) ) GOTO 80
C
C        If file already exists, delete it if NEW requested, otherwise
C        raise an error.
C
         IF ( EXISTS ) THEN
            IF ( OPT ( NNEW ) ) THEN
               IF ( EIKDEL ( PATHNM(1:NF) ) ) GOTO 30
               EXISTS = .FALSE.
            ELSE
               ERROR  = 135
               IDMESS = PATHNM(1:NF)
               GOTO 80
            ENDIF
         ENDIF
         IOP = 2
      ELSE
C
C        See if file exists on the path if reading
C
         IF ( FILSEA ( FILE(1:NF), ' ', PATHNM,
     +                 EXISTS ) ) GOTO 80
         IF ( EXISTS ) THEN
            NF  = LNBLNK ( PATHNM )
            IOP = 1
         ELSE
C
C           Error - non-existent file requested for read
C
            ERROR  = 130
            IDMESS = FILE(1:NF)
            GOTO 80
         ENDIF
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( IOP, FD, PATHNM(1:NF) ) ) GOTO 40
C
C     Switch code on whether reading or writing file header info.
C
      IF ( LWRITE ) THEN
C
C        Code for EPUT
C        -------------
C
C        Fetch dimensions, etc. from picture
C
         NCOL   = NCOLS(LP1)
         NROW   = NROWS(LP1)
         NLAY   = NLAYS(LP1)
         CLASS  = CLASSN(LP1)
         NBPPIX = 1
C
C        Fault bad picture class
C
         IF ( CLASS .NE. NCLIMA ) THEN
            ERROR = 6
            IDERR = IVALPN(NFROM)
            GOTO 20
         ENDIF
C
C        Fault multi-layer pictures
C
         IF ( NLAY .NE. 1 ) THEN
            ERROR = 62
            IDERR = NEPUT
            GOTO 20
         ENDIF
C
C        See if picture label and title available
C
         CDATE(1:17) = '00-00-00 00:00:00 '
         IF ( LBLINC ) THEN
            CDATE(1:1) = CHAR(    (LABEL(LBDAY))/10 + IC0)
            CDATE(2:2) = CHAR( MOD(LABEL(LBDAY),10) + IC0)
            CDATE(4:4) = CHAR(    (LABEL(LBMON))/10 + IC0)
            CDATE(5:5) = CHAR( MOD(LABEL(LBMON),10) + IC0)
            CDATE(7:7) = CHAR(    (LABEL(LBYEAR))/10 + IC0)
            CDATE(8:8) = CHAR( MOD(LABEL(LBYEAR),10) + IC0)
            CDATE(10:10) = CHAR(    (LABEL(LBHOUR))/10 + IC0)
            CDATE(11:11) = CHAR( MOD(LABEL(LBHOUR),10) + IC0)
            CDATE(13:13) = CHAR(    (LABEL(LBMIN))/10 + IC0)
            CDATE(14:14) = CHAR( MOD(LABEL(LBMIN),10) + IC0)
            CDATE(16:16) = CHAR(    (LABEL(LBSEC))/10 + IC0)
            CDATE(17:17) = CHAR( MOD(LABEL(LBSEC),10) + IC0)
         ENDIF
         CDATE(18:18) = CHAR(0)
C
C        Output header line
C
         IF ( EIKCHA ( IOP, FD, ETITLE ) )   GOTO 50
         IF ( EIKINT ( IOP, FD, NCOL ) )     GOTO 50
         IF ( EIKINT ( IOP, FD, NROW ) )     GOTO 50
         IF ( EIKINT ( IOP, FD, NBPPIX ) )   GOTO 50
         IF ( EIKCHA ( IOP, FD, CDATE ) )    GOTO 50
         IF ( EIKBYA ( IOP, FD, LB1, 460 ) ) GOTO 50
      ELSE
C
C        Code for EGET
C        -------------
C
C        Read header line
C
         IF ( EIKCHA ( IOP, FD, CTITLE ) )   GOTO 50
         IF ( EIKINT ( IOP, FD, NCOL ) )     GOTO 50
         IF ( EIKINT ( IOP, FD, NROW ) )     GOTO 50
         IF ( EIKINT ( IOP, FD, NBPPIX ) )   GOTO 50
         IF ( EIKCHA ( IOP, FD, CDATE ) )    GOTO 50
         IF ( EIKBYA ( IOP, FD, LB1, 460 ) ) GOTO 50
C
C        Check we are reading the right type of file
C
         IF ( CTITLE .NE. ETITLE .OR. NBPPIX .NE. 1 ) THEN
            ERROR  = 77
            IDMESS = 'Bad file format'
            GOTO 70
         ENDIF
C
C        Open picture
C
         NLAY  = 1
         CLASS = NCLIMA
         FORM  = NFMBYT
         LP1   = 0
         IF ( SEMOPN ( 2, IVALPN(NTO), NCOL, NROW, NLAY, CLASS,
     +                 SEMFRM(FORM), LP1 ) ) GOTO 70
C
C        Update date in label if possible
C
         IF ( LBLINC ) THEN
            IF ( MEDN(DEVN(LP1)) .NE. MEDTP ) THEN
               LABEL(LBDAY)  = 10 * (ICHAR(CDATE(1:1)) - IC0)
     +                            + (ICHAR(CDATE(2:2)) - IC0)
               LABEL(LBMON)  = 10 * (ICHAR(CDATE(4:4)) - IC0)
     +                            + (ICHAR(CDATE(5:5)) - IC0)
               LABEL(LBYEAR) = 10 * (ICHAR(CDATE(7:7)) - IC0)
     +                            + (ICHAR(CDATE(8:8)) - IC0)
C
               LABEL(LBHOUR) = 10 * (ICHAR(CDATE(10:10)) - IC0)
     +                            + (ICHAR(CDATE(11:11)) - IC0)
               LABEL(LBMIN)  = 10 * (ICHAR(CDATE(13:13)) - IC0)
     +                            + (ICHAR(CDATE(14:14)) - IC0)
               LABEL(LBSEC)  = 10 * (ICHAR(CDATE(16:16)) - IC0)
     +                            + (ICHAR(CDATE(17:17)) - IC0)
C
               IF ( SEMLAB ( 2, LABEL, LP1 ) ) GOTO 70
            ENDIF
         ENDIF
      ENDIF
C
C     Establish local row form and length
C
      K = 1
C
C     Loop over rows reading or writing data
C
      DO 10 J = 1, NROW
C
C        Switch code on verb name
C
         IF ( LWRITE ) THEN
C
C           Read source row from LP1
C
            IF ( SEMROW ( 1, RB1, NFMBYT, J, K, LP1 ) ) GOTO 70
C
C           Write source row to file
C
            IF ( EIKBYA ( IOP, FD, LB1, NCOL ) ) GOTO 50
         ELSE
C
C           Read source row from file
C
            IF ( EIKBYA ( IOP, FD, LB1, NCOL ) ) GOTO 50
C
C           Store source row in LP1
C
            IF ( SEMROW ( 2, RB1, NFMBYT, J, K, LP1 ) ) GOTO 70
         ENDIF
   10 CONTINUE
C
C     Close file
C
   20 IF ( EIKCLO ( FD ) ) GOTO 60
      GOTO 80
C
C     Deal with UNIX I/O errors
C
   30 ERROR  = 77
      IDMESS = 'Error deleting existing file '//PATHNM(1:NF)
      GOTO 80
C
   40 ERROR = 77
      IDMESS = 'Error opening file '//PATHNM(1:NF)
      GOTO 80
C
   50 ERROR = 77
      IF ( IOP .EQ. 1 ) THEN
         IDMESS = 'Error reading file '//PATHNM(1:NF)
      ELSE
         IDMESS = 'Error writing file '//PATHNM(1:NF)
      ENDIF
      GOTO 70
C
   60 ERROR  = 77
      IDMESS = 'Error closing file '//PATHNM(1:NF)
      GOTO 80
C
C     Closing a file after an error (ignore any error on closing file)
C
   70 IF ( EIKCLO ( FD ) ) GOTO 80
      GOTO 80
C
C     All done
C
   80 CONTINUE
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
