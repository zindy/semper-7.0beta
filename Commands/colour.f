C Semper 6 processing module COLOUR
C
      SUBROUTINE COLOUR
C
C Provides command COLOUR:
C  Maps a monochrome image to a three layer colour image
C  If three layer map image (or LUT) supplied with the
C  WITH key will remap using that image. Single layer map
C  can be used for simple mapping as well. Used after
C  INPUT BMP/TIFF m MAP n to create RGB image.
C
      INTEGER IPACK,IVALPN,SEMFRM
      LOGICAL SEMOPN,SEMROW,VARSET
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,I,INFORM,J,LPN,NROW,NLAY,NCOL,OUFORM,OUTLAY
C
      LOGICAL MAPPED,CIMAGE
C
      INTEGER IB1(0:LNBUF/LNINT)
      INTEGER IB2(0:LNBUF/LNINT)
      INTEGER BLUE(0:LNBUF/LNINT)
      INTEGER GREEN(0:LNBUF/LNINT)
      INTEGER RED(0:LNBUF/LNINT)
C
      EQUIVALENCE (RB1,IB1),(RB2,IB2)
      EQUIVALENCE (RB4,BLUE),(RB5,GREEN),(RB6,RED)
C
C     Colour map specified ?
C
      MAPPED = VARSET(-5181)
      IF (MAPPED) THEN
         I = IVALPN(-5181)
         IF (SEMOPN(1,I,NCOL,NROW,NLAY,CLASS,FORM,LPN)) GOTO 20
         IF (NCOL .NE. 256 .OR. NLAY .NE. 1) THEN
C
C     Wrong size for with
C
            ERROR = 5
            IDERR = I
            GOTO 20
         ENDIF
         IF (SEMROW(1,RED,NFMINT,1,1,LPN)) GOTO 20
         IF (NROW .EQ. 3) THEN
            I = 2
            J = 3
         ELSE
            I = 1
            J = 1
         ENDIF
         IF (SEMROW(1,GREEN,NFMINT,I,1,LPN)) GOTO 20
         IF (SEMROW(1,BLUE,NFMINT,J,1,LPN)) GOTO 20
      ENDIF
C
C     Fetch dimensions, etc.
C
      NCOL = NCOLS(LP1)
      NROW = NROWS(LP1)
      NLAY = NLAYS(LP1)
      IF (NLAY .NE. 1 .AND. NLAY .NE. 3) GOTO 60
      CIMAGE = NLAY .EQ. 3
      CLASS= CLASSN(LP1)
      IF (CLASS .NE. NCLIMA) GOTO 30
      FORM = FORMN(LP1)
      OUFORM = SEMFRM(FORM)
C
      IF (MAPPED) THEN
         IF (FORM .NE. NFMBYT) GOTO 50
         INFORM = NFMINT
      ELSE
         INFORM = OUFORM
      ENDIF
C
C     Open output picture
C
      LP2 = LP1
      OUTLAY = 3
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,OUTLAY,CLASS,
     +           OUFORM,LP2)) GOTO 20
C
      DO 10 J=1,NROW
C
C        Read source row(s) from LP1
C
         IF (SEMROW(1,IB1,INFORM,J,1,LP1)) GOTO 20
         IF (MAPPED) THEN
            CALL COLMAP(IB1,IB2,RED,NCOL)
            IF (SEMROW(2,IB2,INFORM,J,1,LP2)) GOTO 20
         ELSE
            IF (SEMROW(2,IB1,INFORM,J,1,LP2)) GOTO 20
         ENDIF
C
         IF (CIMAGE) THEN
C
C           Read Green source, map if appropriate
C
            IF (SEMROW(1,IB1,INFORM,J,2,LP1)) GOTO 20
            IF (MAPPED) THEN
               CALL COLMAP(IB1,IB2,GREEN,NCOL)
               IF (SEMROW(2,IB2,INFORM,J,2,LP2)) GOTO 20
            ELSE
               IF (SEMROW(2,IB1,INFORM,J,2,LP2)) GOTO 20
            ENDIF
C
C           Read Blue source
C
            IF (SEMROW(1,IB1,INFORM,J,3,LP1)) GOTO 20
         ELSE
C
C           Single layer source - if mapping use green maps
C
            IF (MAPPED) THEN
               CALL COLMAP(IB1,IB2,GREEN,NCOL)
               IF (SEMROW(2,IB2,INFORM,J,2,LP2)) GOTO 20
            ELSE
C
C              Otherwise just output again
C
               IF (SEMROW(2,IB1,INFORM,J,2,LP2)) GOTO 20
            ENDIF
         ENDIF
C
C Blue layer all types
C
         IF (MAPPED) THEN
            CALL COLMAP(IB1,IB2,BLUE,NCOL)
            IF (SEMROW(2,IB2,INFORM,J,3,LP2)) GOTO 20
         ELSE
            IF (SEMROW(2,IB1,INFORM,J,3,LP2)) GOTO 20
         ENDIF
   10 CONTINUE
C
C     All done (one way or another!)
C
   20 RETURN
C
C     Wrong class for input picture
C
   30 ERROR = 6
   40 IDERR = IVALPN(10335)
      GOTO 20
C
C     Bad form for mapped input picture
C
   50 ERROR = 43
      GOTO 40
C
C     Wrong size for input
C
   60 ERROR = 5
      GOTO 40
C
C Copyright (C) 1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE COLMAP(IN,OUT,MAP,N)
      INTEGER IN(*),OUT(*),MAP(0:*),N
C
      INTEGER I
C
      DO 10 I = 1,N
         OUT(I) = MAP(IN(I))
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1996 Synoptics Ltd,  All Rights Reserved
C
      END
