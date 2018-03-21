C Semper 6 processing module PID
C
      SUBROUTINE PID
C
C Given a picture position via the keys POSITION and PO2, returns in
C variable PID the corresponding particle id obtained from the segmented
C source picture.  The return variable is left unset if an error is
C detected.  If the position points to a background pixel, the variable
C is set to zero.  The result is printed, unless option NOVERIFY is set.
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL VARSET,SEMOPN,SEMROW,SEMLU,OPTNO,SEMCON
C
      INCLUDE 'COMMON'
C
      REAL X,XMAX,XMIN,Y,YMAX,YMIN
      INTEGER CLASS,CCOL,CROW,FORM,I,J,NCOL,NLAY,NPIC,NROW
C
      INTEGER IB1(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1)
C
C Packed names
C
      INTEGER NPID,NSEGME,NPSEGM,NPOSIT,NPO2,NVERIF
      PARAMETER (NPID=25964, NSEGME=30607, NPSEGM=26365)
      PARAMETER (NPOSIT=26219, NPO2=26232, NVERIF=-3419)
C
C If key SEGMENT is set to zero, picture number for segmented picture
C is obtained from key PSEGMENT
C
      IF (IVAL(NSEGME).EQ.0) THEN
C
C If variable PSEGMENT is set, use it to determine picture number
C
         IF (VARSET(NPSEGM)) THEN
            NPIC=IVALPN(NPSEGM)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPSEGM
            GOTO 20
         ENDIF
C
C Otherwise, use key value to determine picture number
C
      ELSE
         NPIC=IVALPN(NSEGME)
      ENDIF
C
C Open segmented picture
C
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 20
C
C Fetch segmented picture centre position
C
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Determine segmented picture limits
C
      XMIN=REAL(1-CCOL)
      XMAX=REAL(NCOL-CCOL)
      YMIN=REAL(CROW-NROW)
      YMAX=REAL(CROW-1)
C
C Fetch value for keys POSITION and PO2
C
      X=VAL(NPOSIT)
      Y=VAL(NPO2)
C
C Fault position outside segmented picture
C
      IF (X.LT.XMIN.OR.X.GT.XMAX.OR.Y.LT.YMIN.OR.Y.GT.YMAX) THEN
         ERROR=9
         GOTO 20
      ENDIF
C
C Convert position to nearest row/column position
C
      I=CCOL+NINT(X)
      J=CROW-NINT(Y)
C
C Read corresponding row of segmented picture
C
      IF (SEMROW(1,RB1,NFMINT,J,1,LP1)) GOTO 20
C
C Fault negative pixel value
C
      IF (IB1(I).LT.0) THEN
         ERROR=154
         GOTO 20
      ENDIF
C
C Set return variable PID = pixel value
C
      IF (SEMLU(1,NPID,REAL(IB1(I)))) GOTO 20
C
C Print results (unless option NOVERIFY is set)
C
      IF (.NOT.OPTNO(NVERIF)) THEN
         IF (IB1(I).EQ.0) THEN
            RECORD='Background hit'
         ELSE
            WRITE (RECORD,10) IB1(I)
   10       FORMAT ('Particle id:',I5)
         ENDIF
C
         IF (SEMCON(RECORD)) GOTO 20
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
