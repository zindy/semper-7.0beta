C Semper 6 processing module SURVEY
C
      SUBROUTINE SURVEY
C
C Scans LP1 sub-reg for MIN,MAX and - iff FULL - MEAN,SD as well,
C reporting results unless NOVERIFY is set.  RANGE is used normally for
C MIN,MAX det'n, but if FULL is set MEANSD is used instead, which
C scans unconditionally, and therefore provides a recovery route
C should a label ever get a bad range recorded in it.
C
C MIN,MAX return range spanned by re AND im part; MEAN,ME2 provide
C the (complex) mean, and SD is the rms modulus deviation,
C i.e. root[ <|p-E(p)|^2> ]
C
C HISTOGRAM pics treated specially: range,mean,sd  returned are those of
C pixels represented by histogram, not those of the histogram counts
C themselves, and median,mode are also estimated and set
C
      LOGICAL SEMROW,SEMLU,DSPSUB,OPT,OPTNO,SEMTYP,TEXTU1,SEMCON
C
      INCLUDE 'COMMON'
C
      INTEGER TBUF(256),OBUF(256),M1,M2,N1,N2
      EQUIVALENCE (RB2,TBUF),(RB3,OBUF)
      EQUIVALENCE (SMGI1,M1),(SMGI4,M2),(SMGI2,N1),(SMGI5,N2)
      INTEGER FBUF(60),FXTRA(8),PTR,I,J,LENF,LENO,NCHAN,FORM
      REAL HALFTC,TOTAL,CNTMAX,DX,X,VMODE,VMEDN
      LOGICAL MEDSET
C
C Packed names
      INTEGER NVERIF,NFULL,NFROM,NMEDIA,NMODE
      PARAMETER (NVERIF=-3419,NFULL=10452,NFROM=10335)
      PARAMETER (NMEDIA=21004,NMODE=21404)
C
C Format string for terminal verification
      DATA FBUF/KQUOTE,KUCM,KLCE,KLCA,KLCN,KSPACE,KQUOTE,KCOMMA,KLCM,
     +   KLCE,KLCA,KCOMMA,KQUOTE,2*KSPACE,KUCS,KLCD,KSPACE,KQUOTE,
     +   KCOMMA,KLCS,KLCD,KCOMMA,KQUOTE,2*KSPACE,KUCM,KLCE,KLCD,KLCI,
     +   KLCA,KLCN,KSPACE,KQUOTE,KCOMMA,KLCM,KLCE,KLCD,KCOMMA,KQUOTE,
     +   2*KSPACE,KUCM,KLCO,KLCD,KLCE,KSPACE,KQUOTE,KCOMMA,KLCM,KLCO,
     +   KLCD,8*KSPACE/
      DATA FXTRA/KCOMMA,KQUOTE,KCOMMA,KQUOTE,KCOMMA,KLCM,KLCE,KTWO/
C
C Call DSPSUB: establishes subregion, range, and mean/sd if full
      IF (DSPSUB(I,J,1,OPTNO(NVERIF),FORM,1,M1,M2,N1,N2)) RETURN
C
C If option FULL is set, print mean/sd
      IF (OPT(NFULL)) THEN
C
C Prepare output format
         DO 10 I=1,60
            TBUF(I)=FBUF(I)
   10    CONTINUE
C
         LENF=60
         IF (FORMN(LP1).EQ.NFMCOM) THEN
            IF (TEXTU1(TBUF,LENF,256,12,0,FXTRA,8)) GOTO 20
         ENDIF
C Omit median,mode at first
   20    LENF=LENF-38
C
C If histogram, re-estimate MEAN,SD from histogram
         IF (CLASSN(LP1).EQ.NCLHIS) THEN
            IF (SEMROW(1,RB1,2,1,1,LP1)) RETURN
            NCHAN=NCOLS(LP1)-2
            HALFTC=VMEAN/2.*NCHAN
            TOTAL=0.
            VMEAN=0.
            VSD=0.
            MEDSET=.FALSE.
            CNTMAX=0.
            DX=(VMAX-VMIN)/NCHAN
            X=VMIN+.5*DX
C
            DO 30 I=1,NCHAN
               TOTAL=TOTAL+RB1(I)
               VMEAN=VMEAN+X*RB1(I)
               VSD=VSD+X*X*RB1(I)
               IF (RB1(I).GT.CNTMAX) THEN
                  CNTMAX=RB1(I)
                  VMODE=X
               ENDIF
               IF (.NOT.MEDSET) THEN
                  IF (TOTAL.GE.HALFTC) THEN
                     VMEDN=X
                     MEDSET=.TRUE.
                  ENDIF
               ENDIF
               X=X+DX
   30       CONTINUE
            VMEAN=VMEAN/TOTAL
            VSD=VSD/TOTAL-VMEAN*VMEAN
            IF (VSD.LT.0.) VSD=0.
            VSD=SQRT(VSD)
            IF (SEMLU(1,NMEDIA,VMEDN)) RETURN
            IF (SEMLU(1,NMODE,VMODE)) RETURN
C
C Include median, mode in format
            LENF=LENF+38
         ENDIF
C
C Unless NOVERIFY, encode and verify statistics
         IF (.NOT.OPTNO(NVERIF)) THEN
            PTR=1
            LENO=72
            IF (SEMTYP(TBUF,LENF,PTR,OBUF,LENO,.FALSE.)) GOTO 40
   40       CALL SEMCHS(RECORD,OBUF,LENO)
            IF (SEMCON(RECORD)) RETURN
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
