C Semper processing module EXPAND
C
      SUBROUTINE EXPAND
C
C Horizontally resamples images.  This is a faster way to correct
C images captured via framestores with non-square pixels than to use
C EXTRACT.  The source:output ratio is specified by means of the
C RATIO keys.  Any combination of values may be given for the ratio.
C Any common factors in the ratio values will be divided out before
C the source image is processed.  If the option NNEIGHBOUR is set,
C the source data is sampled at the nearest pixel positions rather
C than linear interpolation being used.
C
C Syntax:  Expand :EXPAND $1=sel from=$1 $2=from to=$2 ratio= ra2= +
C                         nneighbour open(lp1,old)=from
C
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL OPT,SEMOPN,SEMROW
C
      INTEGER I,J,K,L,N,N1,N2,NCOL,NROW,NLAY,CLASS,INFORM,FORM
      INTEGER NPIC,NSIZ
      LOGICAL LNNEIG
      REAL    C,F,F1,F2
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT),IB2(LNBUF/LNINT)
      EQUIVALENCE (IB1,RB1),(IB2,RB2)
C
C Packed names
C
      INTEGER NRATIO,NRA2,NTO,NNNEIG
      PARAMETER (NRATIO=28860, NRA2=28872, NTO=-601, NNNEIG=22965)
C
C Fetch resampling ratio values
C
      N1=IVAL(NRATIO)
      N2=IVAL(NRA2)
C
C Fault bad value
C
      IF (N1.LT.1.OR.N2.LT.1) THEN
         ERROR=3
         IDERR=NRATIO
         GOTO 110
      ENDIF
C
C Divide out any common factors in the ratio values
C
      N=2
C
   10 IF (MOD(N1,N).EQ.0.AND.MOD(N2,N).EQ.0) THEN
         N1=N1/N
         N2=N2/N
         GOTO 10
      ENDIF
C
      N=N+1
      IF (N.LE.MIN(N1,N2)) GOTO 10
C
      F=REAL(N2)/REAL(N1)
C
C See if option NNEIGHBOUR is set
C
      LNNEIG = OPT(NNNEIG)
C
C Fetch sources picture size, class and form
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      CLASS=CLASSN(LP1)
      FORM=FORMN(LP1)
C
C Determine form for processing according source picture form
C
      IF (FORM.EQ.NFMBYT.OR.FORM.EQ.NFMINT) THEN
         INFORM=NFMINT
      ELSE
         INFORM=NFMFP
      ENDIF
C
C Open output picture
C
      NPIC=IVALPN(NTO)
      NSIZ=INT(MIN(1.0+REAL(NCOL-1)*F,32000.0))
      FORM=SEMFRM(FORM)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NSIZ,NROW,NLAY,CLASS,FORM,LP2)) GOTO 110
C
C Process each layer in turn
C
      DO 100 K=1,NLAY
C
C Process each row in turn
C
         DO 90 J=1,NROW
C
C Fetch source row
C
            IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 110
C
C Process all precisely sampled results
C
            N=1
            IF (INFORM.EQ.NFMINT) THEN
               DO 20 I=1,NCOL,N1
                  IB2(N)=IB1(I)
                  N=N+N2
   20          CONTINUE
            ELSE
               DO 30 I=1,NCOL,N1
                  RB2(N)=RB1(I)
                  N=N+N2
   30          CONTINUE
            ENDIF
C
C Process the data in between
C
            DO 80 L=2,N2
               C=REAL(L-1)/F
C
C If option NNEIGHBOUR is set, sample the source data
C
               N=L
               IF (LNNEIG) THEN
                  IF (INFORM.EQ.NFMINT) THEN
                     DO 40 I=1+NINT(C),NCOL,N1
                        IB2(N)=IB1(I)
                        N=N+N2
   40                CONTINUE
                  ELSE
                     DO 50 I=1+NINT(C),NCOL,N1
                        RB2(N)=RB1(I)
                        N=N+N2
   50                CONTINUE
                  ENDIF
C
C Otherwise, use interpolation
C
               ELSE
                  F2=C-AINT(C)
                  F1=1.0-F2
C
                  IF (INFORM.EQ.NFMINT) THEN
                     DO 60 I=1+INT(C),NCOL-1,N1
                        IB2(N)=NINT(F1*REAL(IB1(I))+F2*REAL(IB1(I+1)))
                        N=N+N2
   60                CONTINUE
                  ELSE
                     DO 70 I=1+INT(C),NCOL-1,N1
                        RB2(N)=F1*RB1(I)+F2*RB1(I+1)
                        N=N+N2
   70                CONTINUE
                  ENDIF
               ENDIF
   80       CONTINUE
C
C Store results in output row
C
            IF (SEMROW(2,RB2,INFORM,J,K,LP2)) GOTO 110
   90    CONTINUE
  100 CONTINUE
C
  110 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
