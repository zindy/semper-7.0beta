C Semper 6 processing module CUT
C
      SUBROUTINE CUT
C
C Extracts any part of the source picture and stores the result in
C the output picture.
C
C Keys SIZE,SI2,SI3 may be used to specify the output picture size.
C
C Keys POSITION,PO2,PO3 and LAYER,LA2 and options LEFT,RIGHT,
C TOP,BOTTOM,NEAR and FAR may be used to define, with respect to
C the source picture, the region to extract.
C
C Any part of the output picture which lies outsids the source
C picture is set to the background value specified by the
C VALUE key.
C
C Global declarations
C
      INCLUDE 'COMMON'
C
C Suggested additional globals
C
      INTEGER MAXIPB,MAXRPB
      PARAMETER (MAXIPB=LNBUF/LNINT,MAXRPB=LNBUF/LNREAL)
C
C Local declarations
C
      REAL VAL
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL SEMOPN,SEMROW,TSTSRG,MRKREG,SEMCEN
C
      REAL VALUE
      INTEGER IB1(MAXIPB),IB2(MAXIPB),IB3(MAXIPB)
      INTEGER FIRST(3),LAST(3),SIZE(3)
      INTEGER FORM,INFORM,IVALUE,CCOL,CROW,CLAY
      INTEGER I,I1,I2,IOFF,J,J1,J2,J3,JOFF,JOUT,K,K1,K2,K3,KOFF,KOUT
      INTEGER NCOL,NLAY,NROW
      INTEGER*4 N4,NCOL4
      LOGICAL LAYIN,COLINC,COLDSJ
C
      EQUIVALENCE (IB1,RB1),(IB2,RB2),(RB3,IB3)
      EQUIVALENCE (FIRST,SMGI1),(LAST,SMGI4),(SIZE,SMGI7)
C
C Packed names
C
      INTEGER NTO,NVALUE
      PARAMETER (NTO=-601,NVALUE=-3253)
C
C Determine limits of output picture w.r.t. source picture
C
      IF (TSTSRG(2,LP1)) GOTO 70
C
C Note whether column range is entirely included in source,
C or entirely disjoint
C
      COLINC=FIRST(1).GE.1.AND.LAST(1).LE.NCOLS(LP1)
      COLDSJ=FIRST(1).GT.NCOLS(LP1).OR.LAST(1).LE.0
C
C Open output
C
      NCOL=SIZE(1)
      NROW=SIZE(2)
      NLAY=SIZE(3)
      FORM=SEMFRM(FORMN(LP1))
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,NLAY,CLASSN(LP1),FORM,LP2))
     +    GOTO 70
C
C Determine output centre position
C
      CCOL=CCOLN(LP1)-FIRST(1)+1
      CROW=CROWN(LP1)-FIRST(2)+1
      CLAY=CLAYN(LP1)-FIRST(3)+1
C
C Update centre position of output picture if within picture limits
C
      IF (CCOL.GE.1.AND.CCOL.LE.NCOL.AND.
     +    CROW.GE.1.AND.CROW.LE.NROW.AND.
     +    CLAY.GE.1.AND.CLAY.LE.NLAY) THEN
         IF (SEMCEN(LP2,CCOL,CROW,CLAY)) GOTO 70
      ENDIF
C
C Establish form for internal use: simpler of input,output forms
C
      INFORM=NFMCOM
      IF (FORMN(LP1).EQ.NFMFP.OR.FORM.EQ.NFMFP) INFORM=NFMFP
      IF (FORMN(LP1).EQ.NFMINT.OR.FORM.EQ.NFMINT
     +   .OR.FORMN(LP1).EQ.NFMBYT.OR.FORM.EQ.NFMBYT) INFORM=NFMINT
C
C Draw extraction frame
C
      IF (MRKREG(0)) GOTO 70
C
C Determine offset between output and source
C
      IOFF=1-FIRST(1)
      JOFF=1-FIRST(2)
      KOFF=1-FIRST(3)
C
C Note truncated source col range
C
      I1=MAX(1,FIRST(1))
      I2=MIN(LAST(1),NCOLS(LP1))
      N4=I2-I1+1
C
C Reverse direction of processing if source and output would
C interfere, given in-situ processing
C
      IF (JOFF.GT.0) THEN
         J1=LAST(2)
         J2=FIRST(2)
         J3=-1
      ELSE
         J1=FIRST(2)
         J2=LAST(2)
         J3=1
      ENDIF
C
      IF (KOFF.GT.0) THEN
         K1=LAST(3)
         K2=FIRST(3)
         K3=-1
      ELSE
         K1=FIRST(3)
         K2=LAST(3)
         K3=1
      ENDIF
C
C Adjust control parameters if output form is complex
C
      IF (INFORM.EQ.NFMCOM) THEN
         I1=2*I1-1
         I2=2*I2
         IOFF=2*IOFF
         NCOL=2*NCOL
      ENDIF
C
C Prepare background in RB3
C
      IF (INFORM.EQ.NFMINT) THEN
         IVALUE=IVAL(NVALUE)
         DO 10 I=1,NCOL
            IB3(I)=IVALUE
   10    CONTINUE
      ELSE
         VALUE=VAL(NVALUE)
         DO 20 I=1,NCOL
            RB3(I)=VALUE
   20    CONTINUE
      ENDIF
C
C Prepare copy in RB4 in output form, for output to rows which do
C not overlap source at all; this form required in case SEMROW
C form-converts in situ (non-caching disc)
C
      NCOL4=NCOLS(LP2)
      CALL CFORM(RB3,RB4,INFORM,FORM,NCOL4)
C
C Extract data from source picture to output picture
C
      DO 60 K=K1,K2,K3
         KOUT=K+KOFF
C
C Note whether layer lies within source
C
         LAYIN=K.GE.1.AND.K.LE.NLAYS(LP1)
C
         DO 50 J=J1,J2,J3
            JOUT=J+JOFF
C
C Does output row overlap source?  If so, fetch it and construct
C output; if not, simply output from prepared background buffer
C
            IF (LAYIN.AND.
     +         J.GE.1.AND.J.LE.NROWS(LP1).AND.
     +         .NOT.COLDSJ) THEN
C
C Fetch source row
C
               IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 70
C
C Is output completely included in source?  If so, output directly
C from source buffer; if not, prefill with background and copy the
C strip of source needed
C
               IF (COLINC) THEN
C
C Output directly from source
C
                  IF (INFORM.EQ.NFMINT) THEN
                     IF (SEMROW(2,IB1(I1),NFMINT,JOUT,KOUT,LP2)) GOTO 70
                  ELSE
                     IF (SEMROW(2,RB1(I1),INFORM,JOUT,KOUT,LP2)) GOTO 70
                  ENDIF
C
C Prefill output buffer and copy strip from source
C
               ELSE
                  IF (INFORM.EQ.NFMINT) THEN
                     CALL CFORM(IB3,IB2,NFMINT,NFMINT,NCOL4)
                     CALL CFORM(IB1(I1),IB2(I1+IOFF),NFMINT,NFMINT,N4)
                  ELSE
                     CALL CFORM(RB3,RB2,INFORM,INFORM,NCOL4)
                     CALL CFORM(RB1(I1),RB2(I1+IOFF),INFORM,INFORM,N4)
                  ENDIF
C
C Output the result
C
                  IF (SEMROW(2,RB2,INFORM,JOUT,KOUT,LP2)) GOTO 70
               ENDIF
C
C Row does not overlap source: just output from RB4
C
            ELSE
               IF (SEMROW(2,RB4,FORM,JOUT,KOUT,LP2)) GOTO 70
            ENDIF
   50    CONTINUE
   60 CONTINUE
C
   70 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
