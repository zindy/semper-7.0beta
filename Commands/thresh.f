C Semper 6 processing module THRESH
C
      SUBROUTINE THRESH
C
C Apply specified thresholding condition(s) to source picture and
C produce binary output picture.  The scheme reflects the approach
C used in ANALYSE but allows for up to 18 upper and 18 lower limits
C to be specified by means of the multi-valued keys LE, LT, GE and GT.
C The limits can be specified in any order so long as the upper and
C lower limits, when sorted into ascending order, alternate.  If no
C threshold values are specified, the mid-range value of the source
C picture is used as a lower limit.  The output picture range is
C is determined directly and stored in the picture label.
C
      LOGICAL VARSET,SEMRNG,SEMOPN,SEMROW
      INTEGER IPACK,IVALPN
      REAL    VAL
C
      INTEGER I,J,K,L,L1,L2,NCOL,NROW,NLAY,NPIC,INFORM
      INTEGER IG(18),IL(18),LG(18),LL(18),IUPPER,ILOWER,NG,NL
      REAL    RG(18),RL(18),RUPPER,RLOWER,RMIN,RMAX
      LOGICAL EQ(18),UPPER,LOWER,FAULT,LFG,LBG
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT)
      INTEGER IB2(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1),(IB2,RB2)
C
      CHARACTER*3 GENAME(9),LENAME(9),GTNAME(9),LTNAME(9)
      DATA GENAME / 'ge ', 'ge2', 'ge3', 'ge4', 'ge5',
     +              'ge6', 'ge7', 'ge8', 'ge9'/
      DATA LENAME / 'le ', 'le2', 'le3', 'le4', 'le5',
     +              'le6', 'le7', 'le8', 'le9'/
      DATA GTNAME / 'gt ', 'gt2', 'gt3', 'gt4', 'gt5',
     +              'gt6', 'gt7', 'gt8', 'gt9'/
      DATA LTNAME / 'lt ', 'lt2', 'lt3', 'lt4', 'lt5',
     +              'lt6', 'lt7', 'lt8', 'lt9'/
C
C Zero counts of upper and lower limits
C
      NG=0
      NL=0
C
C Fetch values for upper/lower limits from multi-valued keys GE, LE,
C GT and LT
C
      DO 10 L=1,9
         IF (VARSET(IPACK(GENAME(L)))) THEN
            NG=NG+1
            LG(NG)=NG
            RG(NG)=VAL(IPACK(GENAME(L)))
            EQ(NG)=.TRUE.
         ENDIF
C
         IF (VARSET(IPACK(LENAME(L)))) THEN
            NL=NL+1
            LL(NL)=NL
            RL(NL)=VAL(IPACK(LENAME(L)))
            EQ(NL)=.TRUE.
         ENDIF
C
         IF (VARSET(IPACK(GTNAME(L)))) THEN
            NG=NG+1
            LG(NG)=NG
            RG(NG)=VAL(IPACK(GTNAME(L)))
            EQ(NG)=.FALSE.
         ENDIF
C
         IF (VARSET(IPACK(LTNAME(L)))) THEN
            NL=NL+1
            LL(NL)=NL
            RL(NL)=VAL(IPACK(LTNAME(L)))
            EQ(NL)=.FALSE.
         ENDIF
   10 CONTINUE
C
C Fault excess of upper over lower limit values (or vice versa)
C
      IF (ABS(NG-NL).GT.1) THEN
         ERROR=77
         IDMESS='Too many upper or lower limits specified'
         GOTO 270
      ENDIF
C
C If no threshold values specified, use mid-range value of source
C picture as single lower limit
C
      IF (NG.EQ.0.AND.NL.EQ.0) THEN
         IF (SEMRNG(1,RLOWER,RUPPER,LP1)) GOTO 270
         RG(1)=(RLOWER+RUPPER)/2.0
         EQ(1)=.TRUE.
         LG(1)=1
         NG=1
         NL=0
         UPPER=.FALSE.
         LOWER=.TRUE.
         L1=2
         L2=1
C
C Otherwise, sort upper/lower limits ...
C
      ELSE
         CALL THRES2(RG,LG,NG)
         CALL THRES2(RL,LL,NL)
C
C ... and look for isolated upper and lower limits ...
C
         IF (NG.EQ.0) THEN
            UPPER=.TRUE.
            LOWER=.FALSE.
         ELSE IF (NL.EQ.0) THEN
            UPPER=.FALSE.
            LOWER=.TRUE.
         ELSE
            UPPER=RL(LL(1)).LT.RG(LG(1))
            LOWER=RG(LG(NG)).GT.RL(LL(NL))
         ENDIF
C
C ... and establish range of paired limits (if any) ...
C
         IF (UPPER) THEN
            L1=2
         ELSE
            L1=1
         ENDIF
         L2=NL
C
C ... and fault any inconsistencies between upper and lower limits
C
         IF (UPPER.EQV.LOWER) THEN
            FAULT=NG.NE.NL
         ELSE
            IF (UPPER) THEN
               FAULT=NG.NE.NL-1
            ELSE
               FAULT=NG.NE.NL+1
            ENDIF
         ENDIF
C
         IF (.NOT.FAULT) THEN
            DO 20 L=L1,L2
               IF (RL(LL(L)).LT.RG(LG(1+(L-L1)))) FAULT=.TRUE.
   20       CONTINUE
C
            DO 30 L=L1,L2-1
               IF (RL(LL(L)).GE.RG(LG(2+(L-L1)))) FAULT=.TRUE.
   30       CONTINUE
         ENDIF
C
         IF (FAULT) THEN
            ERROR=77
            IDMESS='Inconsistent set of upper/lower limits specified'
            GOTO 270
         ENDIF
      ENDIF
C
C Determine form for processing source picture
C
      IF (FORMN(LP1).NE.NFMFP.AND.FORMN(LP1).NE.NFMCOM) THEN
         INFORM=NFMINT
      ELSE
         INFORM=NFMFP
      ENDIF
C
C If non floating-point source picture, convert threshold values to
C integer equivalents
C
      IF (INFORM.EQ.NFMINT) THEN
         DO 40 L=1,NG
            IG(L)=NINT(RG(L))
            IF (REAL(IG(L)).LT.RG(L)) IG(L)=IG(L)+1
   40    CONTINUE
C
         DO 50 L=1,NL
            IL(L)=NINT(RL(L))
            IF (REAL(IL(L)).GT.RL(L)) IL(L)=IL(L)-1
   50    CONTINUE
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Open output picture with byte form
C
      NPIC=IVALPN(-601)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,NLAY,NCLIMA,NFMBYT,LP2)) GOTO 270
C
C Initialise range flags
C
      LBG=.FALSE.
      LFG=.FALSE.
C
C Process source to output picture
C
      DO 260 K=1,NLAY
C
         DO 250 J=1,NROW
C
C Fetch source picture row
C
            IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 270
C
C Initialise output result
C
            DO 60 I=1,NCOL
               IB2(I)=0
   60       CONTINUE
C
C Process isolated upper limit
C
            IF (UPPER) THEN
               IF (INFORM.EQ.NFMINT) THEN
                  IUPPER=IL(LL(1))
                  IF (EQ(LL(1))) THEN
                     DO 70 I=1,NCOL
                        IF (IB1(I).LE.IUPPER) IB2(I)=1
   70                CONTINUE
                  ELSE
                     DO 80 I=1,NCOL
                        IF (IB1(I).LT.IUPPER) IB2(I)=1
   80                CONTINUE
                  ENDIF
               ELSE
                  RUPPER=RL(LL(1))
                  IF (EQ(LL(1))) THEN
                     DO 90 I=1,NCOL
                        IF (RB1(I).LE.RUPPER) IB2(I)=1
   90                CONTINUE
                  ELSE
                     DO 100 I=1,NCOL
                        IF (RB1(I).LT.RUPPER) IB2(I)=1
  100                CONTINUE
                  ENDIF
               ENDIF
            ENDIF
C
C Process isolated lower limit
C
            IF (LOWER) THEN
               IF (INFORM.EQ.NFMINT) THEN
                  ILOWER=IG(LG(NG))
                  IF (EQ(LG(NG))) THEN
                     DO 110 I=1,NCOL
                        IF (IB1(I).GE.ILOWER) IB2(I)=1
  110                CONTINUE
                  ELSE
                     DO 120 I=1,NCOL
                        IF (IB1(I).GT.ILOWER) IB2(I)=1
  120                CONTINUE
                  ENDIF
               ELSE
                  RLOWER=RG(LG(NG))
                  IF (EQ(LG(NG))) THEN
                     DO 130 I=1,NCOL
                        IF (RB1(I).GE.RLOWER) IB2(I)=1
  130                CONTINUE
                  ELSE
                     DO 140 I=1,NCOL
                        IF (RB1(I).GT.RLOWER) IB2(I)=1
  140                CONTINUE
                  ENDIF
               ENDIF
            ENDIF
C
C Process remaining pairs of threshold limits
C
            DO 230 L=L1,L2
               IF (INFORM.EQ.NFMINT) THEN
                  IUPPER=IL(LL(L))
                  ILOWER=IG(LG(1+(L-L1)))
                  IF (EQ(LL(L))) THEN
                     IF(EQ(LG(1+(L-L1)))) THEN
                        DO 150 I=1,NCOL
                           IF (IB1(I).GE.ILOWER.AND.IB1(I).LE.IUPPER)
     +                        IB2(I)=1
  150                   CONTINUE
                     ELSE
                        DO 160 I=1,NCOL
                           IF (IB1(I).GT.ILOWER.AND.IB1(I).LE.IUPPER)
     +                        IB2(I)=1
  160                   CONTINUE
                     ENDIF
                  ELSE
                     IF(EQ(LG(1+(L-L1)))) THEN
                        DO 170 I=1,NCOL
                           IF (IB1(I).GE.ILOWER.AND.IB1(I).LT.IUPPER)
     +                        IB2(I)=1
  170                   CONTINUE
                     ELSE
                        DO 180 I=1,NCOL
                           IF (IB1(I).GT.ILOWER.AND.IB1(I).LT.IUPPER)
     +                        IB2(I)=1
  180                   CONTINUE
                     ENDIF
                  ENDIF
               ELSE
                  RUPPER=RL(LL(L))
                  RLOWER=RG(LG(1+(L-L1)))
                  IF (EQ(LL(L))) THEN
                     IF(EQ(LG(1+(L-L1)))) THEN
                        DO 190 I=1,NCOL
                           IF (RB1(I).GE.RLOWER.AND.RB1(I).LE.RUPPER)
     +                        IB2(I)=1
  190                   CONTINUE
                     ELSE
                        DO 200 I=1,NCOL
                           IF (RB1(I).GT.RLOWER.AND.RB1(I).LE.RUPPER)
     +                        IB2(I)=1
  200                   CONTINUE
                     ENDIF
                  ELSE
                     IF(EQ(LG(1+(L-L1)))) THEN
                        DO 210 I=1,NCOL
                           IF (RB1(I).GE.RLOWER.AND.RB1(I).LT.RUPPER)
     +                        IB2(I)=1
  210                   CONTINUE
                     ELSE
                        DO 220 I=1,NCOL
                           IF (RB1(I).GT.RLOWER.AND.RB1(I).LT.RUPPER)
     +                        IB2(I)=1
  220                   CONTINUE
                     ENDIF
                  ENDIF
               ENDIF
  230       CONTINUE
C
C Update range flags
C
            DO 240 I=1,NCOL
               IF (IB2(I).EQ.0) THEN
                  LBG=.TRUE.
               ELSE
                  LFG=.TRUE.
               ENDIF
  240       CONTINUE
C
C Store result in output picture
C
            IF (SEMROW(2,IB2,NFMINT,J,K,LP2)) GOTO 270
  250    CONTINUE
  260 CONTINUE
C
C Determine output picture range by examining range flags
C
      IF (LBG) THEN
         RMIN=0.0
      ELSE
         RMIN=1.0
      ENDIF
C
      IF (LFG) THEN
         RMAX=1.0
      ELSE
         RMAX=0.0
      ENDIF
C
C Store output picture range
C
      IF (SEMRNG(2,RMIN,RMAX,LP2)) GOTO 270
C
  270 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 processing module THRES2
C
      SUBROUTINE THRES2(X,L,N)
C
      REAL X(*)
      INTEGER L(*),N
C
C Sorts N floating-point values in array X by re-arranging pointers in
C array L.  Sort is done in place using straightforward insertion sort
C which is very efficient for sorting a small number of values.
C
      INTEGER I,J,K
      REAL Y
C
      DO 20 I=2,N
C
         J = I-1
         K = L(I)
         Y = X(K)
C
   10    IF (Y.LT.X(L(J))) THEN
            L(J+1)=L(J)
            J=J-1
            IF (J.GT.0) GOTO 10
         ENDIF
C
         L(J+1)=K
C
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
