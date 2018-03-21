C Semper 6 processing module PFERET
C
      SUBROUTINE PFERET
C
C Calculates from one to nine feret diameters for a particle.  The
C key ID specifies the particle id.  The number of feret diameters is
C determined by the number of keys from ANGLE to AN9, which specify
C the direction for each feret diameter.  The results are printed
C (unless option NOVERIFY is set) and the variables F to F9 are
C set/unset according to whether a corresponding feret diameter has
C been calculated or not.  Keys SEGMENT and PPLIST specify the picture
C numbers for the segmented picture and the particle parameter list.
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL VARSET,SEMOPN,SEMROW,SEMLU,OPTNO,SEMCON
C
      INCLUDE 'COMMON'
C
      REAL ANGLE(9),C(9),S(9),F,F1,F2,FMIN(9),FMAX(9),FERET(9)
      REAL X1,X2,XREF,Y,YREF
      INTEGER CLASS,FORM,CCOL,CROW,I,I1,I2,ID,J,J1,J2
      INTEGER N,NCOL,NLAY,NPIC,NROW
      LOGICAL LFOUND
C
      INTEGER IB1(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1)
C
      INTEGER NXREF,NYREF,NIDENT,NXMIN,NXMAX,NYMIN,NYMAX
      PARAMETER (NXREF=1, NYREF=2, NIDENT=3)
      PARAMETER (NXMIN=8, NXMAX=9, NYMIN=10, NYMAX=11)
C
C Packed names
C
      INTEGER NSEGME,NPSEGM,NPLIST,NPPLIS,NID,NPID,NVERIF
      INTEGER NANGLE,NAN2,NAN3,NAN4,NAN5,NAN6,NAN7,NAN8,NAN9
      INTEGER NF,NF2,NF3,NF4,NF5,NF6,NF7,NF8,NF9
      PARAMETER (NSEGME=30607, NPSEGM=26365, NPLIST=26089, NPPLIS=26252)
      PARAMETER (NID=14560, NPID=25964, NVERIF=-3419)
      PARAMETER (NANGLE=2167, NAN2=2192, NAN3=2193, NAN4=2194)
      PARAMETER (NAN5=2195, NAN6=2196, NAN7=2197, NAN8=2198, NAN9=2199)
      PARAMETER (NF=9600, NF2=10880, NF3=10920, NF4=10960, NF5=11000)
      PARAMETER (NF6=11040, NF7=11080, NF8=11120, NF9=11160)
C
      INTEGER NKEY(9),NAME(9)
C
      DATA NKEY / NANGLE,NAN2,NAN3,NAN4,NAN5,NAN6,NAN7,NAN8,NAN9 /
      DATA NAME / NF,NF2,NF3,NF4,NF5,NF6,NF7,NF8,NF9 /
C
C If key SEGMENT is set to zero, picture number for segmented picture
C is obtained from key PSEGMENT
C
      IF (IVAL(NSEGME).EQ.0) THEN
C
C If key PSEGMENT is set, use it to determine picture number
C
         IF (VARSET(NPSEGM)) THEN
            NPIC=IVALPN(NPSEGM)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPSEGM
            GOTO 160
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 160
C
C If key PLIST is set to zero, picture number for particle parameter
C list is obtained from key PPLIST
C
      IF (IVAL(NPLIST).EQ.0) THEN
C
C If key PPLIST is set, use it to determine picture number
C
         IF (VARSET(NPPLIS)) THEN
            NPIC=IVALPN(NPPLIS)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPPLIS
            GOTO 160
         ENDIF
C
C Otherwise, use key value to determine picture number
C
      ELSE
         NPIC=IVALPN(NPLIST)
      ENDIF
C
C Open particle parameter list
C
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 160
C
C Fault non-plist class for particle parameter list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 160
      ENDIF
C
C Fault bad size for particle parameter list
C
      IF (NROW.NE.1.OR.NLAY.LT.NYMAX) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 160
      ENDIF
C
C Fetch value for key ID
C
      ID=IVAL(NID)
C
C If key ID is set to zero, particle id is obtained from key PID
C
      IF (ID.EQ.0) THEN
C
C If key PID is set, use it to determine particle id
C
         IF (VARSET(NPID)) THEN
            ID=IVAL(NPID)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPID
            GOTO 160
         ENDIF
      ENDIF
C
C Fault zero or negative or too large value for key ID
C
      IF (ID.LE.0) THEN
         ERROR=3
         IDERR=NID
         GOTO 160
      ENDIF
C
C Read particle id's from particle parameter list
C
      IF (SEMROW(1,RB2,NFMFP,1,NIDENT,LP2)) GOTO 160
C
C Look for specified particle id
C
      DO 10 I=1,NCOL
         IF (ID.EQ.NINT(RB2(I))) GOTO 20
   10 CONTINUE
C
C Fault failure to locate specified particle id
C
      ERROR=155
      GOTO 160
C
C Fetch segmented picture centre position
C
   20 CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Determine particle limits in row/column terms
C
      IF (SEMROW(1,RB2,NFMFP,1,NXMIN,LP2)) GOTO 160
      I1=CCOL+NINT(RB2(I))
C
      IF (SEMROW(1,RB2,NFMFP,1,NXMAX,LP2)) GOTO 160
      I2=CCOL+NINT(RB2(I))
C
      IF (SEMROW(1,RB2,NFMFP,1,NYMAX,LP2)) GOTO 160
      J1=CROW-NINT(RB2(I))
C
      IF (SEMROW(1,RB2,NFMFP,1,NYMIN,LP2)) GOTO 160
      J2=CROW-NINT(RB2(I))
C
C Fetch particle reference point
C
      IF (SEMROW(1,RB2,NFMFP,1,NXREF,LP2)) GOTO 160
      XREF=RB2(I)
C
      IF (SEMROW(1,RB2,NFMFP,1,NYREF,LP2)) GOTO 160
      YREF=RB2(I)
C
C First feret angle specified by key ANGLE
C
      N=1
      ANGLE(1)=VAL(NKEY(1))
C
C Fetch values for keys AN2,AN3,AN4,AN5,AN6,AN7,AN8,AN9 which are set
C
      DO 30 I=2,9
C
C If key is set, store key value
C
         IF (VARSET(NKEY(I))) THEN
            N=I
            ANGLE(I)=VAL(NKEY(I))
C
C Otherwise, stop looking for more keys
C
         ELSE
            GOTO 40
         ENDIF
   30 CONTINUE
C
C Set up cosines and sines of feret angles and use particle reference
C point to set up initial values for feret limits
C
   40 DO 50 I=1,N
C
C Determine cosine and sine of feret angle
C
         C(I)=COS(ANGLE(I))
         S(I)=SIN(ANGLE(I))
C
C Project reference point onto feret direction
C
         F=C(I)*XREF+S(I)*YREF
C
C Use this value to initialise feret limits
C
         FMIN(I)=F
         FMAX(I)=F
   50 CONTINUE
C
C Initialise particle flag
C
      LFOUND=.FALSE.
C
C Scan through region of segmented picture which contains particle
C
      DO 110 J=J1,J2
C
C Read row from segmented picture
C
         IF (SEMROW(1,RB1,NFMINT,J,1,LP1)) GOTO 160
C
C Look for first pixel (if any) set to particle id
C
         DO 60 I=I1,I2
            IF (IB1(I).EQ.ID) GOTO 70
   60    CONTINUE
C
C None found, move on to next row in segmented picture
C
         GOTO 110
C
C Determine X position for this pixel
C
   70    X1=REAL(I-CCOL)
C
C Look for last pixel set to particle id
C
         DO 80 I=I2,I1,-1
            IF (IB1(I).EQ.ID) GOTO 90
   80    CONTINUE
C
C Determine X position for this pixel
C
   90    X2=REAL(I-CCOL)
C
C Determine Y position for row
C
         Y=REAL(CROW-J)
C
C Update feret limits
C
         DO 100 I=1,N
C
C Project two pixel positions onto feret direction
C
            F1=C(I)*X1+S(I)*Y
            F2=C(I)*X2+S(I)*Y
C
C Update feret limits
C
            FMIN(I)=MIN(FMIN(I),F1,F2)
            FMAX(I)=MAX(FMAX(I),F1,F2)
  100    CONTINUE
C
C Set particle flag
C
         LFOUND=.TRUE.
  110 CONTINUE
C
C If particle present in segmented picture, results are defined
C
      IF (LFOUND) THEN
C
C Set up final results
C
         DO 120 I=1,N
            FERET(I)=FMAX(I)-FMIN(I)+1.0
  120    CONTINUE
C
C Print results (unless option NOVERIFY is set)
C
         IF (.NOT.OPTNO(NVERIF)) THEN
            IF (SEMCON(' ')) GOTO 160
            IF (SEMCON('   Angle  Feret diameter')) GOTO 160
C
            DO 140 I=1,N
               WRITE (RECORD,130) (180.0/PI)*ANGLE(I),FERET(I)
  130          FORMAT (F10.3,3X,F10.1)
               IF (SEMCON(RECORD)) GOTO 160
  140       CONTINUE
         ENDIF
C
C Set return variables F,F2,F3,F4,F5,F6,F7,F8,F9
C
         DO 150 I=1,N
            IF (SEMLU(1,NAME(I),FERET(I))) GOTO 160
  150    CONTINUE
C
C Otherwise, fault absence of particle in segmented picture
C
      ELSE
         ERROR=160
      ENDIF
C
  160 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
