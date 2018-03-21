C Semper 6 subsidiary module PCALC
C
      SUBROUTINE PCALC
C
C Given a source picture and the corresponding segmented picture and
C particle parameter list, calculates extra parameter for the particle
C selected by means of the key ID.  The extra parameters calculated are
C minimum, maximum, integrated and mean intensity and the associated
C standard deviation, centres of mass and second moments of mass.  The
C results are printed on the terminal (unless option NOVERIFY is set)
C and returned in the variables I1,I2,SI,MI,SDI,XCM,YCM,MM1,MM2 and PHI.
C Further particle parameters may be added at a later date, as the need
C arises.
C
      INTEGER IVAL,IVALPN
      LOGICAL VARSET,SEMOPN,SEMROW,SEMLU,OPTNO,SEMCON
C
      INTEGER NPPAR
      PARAMETER (NPPAR=10)
C
      LOGICAL LFOUND
      INTEGER NCOL,NLAY,NPIC,NROW
      INTEGER CLASS,CCOL,CROW,FORM,I,I1,I2,I3,ID,ISUM,J,J1,J2,J3
      REAL MEANI,MMMIN,MMMAX,P,PHI,PX,PY,RSMI,RSMII,RSMIX,RSMIXX
      REAL SUM,SUMI,SUMII,SUMIRR,SUMIX,SUMIXX,SUMIXY,SUMIY,SUMIYY
      REAL VALU(NPPAR),X,XCM,Y,YCM,MINI,MAXI,SDI
C
      INCLUDE 'COMMON'
C
      INTEGER IB2(LNBUF/LNINT)
C
      EQUIVALENCE (IB2,RB2)
C
      INTEGER NXREF,NYREF,NIDENT,NXMIN,NXMAX,NYMIN,NYMAX
      PARAMETER (NXREF=1, NYREF=2, NIDENT=3)
      PARAMETER (NXMIN=8, NXMAX=9, NYMIN=10, NYMAX=11)
C
C Packed names
C
      INTEGER NIMAGE,NPIMAG,NSEGME,NPSEGM,NPLIST,NPPLIS,NID,NPID
      INTEGER NVERIF,NSI,NMI,NXCM,NYCM,NMM1,NMM2,NPHI,NI1,NI2,NSDI
      PARAMETER (NIMAGE=14921, NPIMAG=25973)
      PARAMETER (NSEGME=30607, NPSEGM=26365, NPLIST=26089, NPPLIS=26252)
      PARAMETER (NID=14560, NPID=25964, NVERIF=-3419)
      PARAMETER (NSI=30760, NMI=21160, NXCM=-6534, NYCM=-8134)
      PARAMETER (NMM1=21351, NMM2=21352, NPHI=25929)
      PARAMETER (NI1=15640, NI2=15680, NSDI=30569)
C
      INTEGER NVAR(NPPAR)
      DATA NVAR / NSI,NMI,NXCM,NYCM,NMM1,NMM2,NPHI,NI1,NI2,NSDI /
C
C If key IMAGE is set to zero, picture number for source picture
C is obtained from key PIMAGE
C
      IF (IVAL(NIMAGE).EQ.0) THEN
C
C If key PIMAGE is set, use it to determine picture number
C
         IF (VARSET(NPIMAG)) THEN
            NPIC=IVALPN(NPIMAG)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPIMAG
            GOTO 130
         ENDIF
C
C Otherwise, use key value to determine picture number
C
      ELSE
         NPIC=IVALPN(NIMAGE)
      ENDIF
C
C Open source picture
C
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 130
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
            GOTO 130
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 130
C
C Fault difference between segmented and source picture sizes
C
      IF (NCOL.NE.NCOLS(LP1).OR.NROW.NE.NROWS(LP1)) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 130
      ENDIF
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
            GOTO 130
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP3)) GOTO 130
C
C Fault non-plist class for particle parameter list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 130
      ENDIF
C
C Fault bad size for particle parameter list
C
      IF (NROW.NE.1.OR.NLAY.LT.NYMAX) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 130
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
            GOTO 130
         ENDIF
      ENDIF
C
C Fault zero or negative value for key ID
C
      IF (ID.LE.0) THEN
         ERROR=3
         IDERR=NID
         GOTO 130
      ENDIF
C
C Read particle id's from particle parameter list
C
      IF (SEMROW(1,RB3,NFMFP,1,NIDENT,LP3)) GOTO 130
C
C Look for specified particle id
C
      DO 10 I=1,NCOL
         IF (ID.EQ.NINT(RB3(I))) GOTO 20
   10 CONTINUE
C
C Fault failure to locate specified particle id
C
      ERROR=155
      GOTO 130
C
C Fetch segmented picture centre position
C
   20 CCOL=CCOLN(LP2)
      CROW=CROWN(LP2)
C
C Determine particle limits in row/column terms
C
      IF (SEMROW(1,RB3,NFMFP,1,NXMIN,LP3)) GOTO 130
      I1=CCOL+NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NXMAX,LP3)) GOTO 130
      I2=CCOL+NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NYMAX,LP3)) GOTO 130
      J1=CROW-NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NYMIN,LP3)) GOTO 130
      J2=CROW-NINT(RB3(I))
C
C Fetch particle reference point
C
      IF (SEMROW(1,RB3,NFMFP,1,NXREF,LP3)) GOTO 130
      I3=CCOL+NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NYREF,LP3)) GOTO 130
      J3=CROW-NINT(RB3(I))
C
C Read row containing reference point from source picture
C
      IF (SEMROW(1,RB1,NFMFP,J3,1,LP1)) GOTO 130
C
C Use source pixel value at particle reference point to initialise
C intensity range values
C
      MINI=RB1(I3)
      MAXI=RB1(I3)
C
C Initialise overall sums
C
      SUM=0.0
      SUMI=0.0
      SUMII=0.0
      SUMIX=0.0
      SUMIY=0.0
      SUMIXX=0.0
      SUMIYY=0.0
      SUMIXY=0.0
C
C Initialise particle flag
C
      LFOUND=.FALSE.
C
C Scan through region of segmented picture which contains particle
C
      DO 40 J=J1,J2
C
C Read row from source picture
C
         IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 130
C
C Read row from segmented picture
C
         IF (SEMROW(1,RB2,NFMINT,J,1,LP2)) GOTO 130
C
C Determine corresponding Y value
C
         Y=REAL(CROW-J)
C
C Initialise row sums
C
         ISUM=0
         RSMI=0.0
         RSMII=0.0
         RSMIX=0.0
         RSMIXX=0.0
C
C Process row
C
         DO 30 I=I1,I2
C
C See if pixel is set to specified particle id
C
            IF (IB2(I).EQ.ID) THEN
C
C Determine corresponding X value
C
               X=REAL(I-CCOL)
C
C Evaluate row sums
C
               ISUM=ISUM+1
               RSMI=RSMI+RB1(I)
               RSMII=RSMII+RB1(I)*RB1(I)
               RSMIX=RSMIX+RB1(I)*X
               RSMIXX=RSMIXX+RB1(I)*X*X
C
C Update intensity range
C
               MINI=MIN(RB1(I),MINI)
               MAXI=MAX(RB1(I),MAXI)
            ENDIF
   30    CONTINUE
C
C Update overall sums
C
         SUM=SUM+REAL(ISUM)
         SUMI=SUMI+RSMI
         SUMII=SUMII+RSMII
         SUMIX=SUMIX+RSMIX
         SUMIY=SUMIY+RSMI*Y
         SUMIXX=SUMIXX+RSMIXX
         SUMIYY=SUMIYY+RSMI*Y*Y
         SUMIXY=SUMIXY+RSMIX*Y
C
C If non-zero sum, particle exists
C
         IF (ISUM.NE.0) LFOUND=.TRUE.
   40 CONTINUE
C
C If particle present in segmented picture, particle sums are defined
C
      IF (LFOUND) THEN
C
C Determine mean intensity
C
         MEANI=SUMI/SUM
C
C Determine standard deviation for intensity
C
         SDI=SQRT(MAX(SUMII/SUM-MEANI*MEANI,0.0))
C
C Determine centre of mass
C
         XCM=SUMIX/SUMI
         YCM=SUMIY/SUMI
C
C Determine second moments about centre of mass
C
         SUMIXX=SUMIXX/SUMI-XCM*XCM
         SUMIYY=SUMIYY/SUMI-YCM*YCM
         SUMIXY=SUMIXY/SUMI-XCM*YCM
C
C Determine principal second moments of mass
C
         SUMIRR=SUMIXX+SUMIYY
         PX=(SUMIXX-SUMIYY)/2.0
         PY=SUMIXY
         P=SQRT(PX*PX+PY*PY)
         MMMIN=SUMIRR/2.0-P
         MMMAX=SUMIRR/2.0+P
C
C Determine orientation of long axis (axis with least moment)
C
         IF (P.EQ.0.0) THEN
            PHI=0.0
         ELSE
            PHI=ATAN2(PY,PX)/2.0
         ENDIF
C
C Print results unless option NOVERIFY is set
C
         IF (.NOT.OPTNO(NVERIF)) THEN
            IF (SEMCON(' ')) GOTO 130
            WRITE (RECORD,50) MINI,MAXI
   50       FORMAT ('Intensity range   ',2F15.2)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,60) SUMI
   60       FORMAT ('Total intensity   ',E15.6)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,70) MEANI
   70       FORMAT ('Mean intensity    ',F15.2)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,80) SDI
   80       FORMAT ('Standard deviation',F15.2)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,90) XCM,YCM
   90       FORMAT ('Centre of mass    ',2F15.2)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,100) MMMIN,MMMAX
  100       FORMAT ('Min,max 2nd moment',2E15.6)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,110) (180.0/PI)*PHI
  110       FORMAT ('Angle (degrees)   ',F15.2)
            IF (SEMCON(RECORD)) GOTO 130
         ENDIF
C
C Set up array of return values
C
         VALU(1)=SUMI
         VALU(2)=MEANI
         VALU(3)=XCM
         VALU(4)=YCM
         VALU(5)=MMMIN
         VALU(6)=MMMAX
         VALU(7)=PHI
         VALU(8)=MINI
         VALU(9)=MAXI
         VALU(10)=SDI
C
C Set return variables SI,MI,XCM,YCM,MM1,MM2,PHI,I1,I2 and SDI
C
         DO 120 I=1,NPPAR
            IF (SEMLU(1,NVAR(I),VALU(I))) GOTO 130
  120    CONTINUE
C
C Otherwise, fault absence of particle in segmented picture
C
      ELSE
         ERROR=160
      ENDIF
C
  130 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
