C Semper 6 subsidiary module PMOMEN
C
      SUBROUTINE PMOMEN
C
C Given a segmented picture and particle parameter list, calculates
C seven invariant moments for the particle selected by means of the
C key ID.  The moment values are calculated independently of the
C position, size and orientation of the particle.  They are printed
C on the console (unless option NOVERIFY is set) and returned in the
C variables M1, M2, M3, M4, M5, M6 and M7.  If the IMAGE key is set,
C intensity-weighted values for the moments are calculated instead.
C
C Syntax: Pmoment :PMOMEN verify $1=0 id=$1 $2=0 segment=$2 +
C             $3=0 plist=$3 image=
C
      INTEGER IVAL,IVALPN,IPACK
      LOGICAL VARSET,SEMOPN,SEMROW,SEMLU,OPTNO,SEMCON
C
      LOGICAL LIMAGE,LFOUND
      INTEGER NPIC,NCOL,NROW,NLAY,CLASS,FORM
      INTEGER CCOL,CROW,I,I1,I2,ID,IS0,J,J1,J2
      REAL X,Y,XC,YC,M00,M10,M01,M20,M11,M02,M30,M21,M12,M03
      REAL RS0,RS1,RS2,RS3,RSI0,RSI1,RSI2,RSI3,SCALE
      REAL CM20,CM11,CM02,CM30,CM21,CM12,CM03
      REAL E1,E2,E3,E4,E5,E6,M1,M2,M3,M4,M5,M6,M7,VALU(7)
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
      INTEGER NIMAGE,NSEGME,NPSEGM,NPLIST,NPPLIS,NID,NPID,NVERIF
      PARAMETER (NIMAGE=14921)
      PARAMETER (NSEGME=30607, NPSEGM=26365, NPLIST=26089, NPPLIS=26252)
      PARAMETER (NID=14560, NPID=25964, NVERIF=-3419)
C
      CHARACTER*2 NAME(7)
      DATA NAME / 'm1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7' /
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
            GOTO 90
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 90
C
C See if IMAGE key is set
C
      LIMAGE=VARSET(NIMAGE)
C
C If so, open source picture
C
      IF (LIMAGE) THEN
         NPIC=IVALPN(NIMAGE)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 90
C
C Fault difference between segmented and source picture sizes
C
         IF (NCOL.NE.NCOLS(LP2).OR.NROW.NE.NROWS(LP2)) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 90
         ENDIF
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
            GOTO 90
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP3)) GOTO 90
C
C Fault non-plist class for particle parameter list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 90
      ENDIF
C
C Fault bad size for particle parameter list
C
      IF (NROW.NE.1.OR.NLAY.LT.NYMAX) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 90
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
            GOTO 90
         ENDIF
      ENDIF
C
C Fault zero or negative value for key ID
C
      IF (ID.LE.0) THEN
         ERROR=3
         IDERR=NID
         GOTO 90
      ENDIF
C
C Read particle id's from particle parameter list
C
      IF (SEMROW(1,RB3,NFMFP,1,NIDENT,LP3)) GOTO 90
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
      GOTO 90
C
C Fetch segmented picture centre position
C
   20 CCOL=CCOLN(LP2)
      CROW=CROWN(LP2)
C
C Determine particle limits in row/column terms
C
      IF (SEMROW(1,RB3,NFMFP,1,NXMIN,LP3)) GOTO 90
      I1=CCOL+NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NXMAX,LP3)) GOTO 90
      I2=CCOL+NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NYMAX,LP3)) GOTO 90
      J1=CROW-NINT(RB3(I))
C
      IF (SEMROW(1,RB3,NFMFP,1,NYMIN,LP3)) GOTO 90
      J2=CROW-NINT(RB3(I))
C
C Initialise overall sums
C
      M00=0.0
      M10=0.0
      M01=0.0
      M20=0.0
      M11=0.0
      M02=0.0
      M30=0.0
      M21=0.0
      M12=0.0
      M03=0.0
C
C Initialise particle flag
C
      LFOUND=.FALSE.
C
C Scan through region of segmented picture which contains particle
C
      DO 40 J=J1,J2
C
C Read row from segmented picture
C
         IF (SEMROW(1,RB2,NFMINT,J,1,LP2)) GOTO 90
C
C Determine corresponding Y value
C
         Y=REAL(CROW-J)
C
C Initialise row sums
C
         IF (LIMAGE) THEN
            RSI0=0.0
            RSI1=0.0
            RSI2=0.0
            RSI3=0.0
C
C Also read row from source picture
C
            IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 90
         ELSE
            IS0=0
            RS1=0.0
            RS2=0.0
            RS3=0.0
         ENDIF
C
C Process row
C
         DO 30 I=I1,I2
C
C See if pixel is set to specified particle id
C
            IF (IB2(I).EQ.ID) THEN
C
C Particle exists
C
               LFOUND=.TRUE.
C
C Determine corresponding X value
C
               X=REAL(I-CCOL)
C
C Evaluate row sums
C
               IF (LIMAGE) THEN
                  RSI0=RSI0+RB1(I)
                  RSI1=RSI1+RB1(I)*X
                  RSI2=RSI2+RB1(I)*X*X
                  RSI3=RSI3+RB1(I)*X*X*X
               ELSE
                  IS0=IS0+1
                  RS1=RS1+X
                  RS2=RS2+X*X
                  RS3=RS3+X*X*X
               ENDIF
            ENDIF
   30    CONTINUE
C
C Update overall sums
C
         IF (LIMAGE) THEN
            M00=M00+RSI0
            M10=M10+RSI1
            M01=M01+RSI0*Y
            M20=M20+RSI2
            M11=M11+RSI1*Y
            M02=M02+RSI0*Y*Y
            M30=M30+RSI3
            M21=M21+RSI2*Y
            M12=M12+RSI1*Y*Y
            M03=M03+RSI0*Y*Y*Y
         ELSE
            RS0=REAL(IS0)
C
            M00=M00+RS0
            M10=M10+RS1
            M01=M01+RS0*Y
            M20=M20+RS2
            M11=M11+RS1*Y
            M02=M02+RS0*Y*Y
            M30=M30+RS3
            M21=M21+RS2*Y
            M12=M12+RS1*Y*Y
            M03=M03+RS0*Y*Y*Y
         ENDIF
   40 CONTINUE
C
C If particle present in segmented picture, particle sums are defined
C
      IF (LFOUND) THEN
C
C Determine centroid position
C
         XC=M10/M00
         YC=M01/M00
C
C Determine central moments (position independent)
C
         CM20=M20-M00*XC*XC
         CM11=M11-M00*XC*YC
         CM02=M02-M00*YC*YC
         CM30=M30-3.0*M20*XC+2.0*M00*XC*XC*XC
         CM21=M21-M20*YC-2.0*M11*XC+2.0*M00*XC*XC*YC
         CM12=M12-M02*XC-2.0*M11*YC+2.0*M00*XC*YC*YC
         CM03=M03-3.0*M02*YC+2.0*M00*YC*YC*YC
C
C Normalize 2nd and 3rd order central moments (size independent)
C
         SCALE=M00*M00
C
         CM20=CM20/SCALE
         CM11=CM11/SCALE
         CM02=CM02/SCALE
C
         SCALE=SCALE*SQRT(M00)
C
         CM30=CM30/SCALE
         CM21=CM21/SCALE
         CM12=CM12/SCALE
         CM03=CM03/SCALE
C
C Determine invariant moments (orientation independent)
C
         E1=CM20+CM02
         E2=CM20-CM02
         E3=CM30+CM12
         E4=CM03+CM21
         E5=CM30-3.0*CM12
         E6=CM03-3.0*CM21
C
         M1=E1
         M2=E2*E2+4.0*CM11*CM11
         M3=E5*E5+E6*E6
         M4=E3*E3+E4*E4
         M5=E5*E3*(E3*E3-3.0*E4*E4)+E6*E4*(E4*E4-3.0*E3*E3)
         M6=E2*(E3*E3-E4*E4)+4.0*CM11*E3*E4
         M7=E5*E4*(E4*E4-3.0*E3*E3)-E6*E3*(E3*E3-3.0*E4*E4)
C
C Set up array of return values
C
         VALU(1)=M1
         VALU(2)=M2
         VALU(3)=M3
         VALU(4)=M4
         VALU(5)=M5
         VALU(6)=M6
         VALU(7)=M7
C
C Print results unless option NOVERIFY is set
C
         IF (.NOT.OPTNO(NVERIF)) THEN
            IF (SEMCON(' ')) GOTO 90
            WRITE (RECORD,50) M1
   50       FORMAT ('Invariant moments',E15.6)
            IF (SEMCON(RECORD)) GOTO 90
            DO 70 I=2,7
               WRITE (RECORD,60) VALU(I)
   60          FORMAT ('                 ',E15.6)
               IF (SEMCON(RECORD)) GOTO 90
   70       CONTINUE
         ENDIF
C
C Set return variables M1, M2, M3, M4, M5, M6 and M7
C
         DO 80 I=1,7
            IF (SEMLU(1,IPACK(NAME(I)),VALU(I))) GOTO 90
   80    CONTINUE
C
C Otherwise, fault absence of particle in segmented picture
C
      ELSE
         ERROR=160
      ENDIF
C
   90 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
