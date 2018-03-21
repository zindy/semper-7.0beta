C Semper 6 processing module PICTUR
C
      SUBROUTINE PICTUR
C
C Returns picture parameters in Semper variables.  Options are provided
C to specify which groups of parameters to return.  All the variable
C names are short names so that they do not clash with key or option
C names for other command.  This command is intended to replace the
C existing PCB command.
C
C   Picture parameters                  Option       Variables set
C
C   Dimensions                          SIZE         NX,NY,NZ
C   Coordinate limits                   LIMITS       X1,X2,Y1,Y2,Z1,Z2
C   Class                               CLASS        C
C   Data form                           FORM         F
C   Write protection                    WP           W
C   Creation date                       DATE         YD,MD,DD
C   Creation time                       TIME         HT,MT,ST
C   Intensity range                     RANGE        I1,I2
C   Intensity sum                       SUM          S,S2
C   Intensity mean and s.d.             STATISTICS   M,M2,SD
C   Display black and white levels      BW           DB,DW
C   Display sampling                    SAMPLING     DS
C   Position list type                  TYPE         PT
C
C The last four variables listed above only apply to certain types of
C pictures.  The variables S2 and M2 are set to zero if the source
C picture is of complex form.  If the picture parameters requested are
C not defined, the corresponding variables will be returned unset.
C
C If option ALL is specified, the full set of picture parameters is
C returned.
C
C     INTEGER IPACK
      LOGICAL OPT,SEMLAB,SEMMED,SETVAR,UNSETV,SEMRNG,SEMROW
C
      REAL    P,PMIN,PMAX,S,PMI,PMI2,PSD
      REAL    PSI,PSI2,PSII,PSII2
      REAL    LSI,LSI2,LSII,LSII2
      REAL    RSI,RSI2,RSII,RSII2
      INTEGER MEDIUM,I,J,K
      LOGICAL LALL
C
      INCLUDE 'COMMON'
C
      INTEGER LABEL(LNLAB)
C
      EQUIVALENCE (LABEL,RB1)
C
C See if option ALL is set
C
      LALL=OPT(2092)
C
C If option WP, DATE, TIME or TYPE specified, fetch picture label
C
      IF (OPT(-5441).OR.OPT(6460).OR.
     +    OPT(-374).OR.OPT(-1017).OR.LALL) THEN
         IF (SEMLAB(1,LABEL,LP1)) GOTO 90
      ENDIF
C
C If option BW or SAMPLING specified, fetch medium number for picture
C
      IF (OPT(4120).OR.OPT(30453).OR.LALL) THEN
         IF (SEMMED(DEVN(LP1),MEDIUM)) GOTO 90
      ENDIF
C
C If SIZE option is set, return picture dimensions in variables
C NX, NY and NZ
C
      IF (OPT(30786).OR.LALL) THEN
         IF (SETVAR(23360,REAL(NCOLS(LP1)))) GOTO 90
         IF (SETVAR(23400,REAL(NROWS(LP1)))) GOTO 90
         IF (SETVAR(23440,REAL(NLAYS(LP1)))) GOTO 90
      ENDIF
C
C If LIMITS option is set, return picture limits relative to picture
C origin in variables X1, X2, Y1, Y2, Z1 and Z2
C
      IF (OPT(19573).OR.LALL) THEN
         IF (SETVAR(-7641,REAL(1-CCOLN(LP1)))) GOTO 90
         IF (SETVAR(-7681,REAL(NCOLS(LP1)-CCOLN(LP1)))) GOTO 90
         IF (SETVAR(-9241,REAL(CROWN(LP1)-NROWS(LP1)))) GOTO 90
         IF (SETVAR(-9281,REAL(CROWN(LP1)-1))) GOTO 90
         IF (SETVAR(-10841,REAL(1-CLAYN(LP1)))) GOTO 90
         IF (SETVAR(-10881,REAL(NLAYS(LP1)-CLAYN(LP1)))) GOTO 90
      ENDIF
C
C If CLASS option is set, return picture class in variable C
C
      IF (OPT(5281).OR.LALL) THEN
         IF (SETVAR(4800,REAL(CLASSN(LP1)))) GOTO 90
      ENDIF
C
C If FORM option is set, return picture data form in variable F
C
      IF (OPT(10218).OR.LALL) THEN
         IF (SETVAR(9600,REAL(FORMN(LP1)))) GOTO 90
      ENDIF
C
C If WP option is set, return picture write protect flag in variable w
C
      IF (OPT(-5441).OR.LALL) THEN
         IF (SETVAR(-4801,REAL(MIN(LABEL(LBWP),1)))) GOTO 90
      ENDIF
C
C If DATE option is set, return picture creation date in variables
C YD, MD and DD
C
      IF (OPT(6460).OR.LALL) THEN
         IF (SETVAR(-8161,REAL(LABEL(LBYEAR)))) GOTO 90
         IF (SETVAR(20960,REAL(LABEL(LBMON)))) GOTO 90
         IF (SETVAR(6560,REAL(LABEL(LBDAY)))) GOTO 90
      ENDIF
C
C If TIME option is set, return picture creation time in variables
C HT, MT and ST
C
      IF (OPT(-374).OR.LALL) THEN
         IF (SETVAR(13600,REAL(LABEL(LBHOUR)))) GOTO 90
         IF (SETVAR(21600,REAL(LABEL(LBMIN)))) GOTO 90
         IF (SETVAR(31200,REAL(LABEL(LBSEC)))) GOTO 90
      ENDIF
C
C If RANGE option is set, return picture range in varibales I1 and I2
C
      IF (OPT(28854).OR.LALL) THEN
C
C Fetch picture range
C
         IF (SEMRNG(1,PMIN,PMAX,LP1)) GOTO 90
C
C Set return variables
C
         IF (SETVAR(15640,PMIN)) GOTO 90
         IF (SETVAR(15680,PMAX)) GOTO 90
      ENDIF
C
C If SUM option is set, return picture intensity sum in variables S
C and S2 (imaginary part of complex picture)
C
      IF (OPT(31253).OR.LALL) THEN
C
C Initialise intensity sums
C
         PSI=0.0
         PSI2=0.0
C
C Sum picture intensities (real + imaginary parts for complex picture)
C
         DO 40 K=1,NLAYS(LP1)
C
C Initialise layer sums
C
            LSI=0.0
            LSI2=0.0
C
C Sum intensities across layer
C
            DO 30 J=1,NROWS(LP1)
C
C Process according to non-complex/complex data form
C
               IF (FORMN(LP1).NE.NFMCOM) THEN
C
C Fetch picture row in floating-point form
C
                  IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 90
C
C Initialise row sum
C
                  RSI=0.0
C
C Sum intensities along row
C
                  DO 10 I=1,NCOLS(LP1)
                     RSI=RSI+RB1(I)
   10             CONTINUE
C
C Add row sum to layer sum
C
                  LSI=LSI+RSI
               ELSE
C
C Fetch picture row in complex form
C
                  IF (SEMROW(1,RB1,NFMCOM,J,K,LP1)) GOTO 90
C
C Initialise row sums
C
                  RSI=0.0
                  RSI2=0.0
C
C Sum intensities along row
C
                  DO 20 I=1,2*NCOLS(LP1),2
                     RSI=RSI+RB1(I)
                     RSI2=RSI2+RB1(I+1)
   20             CONTINUE
C
C Add row sums to layer sums
C
                  LSI=LSI+RSI
                  LSI2=LSI2+RSI2
               ENDIF
   30       CONTINUE
C
C Add layer sums to overall sums
C
            PSI=PSI+LSI
            PSI2=PSI2+LSI2
   40    CONTINUE
C
C Set return variables
C
         IF (SETVAR(30400,PSI)) GOTO 90
         IF (SETVAR(31680,PSI2)) GOTO 90
      ENDIF
C
C If STATISTICS option is set, return picture intensity mean and
C standard deviation in variables M, M2 (for mean of imaginary part
C of complex picture) and SDI
C
      IF (OPT(31201).OR.LALL) THEN
C
C Initialise intensity sums
C
         PSI=0.0
         PSI2=0.0
         PSII=0.0
         PSII2=0.0
C
C Sum picture intensities (real + imaginary parts for complex picture)
C
         DO 80 K=1,NLAYS(LP1)
C
C Initialise layer sums
C
            LSI=0.0
            LSI2=0.0
            LSII=0.0
            LSII2=0.0
C
C Sum intensities across layer
C
            DO 70 J=1,NROWS(LP1)
C
C Process according to non-complex/complex data form
C
               IF (FORMN(LP1).NE.NFMCOM) THEN
C
C Fetch picture row in floating-point form
C
                  IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 90
C
C Initialise row sums
C
                  RSI=0.0
                  RSII=0.0
C
C Sum data along row (sum + sum of squares)
C
                  DO 50 I=1,NCOLS(LP1)
                     P=RB1(I)
                     RSI=RSI+P
                     RSII=RSII+P*P
   50             CONTINUE
C
C Add row sums to layer sums
C
                  LSI=LSI+RSI
                  LSII=LSII+RSII
               ELSE
C
C Fetch picture row in complex form
C
                  IF (SEMROW(1,RB1,NFMCOM,J,K,LP1)) GOTO 90
C
C Initialise row sums
C
                  RSI=0.0
                  RSI2=0.0
                  RSII=0.0
                  RSII2=0.0
C
C Sum data along row (sum + sum of squares)
C
                  DO 60 I=1,2*NCOLS(LP1),2
                     P=RB1(I)
                     RSI=RSI+P
                     RSII=RSII+P*P
                     P=RB1(I+1)
                     RSI2=RSI2+P
                     RSII2=RSII2+P*P
   60             CONTINUE
C
C Add row sums to layer sums
C
                  LSI=LSI+RSI
                  LSI2=LSI2+RSI2
                  LSII=LSII+RSII
                  LSII2=LSII2+RSII2
               ENDIF
   70       CONTINUE
C
C Add layer sums to overall sums
C
            PSI=PSI+LSI
            PSI2=PSI2+LSI2
            PSII=PSII+LSII
            PSII2=PSII2+LSII2
   80    CONTINUE
C
C Calculate mean and standard deviation
C
         S=REAL(NCOLS(LP1))*REAL(NROWS(LP1))*REAL(NLAYS(LP1))
C
         PMI=PSI/S
         PMI2=PSI2/S
C
         PSD=SQRT(MAX((PSII/S-PMI*PMI)+(PSII2/S-PMI2*PMI2),0.0))
C
C Set return variables
C
         IF (SETVAR(20800,PMI)) GOTO 90
         IF (SETVAR(22080,PMI2)) GOTO 90
         IF (SETVAR(30560,PSD)) GOTO 90
      ENDIF
C
C If BW option is set and picture is a display picture, return display
C black and white levels in variables DB and DW, otherwise, unset the
C variables
C
      IF (OPT(4120).OR.LALL) THEN
         IF (MEDIUM.EQ.MEDDS) THEN
            IF (SETVAR(6480,REAL(GSMIN(LP1)))) GOTO 90
            IF (SETVAR(7320,REAL(GSMAX(LP1)))) GOTO 90
         ELSE
            IF (UNSETV(6480)) GOTO 90
            IF (UNSETV(7320)) GOTO 90
         ENDIF
      ENDIF
C
C If SAMPLING option is set and picture is a display picture, return 
C display
C sampling interval in variable DS, otherwise, unset the variable
C
      IF (OPT(30453).OR.LALL) THEN
         IF (MEDIUM.EQ.MEDDS) THEN
            IF (SETVAR(7160,REAL(PXSAM(LP1)))) GOTO 90
         ELSE
            IF (UNSETV(7160)) GOTO 90
         ENDIF
      ENDIF
C
C If TYPE option is set and picture is a position list, return position
C list type in variable PT, otherwise, unset the variable
C
      IF (OPT(-1017).OR.LALL) THEN
         IF (CLASSN(LP1).EQ.NCLPLI) THEN
            IF (SETVAR(26400,REAL(LABEL(LBPLTY)))) GOTO 90
         ELSE
            IF (UNSETV(26400)) GOTO 90
         ENDIF
      ENDIF
C
   90 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
