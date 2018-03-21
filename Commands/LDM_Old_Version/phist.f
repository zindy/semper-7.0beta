C Semper 6 processing module PHIST
C
      SUBROUTINE PHIST
C
C Produces a histogram class picture for a specified set of particle
C parameter values.  The histogram range is determined from the data
C itself, unless option PRESET is set, in which case the range is taken
C from variables MIN and MAX.  Key CHANNELS, if set, specifies the
C number of histogram channels required.  The data is obtained from a
C specified particle parameter file.  'IF' and 'UNLESS' expressions
C specify conditions for selecting particles.  All particles are
C selected if neither condition is specified.  The options XREF, YREF,
C HOLES, XMIN, XMAX, YMIN, YMAX, HFERET, VFERET, AFERET, BFERET, VPROJ,
C HPROJ, PERIMETER, AREA, XCEN, YCEN, MMIN, MMAX, ANGLE and CIRCULARITY
C specify which parameter is to be used.  If the output picture is a
C display picture, the histogram is output straight to the display.
C
      INTEGER IVAL,IVALPN
      LOGICAL VARSET,OPT,SEMLU,SEMOPN,SEMROW,SEMEXP,SEMMED
C
      INCLUDE 'COMMON'
C
      REAL H,HDIFF,HMAX,HMIN,VALUE
      INTEGER CLASS,FORM,I,I1,I2,IH,IPPAR,IPTR,IPTRIF,IPTRUN,J,K
      INTEGER MEDIUM,N,NCHAN,NCOL,NLAY,NOUT,NPAR,NPIC,NROW,OPC
      LOGICAL LIF,LUNLES,LEXPIF,LEXPUN,LDISP
C
      INTEGER NPPAR
      PARAMETER (NPPAR=25)
C
      INTEGER NPBLK
      PARAMETER (NPBLK=4*(LNBUF/(NPPAR*LNREAL)))
C
      INTEGER IB5(LNBUF/LNINT),IB6(LNBUF/LNINT)
      REAL RPBUFF(NPPAR,NPBLK)
C
      EQUIVALENCE (RPBUFF,RB1),(IB5,RB5),(IB6,RB6)
C
C Packed names
C
      INTEGER NPLIST,NPPLIS,NIF,NUNLES,NFROM,NTO,NPRESE,NCHANN
      INTEGER NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA
      INTEGER NXMIN,NXMAX,NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE
      INTEGER NVPROJ,NHPROJ,NPERIM,NAREA
      INTEGER NXCEN,NYCEN,NMMIN,NMMAX,NANGLE,NCIRCU
C
      PARAMETER (NPLIST=26089, NPPLIS=26252, NIF=14640, NUNLES=-2173)
      PARAMETER (NFROM=10335, NTO=-601, NPRESE=26325, NCHANN=5121)
      PARAMETER (NXREF=-7126, NYREF=-8726, NID=14560, NPAREN=25658)
      PARAMETER (NHOLES=13412, NBACKG=3243, NCONTA=5414)
      PARAMETER (NXMIN=-6930, NXMAX=-6922, NYMIN=-8530, NYMAX=-8522)
      PARAMETER (NHFERE=13045, NVFERE=-3446, NAFERE=1845, NBFERE=3445)
      PARAMETER (NVPROJ=-3859, NHPROJ=13458, NPERIM=25818, NAREA=2325)
      PARAMETER (NXCEN=-6526, NYCEN=-8126, NMMIN=21329, NMMAX=21321)
      PARAMETER (NANGLE=2167, NCIRCU=5178)
      INTEGER NAME(NPPAR)
C
      DATA NAME / NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA,
     +            NXMIN,NXMAX,NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE,
     +            NVPROJ,NHPROJ,NPERIM,NAREA,NXCEN,NYCEN,NMMIN,NMMAX,
     +            NANGLE,NCIRCU /
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
            GOTO 120
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 120
C
C Fault non-plist class for particle parameter list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 120
      ENDIF
C
C Fault bad size for particle parameter list
C
      IF (NROW.NE.1.OR.NLAY.LT.NPPAR) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 120
      ENDIF
C
C Initialise flag to detect presence of first particle parameter option
C
      IPPAR=0
C
C Save current settings of parameter variables
C
      DO 10 K=1,NPPAR
         IF (VARSET(NAME(K))) THEN
            IF (IPPAR.EQ.0) IPPAR=K
         ENDIF
         IF (SEMLU(3,NAME(K),VALUE)) GOTO 120
   10 CONTINUE
C
C See if text keys IF and UNLESS are set
C
      LIF=VARSET(NIF)
      LUNLES=VARSET(NUNLES)
C
C Determine which and how many particles are selected
C
      NPAR=0
C
C If key IF or UNLESS is set, a particle is selected if the 'IF'
C string value is TRUE and the 'UNLESS' string value is FALSE
C
      IF (LIF.OR.LUNLES) THEN
C
C Fetch pointers to 'IF' and 'UNLESS' strings
C
         IPTRIF=IVAL(NIF)
         IPTRUN=IVAL(NUNLES)
C
C Process the particle data in blocks
C
         DO 60 J=1,NCOL,NPBLK
C
C Determine range of particles in this block
C
            I1=J
            I2=MIN(J+NPBLK-1,NCOL)
C
C Fetch values for each particle parameter in turn
C
            DO 30 K=1,NPPAR
C
C Read values from source picture
C
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 120
C
C Copy values to buffer array
C
               N=0
               DO 20 I=I1,I2
                  N=N+1
                  RPBUFF(K,N)=RB5(I)
   20          CONTINUE
   30       CONTINUE
C
C See if each particle in block is selected
C
            N=0
            DO 50 I=I1,I2
C
               N=N+1
C
C Set parameter variables to the corresponding particle parameter value
C
               DO 40 K=1,NPPAR
                  IF (SEMLU(1,NAME(K),RPBUFF(K,N))) GOTO 120
   40          CONTINUE
C
C Determine logical value of 'IF' string (default is TRUE)
C
               IF (LIF) THEN
                  IPTR=IPTRIF
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +            GOTO 120
                  LEXPIF=VALUE.NE.0.0
               ELSE
                  LEXPIF=.TRUE.
               ENDIF
C
C Determine logical value of 'UNLESS' string (default is FALSE)
C
               IF (LUNLES) THEN
                  IPTR=IPTRUN
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +               GOTO 120
                  LEXPUN=VALUE.NE.0.0
               ELSE
                  LEXPUN=.FALSE.
               ENDIF
C
C If the 'IF' string is TRUE and the 'UNLESS' string is FALSE, add this
C particle to the list of selected particles
C
               IF (LEXPIF.AND.(.NOT.LEXPUN)) THEN
                  NPAR=NPAR+1
                  IB6(NPAR)=I
               ENDIF
   50       CONTINUE
   60    CONTINUE
C
C Otherwise, all particles are selected
C
      ELSE
C
C Add all particles to list of selected particles
C
         DO 70 I=1,NCOL
            NPAR=NPAR+1
            IB6(NPAR)=I
   70    CONTINUE
      ENDIF
C
C See if any particles have been selected
C
      IF (NPAR.EQ.0) THEN
C
C Fault no particles being selected
C
         ERROR=158
         GOTO 120
C
C Otherwise, carry on with normal processing
C
      ELSE
C
C If a particle parameter option is set, carry on with producing
C histogram
C
         IF (IPPAR.NE.0) THEN
C
C Read selected particle parameter values
C
            IF (SEMROW(1,RB4,NFMFP,1,IPPAR,LP1)) GOTO 120
C
C Determine histogram range
C
            IF (OPT(NPRESE)) THEN
C
C Option PRESET is set, get range from variables MIN and MAX
C
               HMIN=VMIN
               HMAX=VMAX
C
C Otherwise, get range from minimum and maximum parameter values
C
            ELSE
C
C Set up initial minimum and maximum values
C
               HMIN=RB4(IB6(1))
               HMAX=RB4(IB6(1))
C
C Scan through rest of data
C
               DO 80 I=2,NPAR
                  HMIN=MIN(HMIN,RB4(IB6(I)))
                  HMAX=MAX(HMAX,RB4(IB6(I)))
   80          CONTINUE
            ENDIF
C
C Determine number of histogram channels
C
            IF (VARSET(NCHANN)) THEN
C
C If key CHANNELS is set, fetch its value
C
               NCHAN=IVAL(NCHANN)
C
C Fault value less than 1
C
               IF (NCHAN.LT.1) THEN
                  ERROR=3
                  IDERR=NCHANN
                  GOTO 120
               ENDIF
C
C Otherwise, set up default for number of channels
C
            ELSE
C
C Determine range of parameter values
C
               HDIFF=HMAX-HMIN
C
C If range is too small or too large, number of channels defaults to 256
C
               IF (HDIFF.LT.20.0.OR.HDIFF.GT.255.0) THEN
                  NCHAN=256
C
C Otherwise, numbers of channels chosen for unit counting interval
C
               ELSE
                  NCHAN=INT(HDIFF)+1
               ENDIF
            ENDIF
C
C Fault number of channels if too large for row buffer size
C
            IF (NCHAN+2.GT.LNBUF/LNREAL) THEN
               ERROR=55
               GOTO 120
            ENDIF
C
C Initialise channel counts
C
            DO 90 I=1,NCHAN
               IB5(I)=0
   90       CONTINUE
C
C Carry out histogram count
C
            DO 100 I=1,NPAR
C
C Fetch next selected particle parameter value
C
               H=RB4(IB6(I))
C
C Reject value outside histogram range
C
               IF (H.LT.HMIN.OR.H.GT.HMAX) GOTO 100
C
C Increment corresponding histogram channel count
C
               IH=INT(REAL(NCHAN)*((H-HMIN)/(HMAX-HMIN)))
C
               IF (IH.LT.NCHAN) IH=IH+1
C
               IB5(IH)=IB5(IH)+1
  100       CONTINUE
C
C See if histogram is to be output to the display
C
            NOUT=IVALPN(NTO)
            IF (SEMMED(NOUT/1000,MEDIUM)) GOTO 120
C
            LDISP=MEDIUM.EQ.MEDDS
C
C If outputing to the display, make output picture temporary, otherwise
C make it permanent
C
            IF (LDISP) THEN
               OPC=3
            ELSE
               OPC=2
            ENDIF
C
C Open output histogram picture
C
            LP2=0
            IF (SEMOPN(OPC,NOUT,NCHAN+2,1,1,NCLHIS,NFMFP,LP2)) GOTO 120
C
C Set up final results in output row buffer
C
            DO 110 I=1,NCHAN
               RB4(I)=REAL(IB5(I))
  110       CONTINUE
C
            RB4(NCHAN+1)=HMIN
            RB4(NCHAN+2)=HMAX
C
C Store final results
C
            IF (SEMROW(2,RB4,NFMFP,1,1,LP2)) GOTO 120
C
C If outputing to display, call display routine
C
            IF (LDISP) THEN
               LP1=LP2
               IF (SEMLU(2,NFROM,REAL(NOUT))) GOTO 120
               LBLINC=.FALSE.
               CALL DISP
            ENDIF
         ENDIF
      ENDIF
C
C Restore current settings of parameter variables
C
  120 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
