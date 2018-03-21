C Semper 6 processing module PSHOW
C
      SUBROUTINE PSHOW
C
C Highlights particles on the display.  It is assumed here that the
C segmented picture created by the ANALYSE verb has been displayed
C with ont-to-one intensity scaling.  This implies that this command
C can not successfully be used with images segmented pictures that have
C more particles than there are hardware look-up table entries.  The
C data is obtained from a specified particle parameter file.  'IF' and
C 'UNLESS' expressions specify conditions for selecting particles.  All
C particles are selected if neither condition is specified.  The SORT
C key may be used to re-order the data according to the specified
C parameter.  The SORT key accepts any of the parameter options XREF,
C YREF, ID, PARENT, HOLES, BACKGROUND, CONTACT, XMIN, XMAX, YMIN, YMAX,
C HFERET, VFERET, AFERET, BFERET, VPROJ, HPROJ, PERIMETER, AREA, XCEN,
C YCEN, MMIN, MMAX, ANGLE and CIRCULARITY as its argument.  The options
C ASCENDING (default) and DESCENDING specify the direction for sorting.
C The background is displayed as black and non-selected particles are
C displayed as mid-grey.  The default highlight colour is white, but if
C the key SATURATION is set to a non-zero value not exceeding 1.0,
C selected particles will be highlighted in colour.  The range of
C highlight colours used is specified by the keys HUE and HU2.  HU2
C defaults to HUE if not set.  HUE and HU2 are specified in degrees
C (red = 0,360,etc., green = 120,etc., blue = 240,etc.).  The first
C and last selected particles are dispalyed with colours given by
C HUE and HU2 respectively, with interpolated hue values for intervening
C particles.
C
      LOGICAL VARSET,OPT,SEMLU,SEMOPN,SEMROW,SEMEXP,SEMLUT,FSLU61
      INTEGER IVALPN,IVAL
      REAL VAL
C
      INCLUDE 'COMMON'
C
      LOGICAL LSORT,LDESCE,LIF,LUNLES,LEXPIF,LEXPUN
      INTEGER CLASS,FORM
      REAL    RGB(0:6,3)
C
      INTEGER NIDENT,NPPAR
      PARAMETER (NIDENT=3)
      PARAMETER (NPPAR=25)
C
      INTEGER NAME(NPPAR)
C
      INTEGER NPBLK
      PARAMETER (NPBLK=4*(LNBUF/(NPPAR*LNREAL)))
C
      INTEGER LUT(3*LUTSIZ),IB6(LNBUF/LNINT)
      REAL    RPBUFF(NPPAR,NPBLK)
      EQUIVALENCE (LUT,RPBUFF,RB1),(IB6,RB6)
C
      INTEGER NPIC,NCOL,NROW,NLAY,K,NPAR,IPTRIF,IPTRUN,J,I1,I2,N,I
      INTEGER IPTR,MODE,ID,IH,ILUT
      REAL SATURN,VALUE,HU1,HU2,DHUE,HUE,H,DH
C
C Packed names
      INTEGER NPLIST,NPPLIS,NIF,NUNLES,NSORT,NASCEN,NDESCE,NHUE,NHU2
      INTEGER NSATUR,NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA
      INTEGER NXMIN,NXMAX,NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE
      INTEGER NVPROJ,NHPROJ,NPERIM,NAREA,NXCEN,NYCEN,NMMIN,NMMAX
      INTEGER NANGLE,NCIRCU
      PARAMETER (NPLIST=26089, NPPLIS=26252, NIF=14640, NUNLES=-2173)
      PARAMETER (NSORT=31018, NASCEN=2363, NDESCE=6619)
      PARAMETER (NHUE=13645, NHU2=13672, NSATUR=30460)
      PARAMETER (NXREF=-7126, NYREF=-8726, NID=14560, NPAREN=25658)
      PARAMETER (NHOLES=13412, NBACKG=3243, NCONTA=5414)
      PARAMETER (NXMIN=-6930, NXMAX=-6922, NYMIN=-8530, NYMAX=-8522)
      PARAMETER (NHFERE=13045, NVFERE=-3446, NAFERE=1845, NBFERE=3445)
      PARAMETER (NVPROJ=-3859, NHPROJ=13458, NPERIM=25818, NAREA=2325)
      PARAMETER (NXCEN=-6526, NYCEN=-8126, NMMIN=21329, NMMAX=21321)
      PARAMETER (NANGLE=2167, NCIRCU=5178)
C
      DATA NAME / NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA,
     +            NXMIN,NXMAX,NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE,
     +            NVPROJ,NHPROJ,NPERIM,NAREA,NXCEN,NYCEN,NMMIN,NMMAX,
     +            NANGLE,NCIRCU /
C
      DATA RGB / 0.0,0.0,1.0,1.0,1.0,0.0,0.0,
     +           1.0,0.0,0.0,0.0,1.0,1.0,1.0,
     +           1.0,1.0,1.0,0.0,0.0,0.0,1.0 /
C
C If key PLIST is set to zero, picture number for particle parameter
C list is obtained from key PPLIST
      IF (IVAL(NPLIST).EQ.0) THEN
C
C If key PPLIST is set, use it to determine picture number
         IF (VARSET(NPPLIS)) THEN
            NPIC=IVALPN(NPPLIS)
C
C Otherwise, fault its being unset
         ELSE
            ERROR=25
            IDERR=NPPLIS
            GOTO 150
         ENDIF
C
C Otherwise, use key value to determine picture number
      ELSE
         NPIC=IVALPN(NPLIST)
      ENDIF
C
C Open particle parameter list
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 150
C
C Fault non-plist class for particle parameter list
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 150
      ENDIF
C
C Fault bad size for particle parameter list
      IF (NROW.NE.1.OR.NLAY.LT.NPPAR) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 150
      ENDIF
C
C See if key SORT is set
      LSORT=VARSET(NSORT)
C
C If so, check for conflicting options ASCENDING and DESCENDING
      IF (LSORT) THEN
C
C See if option DESCENDING is set
         LDESCE=OPT(NDESCE)
C
C Fault conflict with option ASCENDING
         IF (LDESCE.AND.OPT(NASCEN)) THEN
            ERROR=60
            IDERR=NASCEN
            IDERR2=NDESCE
            GOTO 150
         ENDIF
      ENDIF
C
C Fetch value of SATURATION key
      SATURN=VAL(NSATUR)
C
C Fault saturation value outside the range 0.0 to 1.0
      IF (SATURN.LT.0.0.OR.SATURN.GT.1.0) THEN
         ERROR=3
         IDERR=NSATUR
         GOTO 150
      ENDIF
C
C Save current settings of parameter variables
C
      DO 10 K=1,NPPAR
         IF (SEMLU(3,NAME(K),VALUE)) GOTO 150
   10 CONTINUE
C
C See if text keys IF and UNLESS are set
      LIF=VARSET(NIF)
      LUNLES=VARSET(NUNLES)
C
C Determine which and how many particles are selected
      NPAR=0
C
C If key IF or UNLESS is set, a particle is selected if the 'IF'
C string value is TRUE and the 'UNLESS' string value is FALSE
      IF (LIF.OR.LUNLES) THEN
C
C Fetch pointers to 'IF' and 'UNLESS' strings
         IPTRIF=IVAL(NIF)
         IPTRUN=IVAL(NUNLES)
C
C Process the particle data in blocks
         DO 60 J=1,NCOL,NPBLK
C
C Determine range of particles in this block
            I1=J
            I2=MIN(J+NPBLK-1,NCOL)
C
C Fetch values for each particle parameter in turn
            DO 30 K=1,NPPAR
C
C Read values from source picture
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 150
C
C Copy values to buffer array
               N=0
               DO 20 I=I1,I2
                  N=N+1
                  RPBUFF(K,N)=RB5(I)
   20          CONTINUE
   30       CONTINUE
C
C See if each particle in block is selected
            N=0
            DO 50 I=I1,I2
C
               N=N+1
C
C Set parameter variables to the corresponding particle parameter value
               DO 40 K=1,NPPAR
                  IF (SEMLU(1,NAME(K),RPBUFF(K,N))) GOTO 150
   40          CONTINUE
C
C Determine logical value of 'IF' string (default is TRUE)
               IF (LIF) THEN
                  IPTR=IPTRIF
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +               GOTO 150
                  LEXPIF=VALUE.NE.0.0
               ELSE
                  LEXPIF=.TRUE.
               ENDIF
C
C Determine logical value of 'UNLESS' string (default is FALSE)
               IF (LUNLES) THEN
                  IPTR=IPTRUN
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +               GOTO 150
                  LEXPUN=VALUE.NE.0.0
               ELSE
                  LEXPUN=.FALSE.
               ENDIF
C
C If the 'IF' string is TRUE and the 'UNLESS' string is FALSE, add this
C particle to the list of selected particles
               IF (LEXPIF.AND.(.NOT.LEXPUN)) THEN
                  NPAR=NPAR+1
                  IB6(NPAR)=I
               ENDIF
   50       CONTINUE
   60    CONTINUE
C
C Otherwise, all particles are selected
      ELSE
C
C Add all particles to list of selected particles
         DO 70 I=1,NCOL
            NPAR=NPAR+1
            IB6(NPAR)=I
   70    CONTINUE
      ENDIF
C
C See if any particles have been selected
      IF (NPAR.EQ.0) THEN
C
C Fault no particles being selected
         ERROR=158
         GOTO 150
C
C Otherwise, carry on with normal processing
      ELSE
C
C If SORT key is set, it specifies a particle parameter to sort on
         IF (LSORT) THEN
C
C Set parameter variables to values 1 to n
            DO 80 K=1,NPPAR
               IF (SEMLU(1,NAME(K),REAL(K))) GOTO 150
   80       CONTINUE
C
C Fetch pointer to 'SORT' string
            IPTR=IVAL(NSORT)
C
C Determine value of 'SORT' string
            IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.)) GOTO 150
C
C Fault bad value for 'SORT' string
            IF (VALUE.LT.REAL(1).OR.VALUE.GT.REAL(NPPAR)) THEN
               ERROR=3
               IDERR=NSORT
               GOTO 150
            ENDIF
C
C 'SORT' string value selects particle parameter to sort on
            K=NINT(VALUE)
C
C Read selected particle parameter values
            IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 150
C
C Sort list of selected particles according to selected parameter
            CALL PSORT(RB5,IB6,NPAR,LDESCE)
         ENDIF
C
C If zero saturation value, monochrome look-up table is generated
         IF (SATURN.EQ.0.0) THEN
C
C Set look-up table mode for monochrome display
            MODE=1
C
C Otherwise, false colour look-up table is generated
         ELSE
C
C Set look-up table mode for false colour display
            MODE=2
C
C Fetch value of HUE key
            HU1=VAL(NHUE)
C
C Determine value for HU2 key (default = HUE)
            IF (VARSET(NHU2)) THEN
               HU2=VAL(NHU2)
            ELSE
               HU2=HU1
            ENDIF
C
C Determine increment for interpolating hue
            IF (NPAR.EQ.1) THEN
               DHUE=0.0
            ELSE
               DHUE=(HU2-HU1)/REAL(NPAR-1)
            ENDIF
         ENDIF
C
C Set background look-up table entry to black and remaining entries to
C mid-grey
         DO 100 J=1,3
            LUT(1+(J-1)*LUTLEN)=0
            DO 90 I=2+(J-1)*LUTLEN,J*LUTLEN
               LUT(I)=LUTMAX/2
   90       CONTINUE
  100    CONTINUE
C
C Read particle id's from particle parameter file
         IF (SEMROW(1,RB4,NFMFP,1,NIDENT,LP1)) GOTO 150
C
C Change look-up table to highlight selected particles
         DO 120 I=1,NPAR
C
C Convert pointer to particle into particle id
            ID=NINT(RB4(IB6(I)))
C
C If particle id within range of look-up table entries, modify the
C corresponding table entry
            IF (ID.LT.LUTLEN) THEN
C
C See if monchrome look-up table is required
               IF (MODE.EQ.1) THEN
C
C If so, set corresponding entry to white
                  LUT(ID+1)=LUTMAX
C
C Otherwise, set corresponding entry to required highlight colour
C (interpolated hue and specified saturation)
               ELSE
C
C Determine value of interpolated hue
                  HUE=HU1+REAL(I-1)*DHUE
C
C Convert hue to red, green and blue intensities
                  H=MOD(HUE/60.0,6.0)
                  IF (H.LT.0.0) H=H+6.0
                  IH=INT(H)
                  DH=H-REAL(IH)
C
                  DO 110 J=1,3
                     H=(1.0-DH)*RGB(IH,J)+DH*RGB(IH+1,J)
                     LUT(ID+1+(J-1)*LUTLEN)=
     +                  NINT(REAL(LUTMAX)*(1.0-SATURN*H))
  110             CONTINUE
               ENDIF
            ENDIF
  120    CONTINUE
C
C Fetch current look-up table number
         ILUT=INT(CLUT)
C
C Update look-up table values on work disc
         IF (SEMLUT(2,ILUT,MODE,LUT)) GOTO 150
C
C Update look-up table in framestore
         IF (FSLU61(2,ILUT,MODE,LUT,ERROR)) GOTO 150
      ENDIF
C
  150 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
