C Semper 6 processing module PSET
C
      SUBROUTINE PSET
C
C Sets the specified particle parameter variables.  The variable
C names are XR, YR, PID, PA, H, BG, EC, X1, X2, Y1, Y2, HF, VF, AF,
C BF, VP, HP, P, A, XC, YC, M1, M2, THETA and C respectively.  For a
C variable to be set, there must at least as many particles selected as
C the value of the INDEX key.  The INDEX key specifies which amongst the
C series of selected particles to use,  If the INDEX key exceeds the
C number of selected particles, all particle parameter variables are
C left unset.  The data for setting these variables is obtained from a
C specified particle parameter file.  'IF' and 'UNLESS' expressions
C specify conditions for selecting particles.  All particles are
C selected if neither condition is specified.  The options XREF, YREF,
C ID, PARENT, HOLES, BACKGROUND, CONTACT, XMIN, XMAX, YMIN, YMAX,
C HFERET, VFERET, AFERET, BFERET, VPROJ, HPROJ, PERIMETER, AREA, XCEN,
C YCEN, MMIN, MMAX, ANGLE and CIRCULARITY specify which parameter
C variables are to be set.  If option ALL is set, all the variables are
C set.  The SORT key may be used to reorder the data according to the
C specified parameter.  The SORT key accepts any of the parameter
C options above as its argument.  The options ASCENDING (default) and
C DESCENDING specify the direction for sorting.  If option COUNT is set,
C the variable N is set to the number of particles selected.  Otherwise,
C it is left unset.  The value in N can subsequently be used as a loop
C count for stepping through the selected particles by means of the
C INDEX key.
C
      LOGICAL VARSET,OPT,SEMLU,SEMOPN,SEMROW,SEMEXP
      INTEGER IVALPN,IVAL
C
      INCLUDE 'COMMON'
C
      INTEGER NPPAR
      PARAMETER (NPPAR=25)
C
      INTEGER NAME(NPPAR),NVAR(NPPAR)
      LOGICAL ISET(NPPAR)
C
      INTEGER NPBLK
      PARAMETER (NPBLK=4*(LNBUF/(NPPAR*LNREAL)))
C
      INTEGER IB6(LNBUF/LNINT)
      REAL    RPBUFF(NPPAR,NPBLK)
      EQUIVALENCE (RPBUFF,RB1),(IB6,RB6)
C
      LOGICAL LSORT,LDESCE,LIF,LUNLES,LEXPIF,LEXPUN,LALL
      INTEGER CLASS,FORM,NPIC,NROW,NCOL,NLAY,IND,K,IPTRIF,IPTRUN
      INTEGER J,I1,I2,N,I,IPTR,NPAR
      REAL VALUE
C
C Packed names
C
      INTEGER NPLIST,NPPLIS,NIF,NUNLES,NSORT,NASCEN,NDESCE,NINDEX,NALL
      INTEGER NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA,NXMIN,NXMAX
      INTEGER NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE,NVPROJ,NHPROJ
      INTEGER NPERIM,NAREA,NXCEN,NYCEN,NMMIN,NMMAX,NANGLE,NCIRCU,NCOUNT
      INTEGER NXR,NYR,NPID,NPA,NH,NBG,NEC,NX1,NX2,NY1,NY2,NHF,NVF,NAF
      INTEGER NBF,NVP,NHP,NP,NA,NXC,NYC,NM1,NM2,NTHETA,NC,NN
      PARAMETER (NPLIST=26089, NPPLIS=26252, NIF=14640, NUNLES=-2173)
      PARAMETER (NSORT=31018, NASCEN=2363, NDESCE=6619)
      PARAMETER (NINDEX=14964, NALL=2092)
      PARAMETER (NXREF=-7126, NYREF=-8726, NID=14560, NPAREN=25658)
      PARAMETER (NHOLES=13412, NBACKG=3243, NCONTA=5414)
      PARAMETER (NXMIN=-6930, NXMAX=-6922, NYMIN=-8530, NYMAX=-8522)
      PARAMETER (NHFERE=13045, NVFERE=-3446, NAFERE=1845, NBFERE=3445)
      PARAMETER (NVPROJ=-3859, NHPROJ=13458, NPERIM=25818, NAREA=2325)
      PARAMETER (NXCEN=-6526, NYCEN=-8126, NMMIN=21329, NMMAX=21321)
      PARAMETER (NANGLE=2167, NCIRCU=5178, NCOUNT=5421)
      PARAMETER (NXR=-7121, NYR=-8721, NPID=25964, NPA=25640)
      PARAMETER (NH=12800, NBG=3480, NEC=8120)
      PARAMETER (NX1=-7641, NX2=-7681, NY1=-9241, NY2=-9281)
      PARAMETER (NHF=13040, NVF=-3441, NAF=1840, NBF=3440)
      PARAMETER (NVP=-3841, NHP=13440, NP=25600, NA=1600)
      PARAMETER (NXC=-6521, NYC=-8121, NM1=22040, NM2=22080)
      PARAMETER (NTHETA=-326, NC=4800, NN=22400)
C
      DATA NAME / NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA,
     +            NXMIN,NXMAX,NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE,
     +            NVPROJ,NHPROJ,NPERIM,NAREA,NXCEN,NYCEN,NMMIN,NMMAX,
     +            NANGLE,NCIRCU /
C
      DATA NVAR / NXR,NYR,NPID,NPA,NH,NBG,NEC,NX1,NX2,NY1,NY2,NHF,NVF,
     +            NAF,NBF,NVP,NHP,NP,NA,NXC,NYC,NM1,NM2,NTHETA,NC /
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
            GOTO 100
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 100
C
C Fault non-plist class for particle parameter list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 100
      ENDIF
C
C Fault bad size for particle parameter list
C
      IF (NROW.NE.1.OR.NLAY.LT.NPPAR) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 100
      ENDIF
C
C See if key SORT is set
C
      LSORT=VARSET(NSORT)
C
C If so, check for conflicting options ASCENDING and DESCENDING
C
      IF (LSORT) THEN
C
C See if option DESCENDING is set
C
         LDESCE=OPT(NDESCE)
C
C Fault conflict with option ASCENDING
C
         IF (LDESCE.AND.OPT(NASCEN)) THEN
            ERROR=60
            IDERR=NASCEN
            IDERR2=NDESCE
            GOTO 100
         ENDIF
      ENDIF
C
C See if option ALL is set
C
         LALL=OPT(NALL)
C
C Flag variables to set and make setting of parameter variables local
C
      DO 10 K=1,NPPAR
         ISET(K)=VARSET(NAME(K)).OR.LALL
         IF (SEMLU(3,NAME(K),VALUE)) GOTO 100
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
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 100
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
                  IF (SEMLU(1,NAME(K),RPBUFF(K,N))) GOTO 100
   40          CONTINUE
C
C Determine logical value of 'IF' string (default is TRUE)
C
               IF (LIF) THEN
                  IPTR=IPTRIF
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +            GOTO 100
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
     +               GOTO 100
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
C If option COUNT is set, set variable N to number of selected particles
C and skip processing INDEX key
C
      IF (OPT(NCOUNT)) THEN
         IF (SEMLU(1,NN,REAL(NPAR))) GOTO 100
         GOTO 100
      ENDIF
C
C See if any particles selected
C
      IF (NPAR.GT.0) THEN
C
C Fetch value for key INDEX
C
         IND=IVAL(NINDEX)
C
C Fault bad value for INDEX key
C
         IF (IND.LT.1.OR.IND.GT.NPAR) THEN
            ERROR=3
            IDERR=NINDEX
            GOTO 100
         ENDIF
C
C If SORT key is set, it specifies a particle parameter to sort on
C
         IF (LSORT) THEN
C
C Set parameter variables to values 1 to n
C
            DO 80 K=1,NPPAR
               IF (SEMLU(1,NAME(K),REAL(K))) GOTO 100
   80       CONTINUE
C
C Fetch pointer to 'SORT' string
C
            IPTR=IVAL(NSORT)
C
C Determine value of 'SORT' string
C
            IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.)) GOTO 100
C
C Fault bad value for 'SORT' string
C
            IF (VALUE.LT.REAL(1).OR.VALUE.GT.REAL(NPPAR)) THEN
               ERROR=3
               IDERR=NSORT
               GOTO 100
            ENDIF
C
C 'SORT' string value selects particle parameter to sort on
C
            K=NINT(VALUE)
C
C Read selected particle parameter values
C
            IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 100
C
C Sort list of selected particles according to selected parameter
C
            CALL PSORT(RB5,IB6,NPAR,LDESCE)
         ENDIF
C
C Set specified particle parameter variables
C
         DO 90 K=1,NPPAR
C
C See if particle parameter variable is flagged to be set
C
            IF (ISET(K)) THEN
C
C Read parameter values
C
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 100
C
C Set corresponding variable
C
               IF (SEMLU(1,NVAR(K),RB5(IB6(IND)))) GOTO 100
            ENDIF
   90    CONTINUE
      ENDIF
C
  100 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
