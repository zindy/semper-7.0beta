C Semper 6 processing module PMARK
C
      SUBROUTINE PMARK
C
C Marks positions on a specified display picture.  The positions
C correspond to selected particles.  If the option CM is set, a
C particle's centre of area is used.  Otherwise, the particle's
C reference point is used.  If a parameter option is set, the positions
C are marked with a text string representing the specified parameter
C value.  Otherwise, the position is marked with a symbol.  'IF' and
C 'UNLESS' expressions specify conditions for selecting particles.
C All particles are selected if neither condition is specified.
C The options XREF, YREF, HOLES, XMIN, XMAX, YMIN, YMAX, HFERET, VFERET,
C AFERET, BFERET, VPROJ, HPROJ, PERIMETER, AREA, XCEN, YCEN, MMIN, MMAX,
C ANGLE and CIRCULARITY specify which parameter is to be used when
C marking with a parameter value.
C
      LOGICAL VARSET,OPT,SEMLU,SEMOPN,SEMROW,SEMEXP,SEMXA1
      LOGICAL FSINIT,FSTEXT,FSMARK
      INTEGER IVAL,IVALPN
C
      INCLUDE 'COMMON'
C
      LOGICAL LIF,LUNLES,LEXPIF,LEXPUN
      INTEGER CLASS,FORM,TEXT(15)
C
      INTEGER NPPAR
      PARAMETER (NPPAR=25)
C
      INTEGER NAME(NPPAR)
C
      INTEGER NPBLK
      PARAMETER (NPBLK=4*(LNBUF/(NPPAR*LNREAL)))
C
      INTEGER IB6(LNBUF/LNINT)
      REAL    RPBUFF(NPPAR,NPBLK)
C
      EQUIVALENCE (RPBUFF,RB1),(IB6,RB6)
      INTEGER NPIC,NCOL,NROW,NLAY,IPPAR,K,NPAR,IPTRIF,IPTRUN,J,I1,I2,N
      INTEGER I,IPTR,K1,K2
      REAL VALUE
C
C Packed names
      INTEGER NPLIST,NPPLIS,NIF,NUNLES,NPICTU,NCM,NXREF,NYREF,NID
      INTEGER NPAREN,NHOLES,NBACKG,NCONTA,NXMIN,NXMAX,NYMIN,NYMAX
      INTEGER NHFERE,NVFERE,NAFERE,NBFERE,NVPROJ,NHPROJ,NPERIM,NAREA
      INTEGER NXCEN,NYCEN,NMMIN,NMMAX,NANGLE,NCIRCU
      PARAMETER (NPLIST=26089, NPPLIS=26252, NIF=14640, NUNLES=-2173)
      PARAMETER (NPICTU=25963, NCM=5320)
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
            GOTO 110
         ENDIF
C
C Otherwise, use key value to determine picture number
      ELSE
         NPIC=IVALPN(NPLIST)
      ENDIF
C
C Open particle parameter list
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 110
C
C Fault non-plist class for particle parameter list
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 110
      ENDIF
C
C Fault bad size for particle parameter list
      IF (NROW.NE.1.OR.NLAY.LT.NPPAR) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 110
      ENDIF
C
C Initialise flag to detect presence of first particle parameter option
      IPPAR=0
C
C Save current settings of parameter variables
      DO 10 K=1,NPPAR
         IF (VARSET(NAME(K))) THEN
            IF (IPPAR.EQ.0) IPPAR = K
         ENDIF
         IF (SEMLU(3,NAME(K),VALUE)) GOTO 110
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
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 110
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
                  IF (SEMLU(1,NAME(K),RPBUFF(K,N))) GOTO 110
   40          CONTINUE
C
C Determine logical value of 'IF' string (default is TRUE)
               IF (LIF) THEN
                  IPTR=IPTRIF
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +            GOTO 110
                  LEXPIF=VALUE.NE.0.0
               ELSE
                  LEXPIF=.TRUE.
               ENDIF
C
C Determine logical value of 'UNLESS' string (default is FALSE)
               IF (LUNLES) THEN
                  IPTR=IPTRUN
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +               GOTO 110
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
         GOTO 110
C
C Otherwise, carry on with normal processing
      ELSE
C
C Initialise display graphics
         IF (FSINIT(3,IVALPN(NPICTU))) GOTO 110
C
C Annotate display picture only if 2-D image
         IF (FSPTYP.EQ.1) THEN
C
C If option CM is set, use centre of area for positioning annotation
            IF (OPT(NCM)) THEN
               K1=20
               K2=21
C
C Otherwise, use particle reference point
            ELSE
               K1=1
               K2=2
            ENDIF
C
C Read X and Y positioning data from particle parameter file
            IF (SEMROW(1,RB1,NFMFP,1,K1,LP1)) GOTO 110
            IF (SEMROW(1,RB2,NFMFP,1,K2,LP1)) GOTO 110
C
C If particle parameter option is set, mark selected particles with
C specified parameter value
            IF (IPPAR.NE.0) THEN
C
C Read specified parameter values from particle parameter file
               IF (SEMROW(1,RB3,NFMFP,1,IPPAR,LP1)) GOTO 110
C
C If parameter is orientation angle, convert angle from radians
C to degrees
               IF (NAME(IPPAR).EQ.NANGLE) THEN
                  DO 80 I=1,NCOL
                     RB3(I)=180.0*RB3(I)/PI
   80             CONTINUE
               ENDIF
C
C Mark each particle in turn
               DO 90 I=1,NPAR
C
C Convert parameter value to text string
                  N=1
                  IF (SEMXA1(4,TEXT,15,N,RB3(IB6(I)),0)) GOTO 110
                  N=N-1
C
C Draw text string
                  IF (FSTEXT(TEXT,N,RB1(IB6(I)),RB2(IB6(I)),0,0))
     +               GOTO 110
   90          CONTINUE
C
C Otherwise, mark selected particles with symbols
            ELSE
               DO 100 I=1,NPAR
                  IF (FSMARK(RB1(IB6(I)),RB2(IB6(I)),FSMMOD,FSMSIZ))
     +               GOTO 110
  100          CONTINUE
            ENDIF
         ENDIF
      ENDIF
C
  110 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
