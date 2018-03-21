C Semper 6 processing module PEDIT
C
      SUBROUTINE PEDIT
C
C Produces new particle parameter file and/or segmented picture.  The
C key TO, if set, specifies a particle parameter file to be output.
C The key SEGMENT specifies a new segmented picture to be output.  The
C key specifies the source picture number.  If key SE2 is set, it
C specifies the output picture number (default is the source picture).
C What particle data is copied to the output picture depends on the
C keys IF and UNLESS.  If neither key is set, all particles are
C selected.  The keys specify general logical expressions which must
C be satisfied for a particle to be selected.  For the segmented
C output picture, the particle id's of any particle that is not
C selected is set to zero.
C
      LOGICAL VARSET,SEMLU,SEMOPN,SEMROW,SEMEXP
      INTEGER SEMFRM,IVAL,IVALPN
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM
      LOGICAL LIF,LUNLES,LEXPIF,LEXPUN
C
      INTEGER NIDENT,NPPAR
      PARAMETER (NIDENT=3)
      PARAMETER (NPPAR=25)
C
      INTEGER NAME(NPPAR)
C
      INTEGER MAXPIX,NPBLK
      PARAMETER (MAXPIX=LNBUF/LNINT)
      PARAMETER (NPBLK=4*(LNBUF/(NPPAR*LNREAL)))
C
      INTEGER IB4(MAXPIX),IB5(MAXPIX),IB6(MAXPIX)
      REAL    RPBUFF(NPPAR,NPBLK)
      EQUIVALENCE (RPBUFF,RB1),(IB4,RB4),(IB5,RB5),(IB6,RB6)
C
      INTEGER NPIC,NCOL,NROW,NLAY,K,NPAR,IPTRIF,IPTRUN,J,I1,I2,N,I
      INTEGER IPTR,NOUT,NSEG,ID
      REAL VALUE
C
C Packed names
      INTEGER NPLIST,NPPLIS,NIF,NUNLES,NTO,NSEGME,NSE2,NXREF,NYREF
      INTEGER NID,NPAREN,NHOLES,NBACKG,NCONTA,NXMIN,NXMAX,NYMIN
      INTEGER NYMAX,NHFERE,NVFERE,NAFERE,NBFERE,NVPROJ,NHPROJ,NPERIM
      INTEGER NAREA,NXCEN,NYCEN,NMMIN,NMMAX,NANGLE,NCIRCU
      PARAMETER (NPLIST=26089, NPPLIS=26252, NIF=14640, NUNLES=-2173)
      PARAMETER (NTO=-601, NSEGME=30607, NSE2=30632)
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
            GOTO 140
         ENDIF
C
C Otherwise, use key value to determine picture number
      ELSE
         NPIC=IVALPN(NPLIST)
      ENDIF
C
C Open particle parameter list
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 140
C
C Fault non-plist class for particle parameter list
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 140
      ENDIF
C
C Fault bad size for particle parameter list
      IF (NROW.NE.1.OR.NLAY.LT.NPPAR) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 140
      ENDIF
C
C Save current settings of parameter variables
      DO 10 K=1,NPPAR
         IF (SEMLU(3,NAME(K),VALUE)) GOTO 140
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
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 140
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
                  IF (SEMLU(1,NAME(K),RPBUFF(K,N))) GOTO 140
   40          CONTINUE
C
C Determine logical value of 'IF' string (default is TRUE)
               IF (LIF) THEN
                  IPTR=IPTRIF
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +               GOTO 140
                  LEXPIF=VALUE.NE.0.0
               ELSE
                  LEXPIF=.TRUE.
               ENDIF
C
C Determine logical value of 'UNLESS' string (default is FALSE)
               IF (LUNLES) THEN
                  IPTR=IPTRUN
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +               GOTO 140
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
         GOTO 140
C
C Otherwise, carry on with normal processing
      ELSE
C
C If key TO is set, new particle parameter file is required
         IF (IVAL(NTO).NE.0) THEN
C
C Open output picture
            NOUT=IVALPN(NTO)
            LP3=LP1
            IF (SEMOPN(2,NOUT,NPAR,1,NPPAR,NCLPLI,NFMFP,LP3)) GOTO 140
C
C Copy each set of particle parameter values in turn
            DO 90 K=1,NPPAR
C
C Read parameter values from source picture
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 140
C
C Copy selected values to output row buffer
               DO 80 I=1,NPAR
                  RB4(I)=RB5(IB6(I))
   80          CONTINUE
C
C Store results in output picture
               IF (SEMROW(2,RB4,NFMFP,1,K,LP3)) GOTO 140
   90       CONTINUE
         ENDIF
      ENDIF
C
C If key SEGMENT is set, new segmented picture is required
      IF (VARSET(NSEGME)) THEN
C
C Fetch picture number specified by SEGMENT key
         NSEG=IVALPN(NSEGME)
C
C Open source picture
         IF (SEMOPN(1,NSEG,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 140
C
C Key SE2 specifies output picture number (default is source picture)
         IF (VARSET(NSE2)) THEN
            NOUT=IVALPN(NSE2)
         ELSE
            NOUT=NSEG
         ENDIF
C
C Open output picture
         FORM=SEMFRM(FORM)
         LP3=LP2
         IF (SEMOPN(2,NOUT,NCOL,NROW,NLAY,CLASS,FORM,LP3)) GOTO 140
C
C Initialise relabeling array
         DO 100 I=1,MAXPIX
            IB5(I)=0
  100    CONTINUE
C
C Read particle id's from particle parameter file
         IF (SEMROW(1,RB4,NFMFP,1,NIDENT,LP1)) GOTO 140
C
C Add selected particle id's to relabeling array
         DO 110 I=1,NPAR
C
C Convert pointer to particle into particle id
            ID=NINT(RB4(IB6(I)))
C
C Add id to relabeling array
            IB5(ID)=ID
  110    CONTINUE
C
C Copy source picture to output picture, relabeling pixel values
         DO 130 J=1,NROW
C
C Read data from next source row
            IF (SEMROW(1,RB4,NFMINT,J,1,LP2)) GOTO 140
C
C Relabel pixel values
            DO 120 I=1,NCOL
               IF (IB4(I).LT.1.OR.IB4(I).GT.MAXPIX) THEN
                  IB4(I)=0
               ELSE
                  IB4(I)=IB5(IB4(I))
               ENDIF
  120       CONTINUE
C
C Store results in output picture
            IF (SEMROW(2,RB4,NFMINT,J,1,LP3)) GOTO 140
  130    CONTINUE
      ENDIF
C
  140 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
