C Semper 6 processing module PTYPE
C
      SUBROUTINE PTYPE
C
C Lists particles parameter values to the console output stream.  The
C data is obtained from a specified particle parameter file.  'IF' and
C 'UNLESS' expressions specify conditions for selecting particles.  All
C particles are selected if neither condition is specified.  The options
C XREF, YREF, ID, PARENT, HOLES, BACKGROUND, CONTACT, XMIN, XMAX, YMIN,
C YMAX, HFERET, VFERET, AFERET, BFERET, VPROJ, HPROJ, PERIMETER, AREA,
C XCEN, YCEN, MMIN, MMAX, ANGLE and CIRCULARITY specify which parameters
C are to be listed (in that order).  If more than five options are given
C the first five parameters in the above list are printed.  The SORT key
C may be used to reorder the data according to the specified parameter.
C The SORT key accepts any of the parameter options above as its
C argument.  The options ASCENDING (default) and DESCENDING specify the
C direction for sorting.  If option ALL is set, all parameters for a
C single specified particle are listed.  The key INDEX specifies which
C particle amongst those selected (and sorted).
C
      LOGICAL VARSET,OPT,SEMLU,SEMOPN,SEMROW,SEMEXP,SEMCON
      INTEGER IVAL,IVALPN
C
      INCLUDE 'COMMON'
C
      LOGICAL LSORT,LDESCE,LALL,LIF,LUNLES,LEXPIF,LEXPUN
      INTEGER CLASS,FORM
      CHARACTER*76 HEADER
      CHARACTER*54 FORMAT
C
      INTEGER NPPAR
      PARAMETER (NPPAR=25)
      REAL    PVAL(NPPAR)
      INTEGER NAME(NPPAR)
      LOGICAL ISET(NPPAR)
      CHARACTER*14 PPHEAD(NPPAR)
      CHARACTER*8  PPFORM(NPPAR)
C
      INTEGER NPBLK
      PARAMETER (NPBLK=4*(LNBUF/(NPPAR*LNREAL)))
C
      INTEGER IB6(LNBUF/LNINT)
      REAL    RBUFF(LNBUF/LNREAL,5),RPBUFF(NPPAR,NPBLK)
C
      EQUIVALENCE (RBUFF,RPBUFF,RB1),(IB6,RB6)
C
      INTEGER NPIC,NCOL,NROW,NLAY,IND,K,IPTRIF,IPTRUN
      INTEGER NPAR,J,I1,I2,N,I,IPTR
      REAL VALUE
C
C Packed names
C
      INTEGER NPLIST,NPPLIS,NIF,NUNLES,NSORT,NASCEN,NDESCE,NINDEX
      INTEGER NALL,NXREF,NYREF,NID,NPAREN,NHOLES,NBACKG,NCONTA,NXMIN
      INTEGER NXMAX,NYMIN,NYMAX,NHFERE,NVFERE,NAFERE,NBFERE,NCIRCU
      INTEGER NVPROJ,NHPROJ,NPERIM,NAREA,NXCEN,NYCEN,NMMIN,NMMAX,NANGLE
      PARAMETER (NPLIST=26089, NPPLIS=26252, NIF=14640, NUNLES=-2173)
      PARAMETER (NSORT=31018, NASCEN=2363, NDESCE=6619)
      PARAMETER (NINDEX=14964, NALL=2092)
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
      DATA PPHEAD / '  X reference ','  Y reference ',
     +              '  Particle id ','   Parent id  ',
     +              '     Holes    ','  Background  ',
     +              ' Edge contact ','   X minimum  ',
     +              '   X maximum  ','   Y minimum  ',
     +              '   Y maximum  ','  Horiz feret ',
     +              '  Vert feret  ',' 45 deg feret ',
     +              ' 135 deg feret',' Vert project ',
     +              ' Horiz project','   Perimeter  ',
     +              '     Area     ','   X centre   ',
     +              '   Y centre   ',' Min 2nd mom. ',
     +              ' Max 2nd mom. ',' Angle (degs.)',
     +              '  Circularity ' /
C
      DATA PPFORM / 'F12.1,2X' , 'F12.1,2X' , 'F10.0,4X',
     +              'F10.0,4X' , 'F12.0,2X' , 'F10.0,4X',
     +              'F10.0,4X' , 'F12.1,2X' , 'F12.1,2X',
     +              'F12.1,2X' , 'F12.1,2X' , 'F12.1,2X',
     +              'F12.1,2X' , 'F12.1,2X' , 'F12.1,2X',
     +              'F12.1,2X' , 'F12.1,2X' , 'F12.3,2X',
     +              'F12.1,2X' , 'F12.1,2X' , 'F12.1,2X',
     +              'E13.6,1X' , 'E13.6,1X' , 'F12.3,2X',
     +              'F11.5,3X' /
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
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 130
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
      IF (NROW.NE.1.OR.NLAY.LT.NPPAR) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 130
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
            GOTO 130
         ENDIF
      ENDIF
C
C See if option ALL is set
C
      LALL=OPT(NALL)
C
C Flag parameters to list and make setting of parameter variables local
C
      DO 10 K=1,NPPAR
         ISET(K)=VARSET(NAME(K))
         IF (SEMLU(3,NAME(K),VALUE)) GOTO 130
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
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 130
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
                  IF (SEMLU(1,NAME(K),RPBUFF(K,N))) GOTO 130
   40          CONTINUE
C
C Determine logical value of 'IF' string (default is TRUE)
C
               IF (LIF) THEN
                  IPTR=IPTRIF
                  IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.))
     +            GOTO 130
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
     +               GOTO 130
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
         GOTO 130
C
C Otherwise, carry on with normal processing
C
      ELSE
C
C If option ALL is set, pick up value of INDEX key
C
         IF (LALL) THEN
C
C Fetch value for INDEX key
C
            IND=IVAL(NINDEX)
C
C Fault bad value for INDEX key
C
            IF (IND.LT.1.OR.IND.GT.NPAR) THEN
              ERROR=3
              IDERR=NINDEX
              GOTO 130
            ENDIF
         ENDIF
C
C If SORT key is set, it specifies a particle parameter to sort on
C
         IF (LSORT) THEN
C
C Set parameter variables to values 1 to n
C
            DO 80 K=1,NPPAR
               IF (SEMLU(1,NAME(K),REAL(K))) GOTO 130
   80       CONTINUE
C
C Fetch pointer to 'SORT' string
C
            IPTR=IVAL(NSORT)
C
C Determine value of 'SORT' string
C
            IF (SEMEXP(LINBUF,LINLEN,IPTR,VALUE,.FALSE.)) GOTO 130
C
C Fault bad value for 'SORT' string
C
            IF (VALUE.LT.REAL(1).OR.VALUE.GT.REAL(NPPAR)) THEN
               ERROR=3
               IDERR=NSORT
               GOTO 130
            ENDIF
C
C 'SORT' string value selects particle parameter to sort on
C
            K=NINT(VALUE)
C
C Read selected particle parameter values
C
            IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 130
C
C Sort list of selected particles according to selected parameter
C
            CALL PSORT(RB5,IB6,NPAR,LDESCE)
         ENDIF
C
C If option ALL is set, list all parameters for particle specified by
C INDEX key (if within range)
C
         IF (LALL) THEN
C
C Read all particle parameters
C
            DO 90 K=1,NPPAR
C
C Read particle parameter values
C
               IF (SEMROW(1,RB5,NFMFP,1,K,LP1)) GOTO 130
C
C Store specified particle parameter value
C
               PVAL(K)=RB5(IB6(IND))
C
C If parameter is orientation angle, convert angle from radians
C to degrees
C
               IF (NAME(K).EQ.NANGLE) PVAL(K)=180.0*PVAL(K)/PI
   90       CONTINUE
C
C Print particle parameters
C
            IF (SEMCON(' ')) GOTO 130
            IF (SEMCON('Particle parameters:')) GOTO 130
            WRITE (RECORD,140) PVAL(1),PVAL(2)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,150) PVAL(3)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,160) PVAL(4)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,170) PVAL(5)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,180) PVAL(6)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,190) PVAL(7)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,200) PVAL(8),PVAL(9),PVAL(10),PVAL(11)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,210) PVAL(12),PVAL(13),PVAL(14),PVAL(15)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,220) PVAL(16),PVAL(17)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,230) PVAL(18)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,240) PVAL(19)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,250) PVAL(20),PVAL(21)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,260) PVAL(22),PVAL(23)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,270) PVAL(24)
            IF (SEMCON(RECORD)) GOTO 130
            WRITE (RECORD,280) PVAL(25)
            IF (SEMCON(RECORD)) GOTO 130
C
C Otherwise, list specified parameters (up to maximum of 5)
C
         ELSE
C
C Initialise header and format strings
C
            HEADER='Index'
            FORMAT=
     +       '(I4,2X,   1X   ,   1X   ,   1X   ,   1X   ,   1X   )'
C
C Read selected particle parameters
C
            N=0
            DO 110 K=1,NPPAR
C
C If corresponding option was set, read particle parameter values
C (up to maximum of five) and add to header and format strings
C
               IF (ISET(K)) THEN
                  IF (N.LT.5) THEN
                     N=N+1
C
C Read particle parameter values
C
                     IF (SEMROW(1,RBUFF(1,N),NFMFP,1,K,LP1))
     +                  GOTO 130
C
C If parameter is orientation angle, convert angle from radians
C to degrees
C
                     IF (NAME(K).EQ.NANGLE) THEN
                        DO 100 I=1,NCOL
                           RBUFF(I,N)=180.0*RBUFF(I,N)/PI
  100                   CONTINUE
                     ENDIF
C
C Add parameter title string to header string
C
                     I = N*14 - 7
                     HEADER(I:I+13)=PPHEAD(K)
C
C Add parameter format specifier to format string
C
                     I = N*9 - 1
                     FORMAT(I:I+7)=PPFORM(K)
                  ENDIF
               ENDIF
  110       CONTINUE
C
C Print header string
C
            IF (SEMCON(' ')) GOTO 130
            IF (SEMCON(HEADER)) GOTO 130
C
C Print selected (and sorted) parameter values
C
            DO 120 I=1,NPAR
               WRITE (RECORD,FORMAT) I,(RBUFF(IB6(I),J),J=1,N)
               IF (SEMCON(RECORD)) GOTO 130
  120       CONTINUE
         ENDIF
      ENDIF
C
  130 RETURN
C
  140 FORMAT ('Reference point   ',2F15.2)
  150 FORMAT ('Particle id       ',F15.0)
  160 FORMAT ('Parent id         ',F15.0)
  170 FORMAT ('Number of holes   ',F15.0)
  180 FORMAT ('Background flag   ',F15.0)
  190 FORMAT ('Edge contact flag ',F15.0)
  200 FORMAT ('Particle limits   ',4F15.2)
  210 FORMAT ('Feret diameters   ',4F15.2)
  220 FORMAT ('H,V projections   ',2F15.2)
  230 FORMAT ('Perimeter         ',F15.2)
  240 FORMAT ('Area              ',F15.2)
  250 FORMAT ('Centre of area    ',2F15.2)
  260 FORMAT ('Min,max 2nd moment',2E15.6)
  270 FORMAT ('Angle (degrees)   ',F15.2)
  280 FORMAT ('Circularity       ',F15.5)
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
