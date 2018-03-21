C Semper 6 processing module RGB
C
      SUBROUTINE RGB
C
C  Display full colour byte picture with monochrome/false colour LUT
C
C  Syntax:  Rgb :RGB $1=sel from=$1 $2=dis to=$2 lut=999 +
C                    monochrome false sample average levels= le2= le3= +
C                    open(lp1,old)=from
C
C  For FALSE, ALL or NONE of LEVELS,LE2,LE3 must be supplied.
C  IF none is supplied then suitable defaults are used:
C             LEVELS=8,8,4  for 8-bit capacity framestores.
C             LEVELS=4,8,4  for 7-bit capacity
C
C
      LOGICAL OPT
C
      LOGICAL LMONOC,LFALSE,LSAMPL,LAVERA,LOPT(4)
      INTEGER I,J,NOPT(4)
C
      EQUIVALENCE (LMONOC,LOPT(1)),(LFALSE,LOPT(2))
      EQUIVALENCE (LSAMPL,LOPT(3)),(LAVERA,LOPT(4))
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NMONOC,NFALSE,NSAMPL,NAVERA
      PARAMETER (NMONOC=21414, NFALSE=9652, NSAMPL=2485, NAVERA=30453)
C
      DATA NOPT / NMONOC,NFALSE,NSAMPL,NAVERA /
C
C Fault source picture that does not have three layers
C
      IF (NLAYS(LP1).NE.3) THEN
         ERROR=5
         IDERR=1000*DEVN(LP1)+PICN(LP1)
         GOTO 40
      ENDIF
C
C Fault source picture that is not byte form
C
      IF (FORMN(LP1).NE.NFMBYT) THEN
         ERROR=43
         IDERR=1000*DEVN(LP1)+PICN(LP1)
         GOTO 40
      ENDIF
C
C See if options MONOCHROME, FALSE, SAMPLED and AVERAGED are set
C
      DO 10 I=1,4
         LOPT(I)=OPT(NOPT(I))
   10 CONTINUE
C
C Fault conflict between any of these options
C
      DO 30 I=1,3
         DO 20 J=I+1,4
            IF (LOPT(I).AND.LOPT(J)) THEN
               ERROR=60
               IDERR=NOPT(I)
               IDERR2=NOPT(J)
               GOTO 40
            ENDIF
   20    CONTINUE
   30 CONTINUE
C
C Call appropriate processing routine
C
      IF (LMONOC.OR.LFALSE) THEN
         CALL SEMRGB(LMONOC)
      ELSE IF (LSAMPL.OR.LAVERA) THEN
         CALL SEMFAL(LAVERA)
      ELSE
         CALL SEMRGB(.TRUE.)
      ENDIF
C
   40 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module SEMRGB
C
      SUBROUTINE SEMRGB(MONO)
C
C  Read 3-layer byte full colour image and produce false colour lut
C  with rgb,rg2,rg3 brightness levels for red,green,blue, and encode
C  the source picture in this form. If the destination is display then
C  the lut is not output but is made the current lut instead. If mono is
C  set then an apparent-brightness-weighted monochrome picture is
C  produced instead, and lut is ignored.
C
C    (LP1=from=colour input picture,  LP2=to=output  LP3=lut=output)
C
      INTEGER IVALPN,IVAL
      LOGICAL SEMROW,SEMOPN,SEMMED,FSLURW,SEMLUT,VARSET
      LOGICAL RGBMON,RGBLUT
C
      REAL    TMAX,TMIN,TEMP
      INTEGER J,K,MEDIUM,NPIC,NR,NG,NB,FORM,NCOL,NROW
      LOGICAL MONO,TODIS,INCR
C
      INCLUDE 'COMMON'
C
      INTEGER IB(LNBUF/LNINT,5)
      EQUIVALENCE (IB,RB1)
C
C Packed names
C
      INTEGER NLEVEL,NLE2,NLE3,NTO,NLUT,LNBLNI
      PARAMETER (NLEVEL=19422, NLE2=19432, NLE3=19433)
      PARAMETER (NTO=-601, NLUT=20060)
C
      IF (.NOT.MONO) THEN
C
C  see if default values of RGB required
C
         IF (VARSET(NLEVEL).AND.VARSET(NLE2).AND.VARSET(NLE3)) THEN
C
C Read value of RGB keys
C
            NR=IVAL(NLEVEL)
            NG=IVAL(NLE2)
            NB=IVAL(NLE3)
C
C Fault bad value for RGB keys
C
            IF (NR.LT.2.OR.NG.LT.2.OR.NB.LT.2.OR.
     +          REAL(NR)*REAL(NG)*REAL(NB).GT.REAL(LUTLEN)) THEN
               ERROR=3
               IDERR=NLEVEL
               GOTO 40
            ENDIF
         ELSE
C
C Establish default values
C
            IF (LUTLEN.EQ.256) THEN
               NR=8
               NG=8
               NB=4
            ELSE IF (LUTLEN.EQ.128) THEN
               NR=4
               NG=8
               NB=4
            ELSE
               TEMP=REAL(2*LUTLEN)**(1.0/3.0)
               NR=MAX(1,INT(TEMP))
               NG=MAX(1,INT(TEMP))
               NB=MAX(1,INT(TEMP/2.0))
C
   10          INCR=.FALSE.
C
               IF(NR*(NG+1)*NB.LE.LUTLEN) THEN
                  NG=NG+1
                  INCR=.TRUE.
               ENDIF
C
               IF((NR+1)*NG*NB.LE.LUTLEN) THEN
                  NR=NR+1
                  INCR=.TRUE.
               ENDIF
C
               IF(NR*NG*(NB+1).LE.LUTLEN) THEN
                  NB=NB+1
                  INCR=.TRUE.
               ENDIF
C
               IF (INCR) GOTO 10
            ENDIF
         ENDIF
      ENDIF
C
C See if output picture is display picture
C
      NPIC=IVALPN(NTO)
      IF (SEMMED(NPIC/1000,MEDIUM)) GOTO 40
      TODIS=MEDIUM.EQ.MEDDS
C
C Set up output scaling - monochrome = 0 -> 255
C                         false      = 0 -> lutlen-1
C
      IF (TODIS) THEN
         TMIN=VMIN
         TMAX=VMAX
C
         IF (MONO) THEN
            VMIN=0.0
            VMAX=255.0
         ELSE
            VMIN=0.0
            VMAX=REAL(LUTLEN-1)
         ENDIF
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Output picture form is integer if number of output colours > 256
C
      FORM=NFMBYT
      IF (.NOT.MONO) THEN
         IF (NR*NG*NB.GT.256) FORM=NFMINT
      ENDIF
C
C Open output picture
C
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,FORM,LP2)) GOTO 40
C
      IF (MONO) THEN
C
C  convert to monochrome picture
C         IB(.,1..3) is workspace for 3 source layers R,G,B of LP1
C         IB(.,4) is workspace intensity calculated from IB1..3
C         IB(.,5) is used as a fast lookup table of size (256*3)
C
         LNBLNI=LNBUF/LNINT
         IF (RGBMON(LP1,LP2,IB(1,1),LNBLNI,IB(1,5))) GOTO 40
C
C  ..else convert to false colour generating associated LUT
C
      ELSE
C
C  if destination is disc
C
         IF (.NOT.TODIS) THEN
C
C Open output picture for look-up table data
C
            NPIC=IVALPN(NLUT)
            LP3=0
            IF (SEMOPN(2,NPIC,LUTLEN,3,1,NCLLUT,NFMINT,LP3)) GOTO 40
         ENDIF
C
C  create false colour lut in LP3 if not to display,
C  return values contiguously in IB in any case
C
         IF (RGBLUT(TODIS,LUTLEN,LUTMAX,NR,NG,NB,IB(1,1),NFMINT,LP3))
     +      GOTO 40
C
C  if 'to' is a display device then replace the current lut
C
         IF (TODIS) THEN
            IF(FSLURW(2,NINT(CLUT),2,IB(1,1))) GOTO 40
            IF(SEMLUT(2,NINT(CLUT),2,IB(1,1))) GOTO 40
         ENDIF
C
C  convert the colour picture to a false colour 'to' picture
C
C  initialise packing routine:
C  the packing look up values are placed in IB5
C
         CALL RGBIN0(NR,NG,NB,IB(1,5))
C
C  for each source row
C
         DO 30 J=1,NROW
C
C Read red, green and blue source rows
C
            DO 20 K=1,3
               IF (SEMROW(1,IB(1,K),NFMINT,J,K,LP1)) GOTO 40
   20       CONTINUE
C
C Pack RGB values to integer representation in IB4 using table in IB5
C
            CALL RGBINT(IB(1,1),IB(1,2),IB(1,3),
     +                  IB(1,4),NCOL,IB(1,5))
C
C  write row to destination picture
C
            IF (SEMROW(2,IB(1,4),NFMINT,J,1,LP2)) GOTO 40
C
   30    CONTINUE
      ENDIF
C
C Restore variables MIN and MAX
C
      IF (TODIS) THEN
         VMIN=TMIN
         VMAX=TMAX
      ENDIF
C
   40 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module RGBMON
C
      LOGICAL FUNCTION RGBMON(LPIN,LPOUT,IB,N1,IT)
C
C  convert to monochrome image from 3-layer colour
C
C  IB(.,1) is used to hold the RED rows
C  IB(.,2) is used to hold the GREEN rows
C  IB(.,3) is used to hold the BLUE rows
C  IB(.,4) is used to hold the MONOCHROME INTENSITY
C
C  IT is used as a fast lookup table
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMROW
      INTEGER N1,LPIN,LPOUT,IB(N1,4),I,J,K,IT(0:255,3)
C
      RGBMON=.TRUE.
C
C  initialise fast monochrome conversion table in IT
C
      DO 10 I=0,255
         IT(I,1)=NINT(0.299*REAL(I))
         IT(I,2)=NINT(0.587*REAL(I))
         IT(I,3)=NINT(0.114*REAL(I))
   10 CONTINUE
C
C  for each source row
C
      DO 40 J=1,NROWS(LPIN)
C
C  read red (K=1), green (K=2), blue (K=3) to IB(.,K)
C
         DO 20 K=1,3
            IF(SEMROW(1,IB(1,K),NFMINT,J,K,LPIN)) GOTO 50
   20    CONTINUE
C
C  convert Red=IB1, Green=IB2, Blue=IB3 to monochrome intensity in IB4
C
         DO 30 I=1,NCOLS(LPIN)
            IB(I,4)=IT(IB(I,1),1)+IT(IB(I,2),2)+IT(IB(I,3),3)
   30    CONTINUE
C
C  write to destination picture
C
         IF(SEMROW(2,IB(1,4),NFMINT,J,1,LPOUT)) GOTO 50
   40 CONTINUE
C
      RGBMON=.FALSE.
C
   50 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module RGBLUT
C
      LOGICAL FUNCTION RGBLUT(TODIS,LLEN,LMAX,NR,NG,NB,IXYZ,IFORM,LPN)
C
C  create lut. If .NOT. TODIS then also save the lut in picture LPN
C
      INTEGER LLEN,LMAX,NR,NG,NB,IXYZ(LLEN,3),IFORM,LPN,J
      LOGICAL SEMROW,TODIS
C
      RGBLUT=.TRUE.
C
C  generate lut in IXYZ Red in IXYZ(.,1) Green IXYZ(.,2) Blue IXYZ(.,3)
C
      CALL RGBLU0(LLEN,LMAX,NR,NG,NB,IXYZ(1,1),IXYZ(1,2),IXYZ(1,3))
C
      IF(.NOT. TODIS) THEN
C
C  store the R,G,B values to rows 1,2,3 of the lut picture LPN
C
         DO 10 J=1,3
            IF(SEMROW(2,IXYZ(1,J),IFORM,J,1,LPN)) GOTO 20
   10    CONTINUE
      ENDIF
C
      RGBLUT=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module RGBLU0
C
      SUBROUTINE RGBLU0(LLEN,LMAX,NR,NG,NB,RED,GREEN,BLUE)
C
C  generate false colour lut with NR red levels, NG green levels , NB
C  blue.
C  The arrays RED,GREEN,BLUE(0:LLEN-1) contain brightnes values 0..LMAX
C
      INTEGER LLEN,LMAX,RED(0:LLEN-1),GREEN(0:LLEN-1),BLUE(0:LLEN-1)
      INTEGER NR,NG,NB,NGB,NRGB,I,ITEMP
      REAL SRED,SGREEN,SBLUE
C
      NGB=NG*NB
      NRGB=NR*NG*NB
      SRED=REAL(LMAX)/REAL(NR-1)
      SGREEN=REAL(LMAX)/REAL(NG-1)
      SBLUE=REAL(LMAX)/REAL(NB-1)
C
C make grey ramp (as I) and extract R,G,B values for LUT
C
      DO 10 I=0,NRGB-1
C
C  The bottom LOG (NB) bits are returned as BLUE levels
C                2
C  The next NG levels are returned as GREEN
C  The next NR levels are returned as RED... the code all makes perfect
C  sense if you assume NR,NG,NB are powers of two (but works even if
C  they are not ! ... see FORTRAN PROGRAMMERS GUIDE command RG1,RG2)
C
         RED(I)=I/NGB
         ITEMP=I-RED(I)*NGB
         GREEN(I)=ITEMP/NB
         BLUE(I)=ITEMP-GREEN(I)*NB
C
C  RED is now in the range (0,NR-1), RED*SRED will occupy (0,LMAX)
C  similaily scale GREEN and BLUE
C
         RED(I)=NINT(REAL(RED(I))*SRED)
         GREEN(I)=NINT(REAL(GREEN(I))*SGREEN)
         BLUE(I)=NINT(REAL(BLUE(I))*SBLUE)
   10 CONTINUE
C
C  clear extraneous top end of lut
C
      DO 20 I=NRGB,LLEN-1
         RED(I)=0
         GREEN(I)=0
         BLUE(I)=0
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module RGBIN0
C
      SUBROUTINE RGBIN0(B1,B2,B3,IB)
C
C  initialisation routine for RGBINT. Creates look up table IB(0:255,3)
C  so that the intensity for a pixel with colour components R,G,B is
C  coded in the false colour lut as IB(R,1)+IB(G,2)+IB(B,3).
C
      INTEGER B1,B2,B3,IB(0:255,3),I
      REAL SCALE1,SCALE2,SCALE3
C
C  scale (0,255) to the range (0,B-1) and store result in IB
C
      SCALE1=REAL(B1-1)/255.0
      SCALE2=REAL(B2-1)/255.0
      SCALE3=REAL(B3-1)/255.0
C
      DO 10 I=0,255
         IB(I,1)=NINT(REAL(I)*SCALE1)*B3*B2
         IB(I,2)=NINT(REAL(I)*SCALE2)*B3
         IB(I,3)=NINT(REAL(I)*SCALE3)
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module RGBINT
C
      SUBROUTINE RGBINT(I1,I2,I3,IOUT,N,RGBVAL)
C
C  convert I1,  I2 and  I3, assumed in the range (0,255) to a
C  single integer IOUT using B1 levels for I1, B2 for I2, B3 for I3
C  The look up table RGBVAL created by RGBIN0 is required.
C
      INTEGER N,I1(N),I2(N),I3(N),IOUT(N)
      INTEGER RGBVAL(0:255,3),I
C
      DO 10 I=1,N
         IOUT(I)=RGBVAL(I1(I),1)+RGBVAL(I2(I),2)+RGBVAL(I3(I),3)
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module SEMFAL
C
      SUBROUTINE SEMFAL(LAVERA)
C
C  Read 3-layer byte full colour image and produce a false colour lut
C  with 256 brightness levels for red,green,blue, and encode
C  the source picture in this form. If the destination is display then
C  the lut is not output but is made the current lut instead. If average
C  is set then the output image is dithered.
C
C    (LP1=from=colour input picture,  LP2=to=output  LP3=lut=output)
C
C
      LOGICAL FSLURW,SEMLUT,SEMOPN,SEMROW,SEMMED
      INTEGER IVALPN
C
      REAL    GREY,DGREY,TMIN,TMAX
      INTEGER I,J,NPIC,NCOL,NROW,LUTBY3,CURLUT,MODE,MEDIUM,FORM
      LOGICAL LAVERA,TODIS
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT)
      INTEGER IB2(LNBUF/LNINT)
      INTEGER IB3(LNBUF/LNINT)
      INTEGER IB4(0:LNBUF/LNINT)
      INTEGER LUT(3*LUTSIZ)
      EQUIVALENCE (IB1,RB1),(IB2,RB2),(IB3,RB3),(LUT,IB4,RB4)
C
C Packed names
C
      INTEGER NTO,NLUT
      PARAMETER (NTO=-601, NLUT=20060)
C
C See if output picture is display picture
C
      NPIC=IVALPN(NTO)
      IF (SEMMED(NPIC/1000,MEDIUM)) GOTO 120
      TODIS=MEDIUM.EQ.MEDDS
C
C  set min,max to 0,lutlen-1 so that semrow outputs unscaled values
C  to display if output is to display.
C
      IF (TODIS) THEN
         TMIN=VMIN
         TMAX=VMAX
         VMIN=0.0
         VMAX=REAL(LUTLEN-1)
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Open output picture = LP2 (single layer, BYTE or INTEGER)
C
      FORM=NFMBYT
      IF (LUTLEN.GT.256) FORM=NFMINT
C
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,FORM,LP2)) GOTO 120
C
C Generate look-up table data
C
      LUTBY3=LUTLEN/3
C
      DGREY=REAL(LUTMAX)/REAL(LUTBY3-1)
C
      DO 30 J=1,3
         DO 10 I=1+(J-1)*LUTLEN,J*LUTLEN
            LUT(I)=0
   10    CONTINUE
C
         GREY=0.0
         DO 20 I=1+(J-1)*LUTLEN+(J-1)*LUTBY3,(J-1)*LUTLEN+J*LUTBY3
            LUT(I)=NINT(GREY)
            GREY=GREY+DGREY
   20    CONTINUE
   30 CONTINUE
C
C If output is to display
C
      IF (TODIS) THEN
C
C  replace current viewing lut
C
         CURLUT=NINT(CLUT)
         MODE=2
         IF (FSLURW(2,CURLUT,MODE,LUT)) GOTO 120
         IF (SEMLUT(2,CURLUT,MODE,LUT)) GOTO 120
      ELSE
C
C Open output picture for look-up table data
C
         NPIC=IVALPN(NLUT)
         LP3=0
         IF(SEMOPN(2,NPIC,LUTLEN,3,1,NCLLUT,NFMINT,LP3)) GOTO 120
C
C  write the 3 rows Red, Green, Blue to the lut picture LP3
C
         IF (SEMROW(2,LUT,            NFMINT,1,1,LP3)) GOTO 120
         IF (SEMROW(2,LUT(LUTLEN+1),  NFMINT,2,1,LP3)) GOTO 120
         IF (SEMROW(2,LUT(1+2*LUTLEN),NFMINT,3,1,LP3)) GOTO 120
      ENDIF
C
C Construct data conversion table
C
      GREY=0.0
      DGREY=REAL(3*LUTBY3-1)/255.0
      DO 40 I=0,255
         IB4(I)=NINT(GREY)
         GREY=GREY+DGREY
   40 CONTINUE
C
C Process the source picture
C
      DO 110 J=1,NROW
         IF (SEMROW(1,IB1,NFMINT,J,1,LP1)) GOTO 120
C
         IF (LAVERA) THEN
            IF (SEMROW(1,IB2,NFMINT,MIN(J+1,NROW),1,LP1)) GOTO 120
            IB1(NCOL+1)=IB1(NCOL)
            IB2(NCOL+1)=IB2(NCOL)
C
            DO 50 I=1+MOD(J,3),NCOL,3
               IB3(I)=(IB4(IB1(I))+IB4(IB1(I+1))+IB4(IB2(I)))/9
   50       CONTINUE
         ELSE
            DO 60 I=1+MOD(J,3),NCOL,3
               IB3(I)=IB4(IB1(I))/3
   60       CONTINUE
         ENDIF
C
         IF (SEMROW(1,IB1,NFMINT,J,2,LP1)) GOTO 120
C
         IF (LAVERA) THEN
            IF (SEMROW(1,IB2,NFMINT,MIN(J+1,NROW),2,LP1)) GOTO 120
            IB1(NCOL+1)=IB1(NCOL)
            IB2(NCOL+1)=IB2(NCOL)
C
            DO 70 I=1+MOD(J+1,3),NCOL,3
               IB3(I)=LUTBY3+
     +                (IB4(IB1(I))+IB4(IB1(I+1))+IB4(IB2(I)))/9
   70       CONTINUE
         ELSE
            DO 80 I=1+MOD(J+1,3),NCOL,3
               IB3(I)=LUTBY3+IB4(IB1(I))/3
   80       CONTINUE
         ENDIF
C
         IF (SEMROW(1,IB1,NFMINT,J,3,LP1)) GOTO 120
C
         IF (LAVERA) THEN
            IF (SEMROW(1,IB2,NFMINT,MIN(J+1,NROW),3,LP1)) GOTO 120
            IB1(NCOL+1)=IB1(NCOL)
            IB2(NCOL+1)=IB2(NCOL)
C
            DO 90 I=1+MOD(J+2,3),NCOL,3
               IB3(I)=LUTBY3+LUTBY3+
     +                (IB4(IB1(I))+IB4(IB1(I+1))+IB4(IB2(I)))/9
   90       CONTINUE
         ELSE
            DO 100 I=1+MOD(J+2,3),NCOL,3
               IB3(I)=LUTBY3+LUTBY3+IB4(IB1(I))/3
  100       CONTINUE
         ENDIF
C
         IF (SEMROW(2,IB3,NFMINT,J,1,LP2)) GOTO 120
  110 CONTINUE
C
C Restore variables MIN and MAX
C
      IF (TODIS) THEN
         VMIN=TMIN
         VMAX=TMAX
      ENDIF
C
  120 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
