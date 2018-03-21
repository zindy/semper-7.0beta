      SUBROUTINE RGB2
C
C----------------------------------------------------------------------
C  SYNTAX:
C
C  Rg2 :RGB2 $1=sel  from=$1  $2=from  to=$2  $3=999  lut=$3  monochrome
C            $4=3  $42=3  $43=2  bit=$4  bi2=$42  bi3=$43
C            open(lp1,old)=from
C
C    (LP1=from=colour input picture,  LP2=to=output  LP3=lut=output)
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
      INTEGER IBUFL
      PARAMETER(IBUFL=LNBUF/LNINT)
C
C  Integer buffers
C
      INTEGER IB(IBUFL,5)
      EQUIVALENCE (IB(1,1), RB1(1))
C
      INTEGER IVALPN,IVAL,IPACK
      LOGICAL OPT,SEMROW,SEMOPN,SEMMED,FSLURW,SEMLUT,SETVAR
      LOGICAL RGB2BW
C
      INTEGER J,K,MEDIUM,TO,LUT,B1,B2,B3,BITS,LUTN
      LOGICAL MKLUT,MONO,TODIS
C
C  check source picture credentials
C
      IF(NLAYS(LP1) .NE. 3 .OR. FORMN(LP1) .NE. NFMBYT) THEN
         ERROR=77
         WRITE(IDMESS,10) IVALPN(IPACK('FROM'))
   10    FORMAT('Source picture ',I6,' is not a 3-layer BYTE picture')
         RETURN
      ENDIF
C
C  decode output picture 'to' and set TODIS=.TRUE. if a display picture
C
      TO=IVALPN(IPACK('TO'))
      IF(SEMMED(TO/1000,MEDIUM)) RETURN
      TODIS=MEDIUM .EQ. MEDDS
C
C  check 'convert to monochrome' option
C
      MONO=OPT(IPACK('MONO'))
C
C  set min,max to 0,lutlen-1 so that semrow outputs unscaled values
C   to display if output is COLOUR and TO DISPLAY.
C
      IF(.NOT. MONO .AND. TODIS) THEN
            IF(SETVAR(IPACK('MIN'),0.0)) RETURN
            IF(SETVAR(IPACK('MAX'),REAL(LUTLEN-1))) RETURN
      ENDIF
C
C  open output 'to' picture = LP2 (single layer, BYTE)
C
      LP2=0
      IF(SEMOPN(2,TO,NCOLS(LP1),NROWS(LP1),1,NCLIMA,NFMBYT,LP2)) RETURN
C
      IF(MONO) THEN
C
C  Convert to monochrome picture.
C     LP1 (R,G,B) --> IB(.,1) IB(.,2), IB(.,3).
C  Output IB(.,4) --> LP2,  IB(.,5) is fast lookup workspace
C
         IF(RGB2BW(LP1,LP2,IB(1,1),IB(1,4),IBUFL,IB(1,5))) RETURN
C
C  ..else convert to false colour generating associated LUT
C
      ELSE
C
C  open output LUT = LP3.
C  Note format: LUTLEN = no. of columns, 3 rows, INTEGER.
C
         LUT=IVALPN(IPACK('LUT'))
         LP3=0
         IF(SEMOPN(2,LUT,LUTLEN,3,1,NCLLUT,NFMINT,LP3)) RETURN
C
C  read value of SEMPER bit,bi2,bi3 keys
C
      B1=IVAL(IPACK('BIT'))
      B2=IVAL(IPACK('BI2'))
      B3=IVAL(IPACK('BI3'))
C
C  check assigned values do not exceed framestore limit
C
      BITS=B1+B2+B3
      IF(2**BITS .GT. LUTLEN .OR. BITS .GT. 8 .OR. B1 .LT. 1 .OR.
     +                              B2 .LT. 1 .OR. B3 .LT. 1) THEN
C
         ERROR=77
         WRITE(IDMESS,20) B1,B2,B3,LUTLEN
   20    FORMAT('BAD BIT in RG2: bits split ',3(I3,' '),' lutlen = ',I6)
         RETURN
      ENDIF
C
C  create false colour lut in LP3,
C  returning R,G,B values contiguously in IB
C
          IF(MKLUT(LUTLEN,LUTMAX,B1,B2,B3,IB(1,1),NFMINT,LP3)) RETURN
C
C  if 'to' is a display device then replace the current lut
C
         IF(TODIS) THEN
C
C  set LUTN = current lut number
C
             LUTN=IVAL(IPACK('CLUT'))
C
C  store lut in buffer in mode 2
C
            IF(FSLURW(2,LUTN,2,IB(1,1))) RETURN
C
C  copy lut to hardware as current lut
C
            IF(SEMLUT(2,LUTN,2,IB(1,1))) RETURN
C
         ENDIF
C
C  convert the colour picture to a false colour 'to' picture
C
C  first initialise fast packing routine
C
         CALL RGB2I0(B1,B2,B3,IB(1,5))
C
C  for each source row
C
         DO 40 J=1,NROWS(LP1)
C
C  read row J RED to IB1, GREEN to IB2, BLUE to IB3
C
            DO 30 K=1,3
               IF(SEMROW(1,IB(1,K),NFMINT,J,K,LP1)) RETURN
   30       CONTINUE
C
C  pack RGB values to integer representation in IB4
C
            CALL RGB2I(IB(1,1),IB(1,2),IB(1,3),
     +                               IB(1,4),NCOLS(LP1),IB(1,5))
C
C  write row to destination picture
C
            IF(SEMROW(2,IB(1,4),NFMINT,J,1,LP2)) RETURN
C
   40    CONTINUE
C
      ENDIF
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION RGB2BW(LPIN,LPOUT,IBIN,IBOUT,N1,IT)
C
C  convert to monochrome image from 3-layer colour
C
C  IBIN(.,1) is used to hold the RED rows
C  IBIN(.,2) is used to hold the GREEN rows
C  IBIN(.,3) is used to hold the BLUE rows
C  IBOUT(.)  is used to hold the MONOCHROME INTENSITY
C
C  IT is used as a fast lookup table
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMROW
      INTEGER N1,LPIN,LPOUT,IBIN(N1,4),IBOUT(N1),I,J,K,IT(0:255,3)
C
      RGB2BW=.TRUE.
C
C  initialise fast monochrome conversion table in IT
C
      DO 10 I=0,255
C
         IT(I,1)=NINT(0.299*I)
         IT(I,2)=NINT(0.587*I)
         IT(I,3)=NINT(0.114*I)
C
   10 CONTINUE
C
C  for each source row
C
      DO 40 J=1,NROWS(LPIN)
C
C  read red (K=1), green (K=2), blue (K=3) to IBIN(.,K)
C
         DO 20 K=1,3
            IF(SEMROW(1,IBIN(1,K),NFMINT,J,K,LPIN)) RETURN
   20    CONTINUE
C
C  convert to monochrome intensity in IBIN(.,4)
C
         DO 30 I=1,NCOLS(LPIN)
C
            IBOUT(I)=IT(IBIN(I,1),1)+IT(IBIN(I,2),2)+IT(IBIN(I,3),3)
C
   30    CONTINUE
C
C  write to destination picture
C
         IF(SEMROW(2,IBOUT,NFMINT,J,1,LPOUT)) RETURN
C
   40 CONTINUE
C
      RGB2BW=.FALSE.
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION MKLUT(LLEN,LMAX,NR,NG,NB,IXYZ,IFORM,LPN)
C
C  create lut
C
      INTEGER LLEN,LMAX,NR,NG,NB,IXYZ(LLEN,3),IFORM,LPN,J
      LOGICAL SEMROW
C
      MKLUT=.TRUE.
C
C  generate lut in IXYZ Red in IXYZ(.,1) Green IXYZ(.,2) Blue IXYZ(.,3)
C
      CALL MKLUT0(LLEN,LMAX,NR,NG,NB,IXYZ(1,1),IXYZ(1,2),IXYZ(1,3))
C
C  store the R,G,B values to rows 1,2,3 of the lut picture LPN
C
      DO 10 J=1,3
C
         IF(SEMROW(2,IXYZ(1,J),IFORM,J,1,LPN)) RETURN
C
   10 CONTINUE
C
      MKLUT=.FALSE.
C
      RETURN
      END
C
      SUBROUTINE MKLUT0(LLEN,LMAX,NR,NG,NB,RED,GREEN,BLUE)
C
C  generate false colour lut with 2**NR red levels, 2**NG green levels
C  2**NB blue levels.
C
      INTEGER LLEN,LMAX,RED(0:LLEN-1),GREEN(0:LLEN-1),BLUE(0:LLEN-1)
      INTEGER NR,NG,NB,I,ITEMP
      INTEGER TWOB,TWOGB,NCOLOR
      REAL SRED,SGREEN,SBLUE
C
      TWOB=2**NB
      TWOGB=2**(NG+NB)
      NCOLOR=2**(NR+NG+NB)
      SRED=REAL(LMAX)/REAL(2**NR-1)
      SGREEN=REAL(LMAX)/REAL(2**NG-1)
      SBLUE=REAL(LMAX)/REAL(TWOB-1)
C
      DO 10 I=0,NCOLOR-1
C
C  extract bits corresponding to I into RED, GREEN, BLUE
C
         RED(I)=I/TWOGB
         ITEMP=I-RED(I)*TWOGB
         GREEN(I)=ITEMP/TWOB
         BLUE(I)=ITEMP-GREEN(I)*TWOB
C
C  RED is now in the range (0,2**NR-1), RED*SRED will occupy (0,LMAX)
C  similarly scale GREEN and BLUE
C
         RED(I)=NINT(REAL(RED(I))*SRED)
         GREEN(I)=NINT(REAL(GREEN(I))*SGREEN)
         BLUE(I)=NINT(REAL(BLUE(I))*SBLUE)
C
   10 CONTINUE
C
C  clear extraneous top end of lut
C
      DO 20 I=NCOLOR,LLEN-1
         RED(I)=0
         GREEN(I)=0
         BLUE(I)=0
   20 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE RGB2I0(B1,B2,B3,IB)
C
C  initialisation routine for RGB2I. Creates look up table IB(0:255,3)
C  so that the intensity for a pixel with colour components R,G,B is
C  coded in the false colour lut as IB(R,1)+IB(G,2)+IB(B,3).
C
      INTEGER B1,B2,B3,IB(0:255,3),I
      REAL SCALE1,SCALE2,SCALE3
C
C  scale (0,255) to the range (0,2**N-1) and store result in IB
C
      SCALE1=REAL(2**B1-1)/255.0
      SCALE2=REAL(2**B2-1)/255.0
      SCALE3=REAL(2**B3-1)/255.0
C
      DO 10 I=0,255
C
         IB(I,1)=NINT(REAL(I)*SCALE1)*2**(B3+B2)
         IB(I,2)=NINT(REAL(I)*SCALE2)*2**B3
         IB(I,3)=NINT(REAL(I)*SCALE3)
C
   10 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE RGB2I(I1,I2,I3,IOUT,N,RGBVAL)
C
C  convert I1,  I2 and  I3, assumed in the range (0,255) to a
C  single integer IOUT (last B3 bits representing I3, next B2 bits
C  representing I2, next I1 bits representing I1).
C
      INTEGER N,I1(0:N-1),I2(0:N-1),I3(0:N-1),IOUT(0:N-1)
      INTEGER RGBVAL(0:255,3),I
C
      DO 10 I=0,N-1
C
         IOUT(I)=RGBVAL(I1(I),1)+RGBVAL(I2(I),2)+RGBVAL(I3(I),3)
C
   10 CONTINUE
C
      RETURN
      END
