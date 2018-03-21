      SUBROUTINE RGB1
C
C----------------------------------------------------------------------
C  SYNTAX:
C
C  Rg1 :RGB1 $1=sel  from=$1  $2=from  to=$2  $3=999  lut=$3  monochrome
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
C  Contiguous integer buffers
C
      INTEGER IB(IBUFL,4)
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
C  set min,max to 0,lutlen-1 so that semrow outputs unscaled values to
C  display if output is COLOUR and TO DISPLAY. Note that the Scaling
C  applied by SEMROW is determined by the values of max,min at the time
C  the picture is OPENED.
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
C  convert to monochrome picture (output to IB(.,4))
C
         IF(RGB2BW(LP1,LP2,IB(1,1),IB(1,4),IBUFL)) RETURN
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
   20    FORMAT('BAD BIT in RG1: bits split ',3(I3,' '),' lutlen = ',I6)
         RETURN
      ENDIF
C
C  create false colour lut in LP3, returning R,G,B values
C  contiguously in IB
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
            CALL RGB2I(IB(1,1),IB(1,2),IB(1,3),B1,B2,B3,
     +                                        IB(1,4),NCOLS(LP1))
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
      LOGICAL FUNCTION RGB2BW(LPIN,LPOUT,IBIN,IBOUT,N1)
C
C  convert to monochrome image from 3-layer colour: for row J
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMROW
      INTEGER N1,LPIN,LPOUT,IBIN(N1,3),IBOUT(N1),J,K
C
      RGB2BW=.TRUE.
C
C  for each source row
C
      DO 20 J=1,NROWS(LPIN)
C
C  read red (K=1), green (K=2), blue (K=3) to IBIN(.,K)
C
         DO 10 K=1,3
            IF(SEMROW(1,IBIN(1,K),NFMINT,J,K,LPIN)) RETURN
   10    CONTINUE
C
C  convert to monochrome intensity
C
         CALL RGBTOY(IBIN(1,1),IBIN(1,2),IBIN(1,3),IBOUT,NCOLS(LPIN))
C
C  write to destination picture
C
         IF(SEMROW(2,IBOUT,NFMINT,J,1,LPOUT)) RETURN
C
   20 CONTINUE
C
      RGB2BW=.FALSE.
C
      RETURN
      END
C
C
      LOGICAL FUNCTION MKLUT(LLEN,LMAX,NR,NG,NB,IXYZ,IFORM,LPN)
C
C  create false colour lut, NR red bits, NG green, NB blue.
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
      INTEGER NR,NG,NB,I,NCOLOR
      REAL SRED,SGREEN,SBLUE
C
      NCOLOR=2**(NR+NG+NB)
      SRED=REAL(LMAX)/REAL(2**NR-1)
      SGREEN=REAL(LMAX)/REAL(2**NG-1)
      SBLUE=REAL(LMAX)/REAL(2**NB-1)
C
C  make grey ramp (temporarily) in RED
C
      DO 10 I=0,LLEN-1
         RED(I)=I
   10 CONTINUE
C
C  unpack these integers into Red Green Blue components
C
      CALL UNPAKB(RED,NR,NG,NB,LLEN,RED,GREEN,BLUE)
C
C  RED is now in the range (0,2**NR-1), RED*SRED will occupy (0,LMAX)
C  similarly scale GREEN and BLUE
C
      DO 20 I=0,NCOLOR-1
C
         RED(I)=NINT(REAL(RED(I))*SRED)
         GREEN(I)=NINT(REAL(GREEN(I))*SGREEN)
         BLUE(I)=NINT(REAL(BLUE(I))*SBLUE)
C
   20 CONTINUE
C
C  clear extraneous top end of lut
C
      DO 30 I=NCOLOR,LLEN-1
         RED(I)=0
         GREEN(I)=0
         BLUE(I)=0
   30 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE RGB2I(I1,I2,I3,B1,B2,B3,IOUT,N)
C
C  convert I1,  I2 and  I3, assumed in the range (0,255) to a
C  single integer IOUT (last B3 bits representing I3, next B2 bits
C  representing I2, next I1 bits representing I1).
C
      INTEGER N,I1(N),I2(N),I3(N),IOUT(N),B1,B2,B3,I
      REAL SCALE1,SCALE2,SCALE3
C
C  scale I1,I2,I3 from the range (0,255) to (0,2**N-1)
C
      SCALE1=REAL(2**B1-1)/255.0
      SCALE2=REAL(2**B2-1)/255.0
      SCALE3=REAL(2**B3-1)/255.0
C
      DO 10 I=1,N
C
         I1(I)=NINT(REAL(I1(I))*SCALE1)
         I2(I)=NINT(REAL(I2(I))*SCALE2)
         I3(I)=NINT(REAL(I3(I))*SCALE3)
C
   10 CONTINUE
C
C  convert resulting values to single integer representation in IB4
C
      CALL BITPAK(I1,I2,I3,N,B2,B3,IOUT)
C
      RETURN
      END
C
C
      SUBROUTINE RGBTOY(RIN,GIN,BIN,YOUT,N)
C
C  given R,G,B input calculate monochrome intensity.
C
      INTEGER I,N,RIN(N),GIN(N),BIN(N),YOUT(N)
C
      DO 10 I=1,N
C
         YOUT(I)=NINT(0.299*RIN(I)+0.587*GIN(I)+0.114*BIN(I))
C
   10 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE BITPAK(IX,IY,IZ,N,NBY,NBZ,IOUT)
C
C  packs the integers IX,IY,IZ to IOUT with:
C
C  IZ packed into the bottom NBZ bits
C  IY placed in the next NBY bits
C  IX placed in the top bits
C
C
C  NOTE: the values of IX,IY,IZ must not exceed the number of bits
C        allocated. IOUT may be one of IX,IY,IZ.
C
      INTEGER N,IX(N),IY(N),IZ(N),NBY,NBZ,IOUT(N),I,TWOZ,TWOY,ITEMP
C
      TWOZ=2**NBZ
      TWOY=2**NBY
C
      DO 10 I=1,N
         ITEMP=IZ(I)+TWOZ*(IY(I)+TWOY*IX(I))
         IOUT(I)=ITEMP
   10 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE UNPAKB(IXYZ,NBX,NBY,NBZ,N,IX,IY,IZ)
C
C  Given IXYZ = IX,IY,IZ in packed form, return IX IY and IZ
C
C  The bottom NBZ bits are returned as IZ
C  The next NBY bits are returned as IY
C  The next NBX bits are returned as IX
C
C  Note: IXYZ may be one of IX,IY,IZ in which case it is overwritten
C
      INTEGER N,NBX,NBY,NBZ,IXYZ(N),IX(N),IY(N),IZ(N),I,ITEMP
      INTEGER TWOZ,TWOYZ,TWOXYZ
C
      TWOZ=2**NBZ
      TWOYZ=2**(NBY+NBZ)
      TWOXYZ=2**(NBX+NBY+NBZ)
C
      DO 10 I=1,N
C
C  clear any extraneous top order bits
C
         ITEMP=MOD(IXYZ(I),TWOXYZ)
C
C  extract bits
C
         IX(I)=ITEMP/TWOYZ
         ITEMP=ITEMP-IX(I)*TWOYZ
         IY(I)=ITEMP/TWOZ
         IZ(I)=ITEMP-IY(I)*TWOZ
C
   10 CONTINUE
C
      RETURN
      END
