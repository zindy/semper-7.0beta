C Semper 6.3 processing module MAPPIX
C
      SUBROUTINE MAPPIX
C
C Maps pixels values from LP1 into LP2, using map in LP3;
C if map is a histogram, forces equalistion or gaussian form
C assuming map to be histogram of source
C
C Multi-layer; fp internally, with LUTs for byte
C
      LOGICAL SEMROW,OPT,IQRNGE
      INTEGER NCW,NCOL,NROW,NLAY,I,I2,J,L,IU,NLVLS
      INTEGER NRANGE,NRA2,NWITH,NFROM,NGAUSS,IVALPN,INFORM,OUFORM
      REAL R1,R2,SA,SB,U,U1,U2,T,A,B,VAL,V,V1,V2,ZMIN,ZMAX
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(1:LNBUF/LNINT),IB2(1:LNBUF/LNINT),IB3(LNBUF/LNINT)
      EQUIVALENCE (IB1,RB1),(IB2,RB2),(IB3,RB3)
C
C Packed names
C
      PARAMETER (NRANGE=28854,NRA2=28872,NWITH=-5181,NFROM=10335)
      PARAMETER (NGAUSS=11261)
C
C Fault map which is not 1-D
C
      IF (NROWS(LP3).NE.1.OR.NLAYS(LP3).NE.1) THEN
         ERROR=5
         IDERR=IVALPN(NWITH)
         GOTO 130
      ENDIF
C
C Read map from LP3
C
      IF (SEMROW(1,RB3,NFMFP,1,1,LP3)) GOTO 130
      NCW=NCOLS(LP3)
C
C If WITH is a histogram, generate equalising map
C
      IF (CLASSN(LP3).NE.NCLHIS) GOTO 30
      NCW=NCW-2
      R1=RB3(NCW+1)
      R2=RB3(NCW+2)
      IF (OPT(NGAUSS)) THEN
         U1=4./REAL(NCW-1)
         U=-2.
      ELSE
         U1=0.
         U=0.
         ENDIF
      A=0.
      DO 10 I=1,NCW
         T=RB3(I)
         RB3(I)=A
         A=A+T*EXP(U**2/2.)
         U=U+U1
   10 CONTINUE
      NCW=NCW+1
      RB3(NCW)=A
      A=(R2-R1)/A
      DO 20 I=1,NCW
         RB3(I)=RB3(I)*A+R1
   20 CONTINUE
      GOTO 40
C
C Fetch and check input range for map
C
   30 R1=VAL(NRANGE)
      R2=VAL(NRA2)
      IF (R1.EQ.R2) THEN
         ERROR=3
         IDERR=NRANGE
         GOTO 130
      ENDIF
C
C Set up control parameters
C
   40 U2 = NCW
      A = (U2-1.)/(R2-R1)
      B = 1.0-R1*A
C
C N.B. A,B convert pixel-values to map-indices as follows :-
C   X (pixel in range R1..R2)  -->  A*X+B (index in range 1..NCW)
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      V1=RB3(1)
      V2=RB3(NCW)
      OUFORM = FORMN(LP2)
      IF (OUFORM .EQ. NFMBYT) THEN
         OUFORM = NFMINT
      ELSE
         OUFORM = NFMFP
      ENDIF
C
C Check for LUT case (number of 'levels' in map picture)
C
      IF (IQRNGE(LP1,NLVLS,ZMIN,ZMAX))  GOTO 130
      IF ((NLVLS .GT. 0) .AND. (NLVLS .LE. LNBUF/LNINT)) THEN
C
C Work out conversion from lut-indices to map_indices
C - need SA,SB such that (map-index) = INT((lut_index)*SA+SB)
C
         SA = A
         SB = A*(ZMIN-1.0)+B
C
C Set up look-up table
C
         DO 50 I=1,NLVLS
C
C First convert lut-index(I) to map-index(U)
C
            U = REAL(I)*SA+SB
C
C Now lookup map-index(U) -> output-value(V)
C
            IF (U.LE.1.) THEN
               V=V1
            ELSE IF (U.GE.U2) THEN
               V=V2
            ELSE
               IU=U
               V=RB3(IU)+(U-REAL(IU))*(RB3(IU+1)-RB3(IU))
            ENDIF
C
C Write final value to lut (INT or FP)
C
            IF (OUFORM .EQ. NFMINT) THEN
               IB2(I) = NINT(V)
            ELSE
               RB2(I) = V
            ENDIF
   50    CONTINUE
C
C Work out indexing offset into lut
C
         I2 = NINT(1.0-ZMIN)
C
C Convert picture in LP1 to LP2 via LUT in IB2
C - N.B. input as INT, lookup+output in form of LUT (OUFORM = INT/FP)
C
         DO 90 L=1,NLAY
            DO 80 J=1,NROW
               IF (SEMROW(1,RB1,NFMINT,J,L,LP1)) GOTO 130
               IF (OUFORM .EQ. NFMINT) THEN
                  DO 60 I=1,NCOL
                     IB3(I)=IB2(IB1(I)+I2)
   60             CONTINUE
               ELSE
                  DO 70 I=1,NCOL
                     RB3(I)=RB2(IB1(I)+I2)
   70             CONTINUE
               ENDIF
               IF (SEMROW(2,RB3,OUFORM,J,L,LP2)) GOTO 130
   80       CONTINUE
   90    CONTINUE
C
C Otherwise map picture using floating point conversion
C
      ELSE
C
C Check for picture in complex form
C
         IF (FORMN(LP1).EQ.NFMCOM) THEN
            INFORM=NFMCOM
            I2=2*NCOL
C
C Otherwise, process in floating-point form
C
         ELSE
            INFORM=NFMFP
            I2=NCOL
         ENDIF
C
C Convert picture in LP1 to LP2
C
         DO 120 L=1,NLAY
            DO 110 J=1,NROW
C
C Read source row from LP1
C
               IF (SEMROW(1,RB1,INFORM,J,L,LP1)) GOTO 130
C
C Convert picture row
C
               DO 100 I=1,I2
                  U=A*RB1(I)+B
                  IF (U.LE.1.) THEN
                     U=V1
                  ELSE IF (U.GE.U2) THEN
                     U=V2
                  ELSE
                     IU=U
                     U=RB3(IU)+(U-REAL(IU))*(RB3(IU+1)-RB3(IU))
                  ENDIF
                  RB1(I)=U
  100          CONTINUE
C
C Store result in LP2
C
               IF (SEMROW(2,RB1,INFORM,J,L,LP2)) GOTO 130
  110       CONTINUE
  120    CONTINUE
      ENDIF
C
  130 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C ===================================================================
C
      LOGICAL FUNCTION IQRNGE(LP,NLEVS,FMIN,FMAX)
C
      INTEGER LP,NLEVS
      REAL FMIN,FMAX
C
C Returns best available estimate of data-range and number of distinct
C  storage values in picture LP - but stops short of surveying
C NLEVS returns levels if reasonable (< 32768), else NLEVS < 0
C FMIN,FMAX return best range bounds known, else FMIN > FMAX
C
C Works for all pics, including display. FMIN<=FMAX if NLEVS>0
C
      INTEGER LABEL(256),PTR,I,N,MEDIUM,FORM
      REAL LEVS,OMIN,OMAX
      LOGICAL LSURVY
C
C Parameters
C
      REAL INTMAX
      PARAMETER (INTMAX=32767.0)
C
C Packed names
C
      INTEGER NPRESE,NSURVE
      PARAMETER (NPRESE=26325,NSURVE=31258)
C
C Includes
C
      INCLUDE 'COMMON'
C
C Functions
C
      LOGICAL SEMXA1,SEMLAB,GETRNG
C
C Equivalences
C
      EQUIVALENCE (RB1,LABEL)
C
C Initialise
C
      IQRNGE = .TRUE.
      LSURVY = .FALSE.
      NLEVS = -1
      LEVS = INTMAX
      FMIN = 1.0
      FMAX = 0.0
C
      MEDIUM = MEDN(DEVN(LP))
      FORM = FORMN(LP)
      IF (MEDIUM .EQ. MEDDS) THEN
C
C display pictures are special ...
C
         FMIN = GSMIN(LP)
         FMAX = GSMAX(LP)
         LEVS = REAL(LUTLEN)
      ELSEIF (FORM .EQ. NFMBYT) THEN
         LEVS = 256.0
         FMIN = 0.0
         FMAX = 255.0
      ELSEIF (FORM .EQ. NFMINT) THEN
         LEVS = 256.0 ** LNINT
         FMIN = -0.5*LEVS
         FMAX = 0.5*LEVS - 1.0
      ENDIF
C
C Recover range from picture label if possible, and enabled
C
      IF (WSTAT(LP).GT.0) GOTO 10
C
C Fetch label
C
      IF (SEMLAB(1,LABEL,LP)) GOTO 30
      IF (.NOT.LBLINC) GOTO 10
C
C Decode range
C
      N=LABEL(LBNCRR)+LBNCRR
      PTR=LBRR1
      IF (SEMXA1(2,LABEL,N,PTR,FMIN,I))  GOTO 10
      PTR=PTR+1
      IF (SEMXA1(2,LABEL,N,PTR,FMAX,I))  GOTO 10
      GOTO 20
C
C
   10 CONTINUE
C
C Survey if neccessary and enabled - N.B. preserve existing MIN,MAX
C
      IF ((FMIN .GT. FMAX) .AND. LSURVY) THEN
         IF (GETRNG(FMIN,FMAX,LP))  GOTO 30
      ENDIF
C
C Improve LEVS when possible (if MIN,MAX are now known)
C
   20 IF (FMIN .LE. FMAX) THEN
         IF (MEDIUM .EQ. MEDDS) THEN
            OMIN = GSMIN(LP)
            OMAX = GSMAX(LP)
            IF (OMAX .EQ. OMIN) THEN
C
C N.B. play safe !
C
               LEVS = INTMAX
            ELSE
               LEVS = LEVS - 1.0
               LEVS = 1.0+ABS( AINT(LEVS*((FMAX-OMIN)/(OMAX-OMIN)))
     +                       - AINT(LEVS*((FMIN-OMIN)/(OMAX-OMIN))) )
            ENDIF
         ELSEIF ((FORM .EQ. NFMBYT) .OR. (FORM .EQ. NFMINT)) THEN
            LEVS = 1.+FMAX-FMIN
         ELSE
            LEVS = INTMAX
         ENDIF
      ENDIF
C
C Reset NLEVS if LEVS is a reasonable number
C
         IF (LEVS .LT. INTMAX)  NLEVS = INT(LEVS)
C
C Normal return
C
      IQRNGE=.FALSE.
   30 RETURN
C
C Copyright (C) 1987-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
