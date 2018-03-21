C Semper 6 processing module BMLUT
C
      SUBROUTINE BMLUT
C
C Simple command to convert old-style data for ERODE and DILATE into
C new-style LUT for use with the more recent BMMAP command.  BMLUT
C accepts the options ERODE and DILATE to determine whether the
C remaining options apply to ERODE or DILATE.  Also accepts option
C MEDIAN to generate mapping table for old MEDIAN command.  Provides
C new-style keys IF and UNLESS to allow the user to create his own
C mapping tables by specifying neighbourhood configurations in terms
C of simple logical expressions (one per sub-iteration).
C
      LOGICAL CONOPT,OPT,VARSET,SEMOPN,SEMROW,SEMXPL,SEMLU,SEMCEN
      INTEGER IPACK,IVAL,IVALPN
C
      INTEGER*2 IHOOD
      INTEGER   I,J,K,K1,K2,M,NM,N,NPIC,NCOL,NROW,NLAY,CLASS,FORM,IPTR
      INTEGER   IPTRIF,IPTRUN,NIF,NUN,P(0:8),N4,N8,C,C4,C8,NAME(14)
      REAL      VALUE(14)
      LOGICAL   LIF,LUN,LERODE,LDILAT,LMEDIA
C
      INCLUDE 'COMMON'
C
      INTEGER MAXLUT,MAXMAP
      PARAMETER ( MAXLUT = 2*LNBUF/LNINT/512 )
      PARAMETER ( MAXMAP = 4*LNBUF/2/16 )
C
      REAL VALIF(MAXLUT),VALUN(MAXLUT)
C
      INTEGER   LUT(0:511,MAXLUT)
      INTEGER   LUT1(0:255)
      INTEGER   LUT2(0:511)
      INTEGER*2 MAP(0:15,MAXMAP)
C
      EQUIVALENCE (LUT,LUT1,RB1),(LUT2,RB2),(MAP,RB3)
C
      INTEGER LUT12(0:7)
C
      INTEGER*2 BITPOS(0:15)
C
C LUT selection parameters
C
      INTEGER NODE,END,OL4,OL8
      INTEGER SK1,SK2,SK3,SK4
      INTEGER NS1,NS2,NS3,NS4,NS5,NS6,NS7,NS8
      INTEGER DS1,DS2,DS3,DS4
C
      PARAMETER (NODE=1, END=NODE+1, OL4=END+1, OL8=OL4+1)
      PARAMETER (SK1=OL8+1, SK2=SK1+1, SK3=SK2+1, SK4=SK3+1)
      PARAMETER (NS1=SK4+1, NS2=NS1+1, NS3=NS2+1, NS4=NS3+1)
      PARAMETER (NS5=NS4+1, NS6=NS5+1, NS7=NS6+1, NS8=NS7+1)
      PARAMETER (DS1=NS8+1, DS2=DS1+1, DS3=DS2+1, DS4=DS3+1)
C
C Built-in geometrical data table, as a set of 256 entry binary luts,
C ------------------------------  each packed into 16 integers of at
C least 16 bits each, with bits numbered from lsb - e.g. the 2^0 bit
C of the first integer contains the result for all zero neighbours.
C The neighbour configuration for a given pixel is converted into an
C 8-bit value (0 to 255), by combining neighbour bits (0 or 1) with the
C following numbering (0=lsb,7=msb):
C
C             7 6 5
C             0   4
C             1 2 3
C
C and using the resulting value to index the lut for the result.
C
      CHARACTER*2 NS(14)
      INTEGER*2 TABLE(0:15,DS4)
C
C Node deletion from a skeleton
C
      DATA (TABLE(I,NODE),I=0,15)
     +   /  1846,    23,   279,     0,   279,     1,     0,     0,
     +       277,     1,     1,     0,     0,     0,     0,     0/
C
C Line end deletion from a skeleton
C
      DATA (TABLE(I,END),I=0,15)
     +   / -20832,  -258,    -2,  -258,    -2,    -1,    -2,    -1,
     +        -12,    -1,    -1,    -1,    -2,    -1,    -2,    -1/
C
C 4-connected outline generation
C
      DATA (TABLE(I,OL4),I=0,15)
     +   /    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     +        -1,    -1,    -1,    -1,    -1,    -1,    -1, 32767/
C
C 8-connected outline generation
C
      DATA (TABLE(I,OL8),I=0,15)
     +   /    -1,    -1,    -1,    -1,    -1, 24415,    -1, 24415,
     +        -1,    -1,    -1,    -1,    -1, 24415,    -1, 24415/
C
C 8-connected skeletonisation (table 1 of 4 to be used in rotation)
C
      DATA (TABLE(I,SK1),I=0,15)
     +   / 12055, 11823,    -1, 11822,    -1,    -2,    -2,-20818,
     +     32597, 32639,    -1, 32639,    -2,    -2,    -2,-20818/
C
C 8-skeletonisation (table 2 of 4)
C
      DATA (TABLE(I,SK2),I=0,15)
     +   /  3863,  -257,    -1,  -258, 32629,  -258, 32628, -2828,
     +     32629,    -1,    -1,    -1, 32628,    -1, 32628, -2828/
C
C 8-skeletonisation (table 3 of 4)
C
      DATA (TABLE(I,SK3),I=0,15)
     +   /-20553,-20817,    -1,-20738,   -11,-20738,    -2,-20738,
     +       -11,    -1,    -1,    -1,   -12,-23308,   -12,-23308/
C
C 8-skeletonisation (table 4 of 4)
C
      DATA (TABLE(I,SK4),I=0,15)
     +   /  3863,  3823,    -1,  3822,    -3,    -1,    -4,    -1,
     +     24405, 24575,    -1, 24575, 24404,    -1, 24404,    -1/
C
C Erosion/dilation for at least 1 neighbour clear/set
C
      DATA (TABLE(I,NS1),I=0,15)
     +   /    -2,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     +        -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1/
C
C Erosion/dilation for at least 2 neighbours clear/set
C
      DATA (TABLE(I,NS2),I=0,15)
     +   /  -280,    -2,    -2,    -1,    -2,    -1,    -1,    -1,
     +        -2,    -1,    -1,    -1,    -1,    -1,    -1,    -1/
C
C Erosion/dilation for at least 3 neighbours clear/set
C
      DATA (TABLE(I,NS3),I=0,15)
     +   / -6016,  -280,  -280,    -2,  -280,    -2,    -2,    -1,
     +      -280,    -2,    -2,    -1,    -2,    -1,    -1,    -1/
C
C Erosion/dilation for at least 4 neighbours clear/set
C
      DATA (TABLE(I,NS4),I=0,15)
     +   /-32768, -6016, -6016,  -280, -6016,  -280,  -280,    -2,
     +     -6016,  -280,  -280,    -2,  -280,    -2,    -2,    -1/
C
C Erosion/dilation for at least 5 neighbours clear/set
C
      DATA (TABLE(I,NS5),I=0,15)
     +   /     0,-32768,-32768, -6016,-32768, -6016, -6016,  -280,
     +    -32768, -6016, -6016,  -280, -6016,  -280,  -280,    -2/
C
C Erosion/dilation for at least 6 neighbours clear/set
C
      DATA (TABLE(I,NS6),I=0,15)
     +   /     0,     0,     0,-32768,     0,-32768,-32768, -6016,
     +         0,-32768,-32768, -6016,-32768, -6016, -6016,  -280/
C
C Erosion/dilation for at least 7 neighbours clear/set
C
      DATA (TABLE(I,NS7),I=0,15)
     +   /     0,     0,     0,     0,     0,     0,     0,-32768,
     +         0,     0,     0,-32768,     0,-32768,-32768, -6016/
C
C Erosion/dilation for all 8 neighbours clear/set
C
      DATA (TABLE(I,NS8),I=0,15)
     +   /     0,     0,     0,     0,     0,     0,     0,     0,
     +         0,     0,     0,     0,     0,     0,     0,-32768/
C
C Inverse 4-connected skeletonisation (table 1 of 4)
C
      DATA (TABLE(I,DS1),I=0,15)
     +   /     0,     0,     0,     0,    11,   257,    11,   257,
     +         0,     0,     0,     0,    11,   257,    11,   257/
C
C Inverse 4-skeletonisation (table 2 of 4)
C
      DATA (TABLE(I,DS2),I=0,15)
     +   / -3856, 20560,     0, 20560,     0,     0,     0,     0,
     +    -24416,     0,     0,     0,     0,     0,     0,     0/
C
C Inverse 4-skeletonisation (table 3 of 4)
C
      DATA (TABLE(I,DS3),I=0,15)
     +   /-24406,     0,     0,     0,    10,     0,    10,     0,
     +    -24406,     0,     0,     0,    10,     0,    10,     0/
C
C Inverse 4-skeletonisation (table 4 of 4)
C
      DATA (TABLE(I,DS4),I=0,15)
     +   /     0, 20817,     0, 20817,     0,   257,     0,   257,
     +         0,     0,     0,     0,     0,   257,     0,   257/
C
C Table to map old-style neighbourhood positions to new-style positions
C
C                          7 6 5
C Old n/hood positions =>  0 * 4
C                          1 2 3
C
C                          0 3 6
C New n/hood positions =>  1 4 7
C                          2 5 8
C
C such that:               LUT12(old_position) = new_position
C
C
      DATA LUT12 / 1, 2, 5, 8, 7, 6, 3, 0 /
C
C Table to mask bit in given 16-bit position (0 => least sig. bit)
C
      DATA BITPOS / 1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
     +              1024, 2048, 4096, 8192, 16384, -32768 /
C
      DATA NS / 'p0', 'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8',
     +          'n4', 'n8', 'c ', 'c4', 'c8' /
C
C See if keys IF and UNLESS are set
C
      LIF=VARSET(14640)
      LUN=VARSET(-2173)
C
C If so, set up neighbourhood mapping tables according to specified
C logical expressions(s)
C
      IF (LIF.OR.LUN) THEN
C
C Make variables P0, P1, ... , P8, N4, N8, C, C4 and C8 local and set
C each one to zero
C
         DO 10 K=1,14
            NAME(K)=IPACK(NS(K))
C
            IF (SEMLU(2,NAME(K),0.0)) GOTO 210
   10    CONTINUE
C
C See if IF key is set
C
         IF (LIF) THEN
C
C Fetch key value which points to start of expression string in the
C command line buffer
C
            IPTRIF=IVAL(14640)
C
C Find out how many IF expressions are specified with dummy read
C
            IPTR=IPTRIF
            NIF=MAXLUT
            IF (SEMXPL(LINBUF,LINLEN,IPTR,VALIF,NIF)) GOTO 210
C
C Otherwise, set up default values for IF expressions
C
         ELSE
            DO 20 J=1,MAXLUT
               VALIF(J)=1.0
   20       CONTINUE
C
            NIF=1
         ENDIF
C
C See if UNLESS key is set
C
         IF (LUN) THEN
C
C Fetch key value which points to start of expression string in the
C command line buffer
C
            IPTRUN=IVAL(-2173)
C
C Find out how many IF expressions are specified with dummy read
C
            IPTR=IPTRUN
            NUN=MAXLUT
            IF (SEMXPL(LINBUF,LINLEN,IPTR,VALUN,NUN)) GOTO 210
C
C Otherwise, set up default values for UNLESS expressions
C
         ELSE
            DO 30 J=1,MAXLUT
               VALUN(J)=0.0
   30       CONTINUE
C
            NUN=1
         ENDIF
C
C Larger number of expressions determines number of sub-iterations
C (up to maximum of MAXLUT)
C
         N=MIN(MAX(NIF,NUN),MAXLUT)
C
C Open output picture to receive results (one row per sub-iteration)
C
         NPIC=IVALPN(-601)
         LP2=0
         IF (SEMOPN(2,NPIC,512,N,1,NCLUND,NFMBYT,LP2)) GOTO 210
C
C Run through all possible 3 x 3 neighbourhood configurations
C
         DO 70 I=0,511
            IHOOD=I
C
C Determine pixel settings for this neighbourhood configuration
C
            DO 40 J=0,8
               IF (IAND(IHOOD,BITPOS(J)).EQ.0) THEN
                  P(J)=0
               ELSE
                  P(J)=1
               ENDIF
C
               VALUE(J+1)=REAL(P(J))
   40       CONTINUE
C
C Determine number of 4-connected neighbours
C
            N4=P(1)+P(3)+P(5)+P(7)
C
            VALUE(10)=REAL(N4)
C
C Determine number of 8-connected neighbours
C
            N8=N4+P(0)+P(2)+P(6)+P(8)
C
            VALUE(11)=REAL(N8)
C
C Determine crossing number (number of 01 transitions around central
C pixel)
C
            C=0
            IF (P(0).EQ.0.AND.P(1).NE.0) C=C+1
            IF (P(1).EQ.0.AND.P(2).NE.0) C=C+1
            IF (P(2).EQ.0.AND.P(5).NE.0) C=C+1
            IF (P(5).EQ.0.AND.P(8).NE.0) C=C+1
            IF (P(8).EQ.0.AND.P(7).NE.0) C=C+1
            IF (P(7).EQ.0.AND.P(6).NE.0) C=C+1
            IF (P(6).EQ.0.AND.P(3).NE.0) C=C+1
            IF (P(3).EQ.0.AND.P(0).NE.0) C=C+1
C
            VALUE(12)=REAL(C)
C
C Determine crossing number adjusted for 4-connected surround
C
            C4=N4
            IF (P(0).NE.0.AND.P(1).NE.0.AND.P(3).NE.0) C4=C4-1
            IF (P(2).NE.0.AND.P(1).NE.0.AND.P(5).NE.0) C4=C4-1
            IF (P(6).NE.0.AND.P(3).NE.0.AND.P(7).NE.0) C4=C4-1
            IF (P(8).NE.0.AND.P(5).NE.0.AND.P(7).NE.0) C4=C4-1
C
            VALUE(13)=REAL(C4)
C
C Determine crossing number adjusted for 8-connected surround
C
            C8=0
            IF (P(1).NE.P(3)) C8=C8+1
            IF (P(1).NE.P(5)) C8=C8+1
            IF (P(3).NE.P(7)) C8=C8+1
            IF (P(5).NE.P(7)) C8=C8+1
            C8=C8/2
            IF (P(0).NE.0.AND.P(1).EQ.0.AND.P(3).EQ.0) C8=C8+1
            IF (P(2).NE.0.AND.P(1).EQ.0.AND.P(5).EQ.0) C8=C8+1
            IF (P(6).NE.0.AND.P(3).EQ.0.AND.P(7).EQ.0) C8=C8+1
            IF (P(8).NE.0.AND.P(5).EQ.0.AND.P(7).EQ.0) C8=C8+1
C
            VALUE(14)=REAL(C8)
C
C Set corresponding Semper variables
C
            DO 50 K=1,14
               IF (SEMLU(1,NAME(K),VALUE(K))) GOTO 210
   50       CONTINUE
C
C Determine values of IF expressions (if any)
C
            IF (LIF) THEN
               IPTR=IPTRIF
               IF (SEMXPL(LINBUF,LINLEN,IPTR,VALIF,NIF)) GOTO 210
            ENDIF
C
C Determine values of UNLESS expressions (if any)
C
            IF (LUN) THEN
               IPTR=IPTRUN
               IF (SEMXPL(LINBUF,LINLEN,IPTR,VALUN,NUN)) GOTO 210
            ENDIF
C
C Set corresponding mapping table entries according to results of IF
C and UNLESS expressions (cycle round expressions if too few specified)
C
            DO 60 J=1,N
               IF (VALIF(1+MOD(J-1,NIF)).EQ.0.0.OR.
     +             VALUN(1+MOD(J-1,NUN)).NE.0.0) THEN
                  LUT(I,J)=0
               ELSE
                  LUT(I,J)=1
               ENDIF
   60       CONTINUE
   70    CONTINUE
C
C Output mapping table(s) to disc
C
         DO 80 J=1,N
            IF (SEMROW(2,LUT(0,J),NFMINT,J,1,LP2)) GOTO 210
   80    CONTINUE
C
C Otherwise, look for option ERODE, DILATE or MEDIAN
C
      ELSE
C
C Fault conflicting options ERODE, DILATE and MEDIAN
C
         IF (CONOPT(8735,6772)) GOTO 210
         IF (CONOPT(6772,21004)) GOTO 210
         IF (CONOPT(21004,8735)) GOTO 210
C
C See if option ERODE, DILATE or MEDIAN is set
C
         LERODE=OPT(8735)
         LDILAT=OPT(6772)
         LMEDIA=OPT(21004)
C
C Fault missing option ERODE, DILATE or MEDIAN
C
         IF (.NOT.(LERODE.OR.LDILAT.OR.LMEDIA)) THEN
            ERROR=77
            IDMESS='Missing option - ERODE, DILATE or MEDIAN'
            GOTO 210
         ENDIF
C
C If MEDIAN option is set, generate mapping table directly
C
         IF (LMEDIA) THEN
C
C Open output picture to receive single mapping table for median filter
C
            NPIC=IVALPN(-601)
            LP2=0
            IF (SEMOPN(2,NPIC,512,1,1,NCLUND,NFMBYT,LP2)) GOTO 210
C
C Run through all possible configurations for 3x3 n/hood
C
            DO 100 I=0,511
               IHOOD=I
C
C Determine number of pixels set for this neighbourhood configuration
C
               N=0
               DO 90 J=0,8
                  IF (IAND(IHOOD,BITPOS(J)).NE.0) N=N+1
   90          CONTINUE
C
C Median filter => non-zero result if majority of pixels in 
C neighbourhood
C are set
C
               IF (N.GT.4) THEN
                  LUT2(I)=1
               ELSE
                  LUT2(I)=0
               ENDIF
  100       CONTINUE
C
C Output mapping table to disc
C
            IF (SEMROW(2,LUT2,NFMINT,1,1,LP2)) GOTO 210
C
C Otherwise, process key/options that relate to ERODE or DILATE option
C
         ELSE
C
C See if key WITH is set
C
            IF (VARSET(-5181)) THEN
C
C If so, open picture containing user-defined mapping tables
C
               NPIC=IVALPN(-5181)
               IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP3))
     +            GOTO 210
C
C Fault bad size for mapping table picture
C
               IF (NCOL.NE.256.OR.NROW.GT.MAXMAP.OR.NLAY.NE.1) THEN
                  ERROR=5
                  IDERR=NPIC
                  GOTO 210
               ENDIF
C
C Number of mapping tables = number of picture rows
C
               NM=NROW
C
C Copy user defined data into mapping table array
C
               DO 130 M=1,NM
C
C Read row = mapping table from disc
C
                  IF (SEMROW(1,LUT1,NFMINT,M,1,LP3)) GOTO 210
C
C Pack data into 16 integers of 16 bits each
C
                  N=0
                  DO 120 J=0,15
                     MAP(J,M)=0
                     DO 110 I=0,15
                        IF (LUT1(N).NE.0) THEN
                           MAP(J,M)=IOR(BITPOS(I),MAP(J,M))
                        ENDIF
                        N=N+1
  110                CONTINUE
  120             CONTINUE
  130          CONTINUE
C
C Otherwise, look for other keys/options according to whether
C ERODE or DILATE option is set
C
            ELSE
C
C If ERODE option is set, look for key/options relating to erosion
C
               IF (LERODE) THEN
C
C SKELETONISE option (8-connected skeleton)
C
                  IF (OPT(30845)) THEN
                     K1=SK1
                     K2=SK4
C
C NODES option
C
                  ELSE IF (OPT(23004)) THEN
                     K1=NODE
                     K2=K1
C
C ENDS option
C
                  ELSE IF (OPT(8564)) THEN
                     K1=END
                     K2=K1
C
C OL4 option (4-connected outline)
C
                  ELSE IF (OPT(24514)) THEN
                     K1=OL4
                     K2=K1
C
C OUTLINE option (8-connected outline)
C
                  ELSE IF (OPT(24860)) THEN
                     K1=OL8
                     K2=K1
C
C Otherwise, NEIGHBOURS key specifies minimum number of clear
C neighbours for eroding (default = 1)
C
                  ELSE
C
C See if NEIGHBOURS key is set
C
                     IF (VARSET(22609)) THEN
C
C Fault bad value for NEIGHBOURS key
C
                        N=IVAL(22609)
C
                        IF (N.LT.1.OR.N.GT.8) THEN
                           ERROR=3
                           IDERR=22609
                           GOTO 210
                        ENDIF
                     ELSE
                        N=1
                     ENDIF
C
                     K1=NS8-(N-1)
                     K2=K1
                  ENDIF
C
C Otherwise, if DILATE option is set, look for key/options relating
C to dilation
C
               ELSE IF (OPT(6772)) THEN
C
C SEPARATELY option
C
                  IF (OPT(30616)) THEN
                     K1=DS1
                     K2=DS4
C
C Otherwise, NEIGHBOURS key specifies minimum number of set
C neighbours for dilating (default = 1)
C
                  ELSE
C
C See if NEIGHBOURS key is set
C
                     IF (VARSET(22609)) THEN
C
C Fault bad value for NEIGHBOURS key
C
                        N=IVAL(22609)
C
                        IF (N.LT.1.OR.N.GT.8) THEN
                           ERROR=3
                           IDERR=22609
                           GOTO 210
                        ENDIF
                     ELSE
                        N=1
                     ENDIF
C
                     K1=NS1+(N-1)
                     K2=K1
                  ENDIF
               ENDIF
C
C Copy predefined data into mapping table array
C
               NM=0
               DO 150 K=K1,K2
                  NM=NM+1
                  DO 140 J=0,15
                     MAP(J,NM)=TABLE(J,K)
  140             CONTINUE
  150          CONTINUE
            ENDIF
C
C Open output picture to receive results (one row per sub-iteration)
C
            NPIC=IVALPN(-601)
            LP2=0
            IF (SEMOPN(2,NPIC,512,NM,1,NCLUND,NFMBYT,LP2)) GOTO 210
C
C Unpack each block of LUT data in turn and output it to disc
C
            DO 200 M=1,NM
C
C Unpack LUT data - 16 integers of 16 bits each
C
               N=0
               DO 170 J=0,15
                  DO 160 I=0,15
                     IF (IAND(BITPOS(I),MAP(J,M)).EQ.0) THEN
                        LUT1(N)=0
                     ELSE
                        LUT1(N)=1
                     ENDIF
                     N=N+1
  160             CONTINUE
  170          CONTINUE
C
C                                                           0 3 6
C Run through all possible configurations for 3x3 n/hood => 1 4 7
C                                                           2 5 8
C
               DO 190 I=0,511
                  IHOOD=I
C
C                                                     7 6 5
C Determine corresponding index into look-up table => 0 * 4
C                                                     1 2 3
C
                  N=0
                  DO 180 J=0,7
                     IF (IAND(IHOOD,BITPOS(LUT12(J))).NE.0) THEN
                        N=N+BITPOS(J)
                     ENDIF
  180             CONTINUE
C
C Store result according to whether central pixel is set and whether
C erosion or dilation specified
C
                  IF (IAND(IHOOD,BITPOS(4)).EQ.0) THEN
                     IF (LERODE) THEN
                        LUT2(I)=0
                     ELSE
                        LUT2(I)=LUT1(N)
                     ENDIF
                  ELSE
                     IF (LERODE) THEN
                        LUT2(I)=LUT1(N)
                     ELSE
                        LUT2(I)=1
                     ENDIF
                  ENDIF
  190          CONTINUE
C
C Output mapping table to disc
C
               IF (SEMROW(2,LUT2,NFMINT,M,1,LP2)) GOTO 210
  200       CONTINUE
         ENDIF
      ENDIF
C
C Reset picture origin to top left
C
      IF (SEMCEN(LP2,1,1,1)) GOTO 210
C
  210 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
