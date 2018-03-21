C Semper 6 processing module MORPH
C
      SUBROUTINE MORPH
C
C Geometrical operator module for binary pictures, allowing arbitrary
C transformations on the basis of 3x3 source picture regions; the 8
C neighbours are used to construct a byte index to a lut giving the
C output pixel value.  Algorithm highly suitable for hardware encoding
C and/or parallelisation; present code complicated by:
C   (i) a large number of options setting up different maps, some of
C       which are sets of maps used in rotation;
C   (ii) considerable effort devoted to speeding up any passes after
C       the first, by noting (a) rows which were not changed by the
C       last application of any map, and (b) rows which can be
C       recognised (perhaps via a further map) as having achieved a
C       final configuration
C   (iii) avoiding output of rows unchanged during a given pass.
C The last two could all be dropped in a hardware implementation, in
C favour of brute force, quitting when no change is made by a complete
C map cycle (where PICCH is tested in the present code).
C
C VDs required:
C $ed :MORPH neighbours= times= with= >$ft
C Erode skeletonise ends nodes outline ol4 >$ed
C Dilate separately >$ed
C Median :MORPH >$ft
C
C 512 element maps are handled internally as two consecutive 256 element
C maps; MEDIAN is the only internal mode using one, but users may
C provide one via key WITH.
C
C Additional program (referred to by HELP) to be added to library:
C Genbmmap()
C ! Generates map for use with binary morphology verbs ERODE/DILATE
C ask 'Map number: ' m; create m size 256,1 value 0; origin left
C for m=0,255
C   for b=0,7; b#b=~and(m,2^b)=0; loop
C   type 'Index ',m,'  Configuration',#25,b7,b6,b5
C   type #25,b0,'   ',b4; type #25,b1,b2,b3
C   ask 'Value ' x; p m=x
C loop
C end
C
C Deficiencies/possible improvements:
C DIL SEP needs stable config recognition table like ERO (for speed)
C Some kind of combined ERODE;DILATE operations (commonly called OPEN
C   or CLOSE by other people?)
C Changing TIMES to count simple data passes, not cycles of passes
C   so that ERO SKE or DIL SEP could be applied in a partial but faster
C   form (current use of ERO SKE TIMES 1 removes two pixel border, I
C   suspect, rather than one).
C It may also be worth re-considering whether the new value of a given
C   pixel is used immediately in the next 3x3 neighbourhood - this might
C   well allow faster skeletonise/inverse skeletonise (because it would
C   not be necessary to use more than one map in turn); I suspect I
C   decided against that route early as being likely to be less amenable
C   to fast microcoding in hardware, and I may of course not have been
C   right!
C
C Global declarations (with suggested additions)
C
      INTEGER IVAL,IVALPN,SEMPPN,VALBIT
      LOGICAL SEMOPN,SEMROW,VARSET,OPT
C
      INCLUDE 'COMMON'
C
C Suggested additional globals (we should probably add # reals per
C buffer MAXRPB, etc. as well)
C
      INTEGER MAXIPB
      PARAMETER (MAXIPB=LNBUF/LNINT)
      INTEGER*4 MAXBI,REAL2I
      PARAMETER (REAL2I=LNREAL/LNINT)
      PARAMETER (MAXBI=6*(MAXIPB+2*LNEDGE*REAL2I))
C
C Configuration map selection parameters
C
      INTEGER SKM1,SKM2,SKM3,SKM4,NODEM,ENDM,OL8M,OL4M
      PARAMETER (SKM1=1,SKM2=SKM1+1,SKM3=SKM2+1,SKM4=SKM3+1)
      PARAMETER (NODEM=SKM4+1,ENDM=NODEM+1,OL8M=ENDM+1,OL4M=OL8M+1)
      INTEGER NS1M,NS2M,NS3M,NS4M,NS5M,NS6M,NS7M,NS8M
      PARAMETER (NS1M=OL4M+1,NS2M=NS1M+1,NS3M=NS2M+1,NS4M=NS3M+1)
      PARAMETER (NS5M=NS4M+1,NS6M=NS5M+1,NS7M=NS6M+1,NS8M=NS7M+1)
      INTEGER VSKM1,VSKM2,VSKM3,VSKM4,STSKM,LASTM
      PARAMETER (VSKM1=NS8M+1,VSKM2=VSKM1+1,VSKM3=VSKM2+1,VSKM4=VSKM3+1)
      PARAMETER (STSKM=VSKM4+1,LASTM=STSKM)
C
C Row status bit assignments
C
      INTEGER CTHISP,STABLE
      PARAMETER (CTHISP=8,STABLE=9)
C
      INTEGER*4 IBPS(3),I4,N4,NCOL4
      INTEGER IB1LHS(MAXBI),IB4(MAXIPB),IB5(MAXIPB),STATUS(MAXIPB)
      INTEGER CHBITS,CONFIG,MAP(0:511),MAPB(0:255),CFGM(0:15,LASTM)
C
      INTEGER I,J,JM,JP,K,L,M,MAP1,MAP2,MAPBN,MAPN,MAXTIM,MP,N
      INTEGER NCOL,NEWV,NNGHS,NPASS,NROW,OLDV,ROW
      LOGICAL SKEL,NEIGHB,USRMAP,MAP512
      LOGICAL PICCH,CHANGD,RSTABL
C
C Packed names
C
      INTEGER NTIMES,NWITH,NSKELE,NENDS,NNODES,NOUTLI
      INTEGER NOL4,NNEIGH,NSEPAR,NERODE,NMEDIA,NFROM
      PARAMETER (NTIMES=-374,NWITH=-5181)
      PARAMETER (NSKELE=30845,NENDS=8564,NNODES=23004,NOUTLI=24860)
      PARAMETER (NOL4=24514,NNEIGH=22609,NSEPAR=30616)
      PARAMETER (NERODE=8735,NMEDIA=21004,NFROM=10335)
C
      EQUIVALENCE (RB1LHS,IB1LHS),(RB4,IB4),(RB5,IB5),(RB6,STATUS)
      EQUIVALENCE (IB4,MAP),(IB4(257),MAPB)
C
C Built-in geometrical data table, as a set of 256 entry binary luts,
C ------------------------------  each packed into 16 integers of at
C least 16 bits each, with bits numbered from lsb - e.g. the 2^0 bit
C of the first integer contains the result for all zero neighbours.
C The neighbour configuration for a given pixel is converted into an
C 8-bit value (0 to 255), by combining neighbour bits (0 or 1) with the
C following numbering (0=lsb,7=msb): 7 6 5
C                                    0   4
C                                    1 2 3
C and using the resulting value to index the lut for the result. (For
C a few cases, the central pixel is used to provide a b8 and the lut
C is 512 elements long instead)
C
C 8-connected skeletonisation (map 1 of 4 to be used in rotation)
C
      DATA (CFGM(I,SKM1),I=0,15)
     +   / 12055, 11823,    -1, 11822,    -1,    -2,    -2,-20818,
     +     32597, 32639,    -1, 32639,    -2,    -2,    -2,-20818/
C
C 8-skeletonisation (map 2 of 4)
C
      DATA (CFGM(I,SKM2),I=0,15)
     +   /  3863,  -257,    -1,  -258, 32629,  -258, 32628, -2828,
     +     32629,    -1,    -1,    -1, 32628,    -1, 32628, -2828/
C
C 8-skeletonisation (map 3 of 4)
C
      DATA (CFGM(I,SKM3),I=0,15)
     +   /-20553,-20817,    -1,-20738,   -11,-20738,    -2,-20738,
     +       -11,    -1,    -1,    -1,   -12,-23308,   -12,-23308/
C
C 8-skeletonisation (map 4 of 4)
C
      DATA (CFGM(I,SKM4),I=0,15)
     +   /  3863,  3823,    -1,  3822,    -3,    -1,    -4,    -1,
     +     24405, 24575,    -1, 24575, 24404,    -1, 24404,    -1/
C
C Configurations that cannot be eroded by further 8-skeletonisation
C
      DATA (CFGM(I,STSKM),I=0,15)
     +   /  3863,  3599,    -1,  3598, 24405,     4, 21844,  1028,
     +     24405, 24415,    -1, 24415, 24404,  1028, 24404,  1028/
C
C Node deletion from a skeleton
C
      DATA (CFGM(I,NODEM),I=0,15)
     +   /  1846,    23,   279,     0,   279,     1,     0,     0,
     +       277,     1,     1,     0,     0,     0,     0,     0/
C
C Line end deletion from a skeleton
C
      DATA (CFGM(I,ENDM),I=0,15)
     +   / -20832,  -258,    -2,  -258,    -2,    -1,    -2,    -1,
     +        -12,    -1,    -1,    -1,    -2,    -1,    -2,    -1/
C
C 8-connected outline generation
C
      DATA (CFGM(I,OL8M),I=0,15)
     +   /    -2,    -1,    -1,    -1,    -1, 24415,    -1, 24415,
     +        -1,    -1,    -1,    -1,    -1, 24415,    -1, 24415/
C
C 4-connected outline generation
C
      DATA (CFGM(I,OL4M),I=0,15)
     +   /    -2,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     +        -1,    -1,    -1,    -1,    -1,    -1,    -1, 32767/
C
C Erosion/dilation for at least 1 neighbour clear/set
C
      DATA (CFGM(I,NS1M),I=0,15)
     +   /    -2,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     +        -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1/
C
C Erosion/dilation for at least 2 neighbours clear/set
C
      DATA (CFGM(I,NS2M),I=0,15)
     +   /  -280,    -2,    -2,    -1,    -2,    -1,    -1,    -1,
     +        -2,    -1,    -1,    -1,    -1,    -1,    -1,    -1/
C
C Erosion/dilation for at least 3 neighbours clear/set
C
      DATA (CFGM(I,NS3M),I=0,15)
     +   / -6016,  -280,  -280,    -2,  -280,    -2,    -2,    -1,
     +      -280,    -2,    -2,    -1,    -2,    -1,    -1,    -1/
C
C Erosion/dilation for at least 4 neighbours clear/set
C
      DATA (CFGM(I,NS4M),I=0,15)
     +   /-32768, -6016, -6016,  -280, -6016,  -280,  -280,    -2,
     +     -6016,  -280,  -280,    -2,  -280,    -2,    -2,    -1/
C
C Erosion/dilation for at least 5 neighbours clear/set
C
      DATA (CFGM(I,NS5M),I=0,15)
     +   /     0,-32768,-32768, -6016,-32768, -6016, -6016,  -280,
     +    -32768, -6016, -6016,  -280, -6016,  -280,  -280,    -2/
C
C Erosion/dilation for at least 6 neighbours clear/set
C
      DATA (CFGM(I,NS6M),I=0,15)
     +   /     0,     0,     0,-32768,     0,-32768,-32768, -6016,
     +         0,-32768,-32768, -6016,-32768, -6016, -6016,  -280/
C
C Erosion/dilation for at least 7 neighbours clear/set
C
      DATA (CFGM(I,NS7M),I=0,15)
     +   /     0,     0,     0,     0,     0,     0,     0,-32768,
     +         0,     0,     0,-32768,     0,-32768,-32768, -6016/
C
C Erosion/dilation for all 8 neighbours clear/set
C
      DATA (CFGM(I,NS8M),I=0,15)
     +   /     0,     0,     0,     0,     0,     0,     0,     0,
     +         0,     0,     0,     0,     0,     0,     0,-32768/
C
C Inverse 4-connected skeletonisation (map 1 of 4)
C
      DATA (CFGM(I,VSKM1),I=0,15)
     +   /     0,     0,     0,     0,    11,   257,    11,   257,
     +         0,     0,     0,     0,    11,   257,    11,   257/
C
C Inverse 4-skeletonisation (map 2 of 4)
C
      DATA (CFGM(I,VSKM2),I=0,15)
     +   / -3856, 20560,     0, 20560,     0,     0,     0,     0,
     +    -24416,     0,     0,     0,     0,     0,     0,     0/
C
C Inverse 4-skeletonisation (map 3 of 4)
C
      DATA (CFGM(I,VSKM3),I=0,15)
     +   /-24406,     0,     0,     0,    10,     0,    10,     0,
     +    -24406,     0,     0,     0,    10,     0,    10,     0/
C
C Inverse 4-skeletonisation (map 4 of 4)
C
      DATA (CFGM(I,VSKM4),I=0,15)
     +   /     0, 20817,     0, 20817,     0,   257,     0,   257,
     +         0,     0,     0,     0,     0,   257,     0,   257/
C
C Note: median output is achieved by using table NS4M when the centre
C is set and NS5M when it is clear
C
C Establish picture parameters
C
      NCOL=NCOLS(LP1)
      NCOL4=NCOL
      NROW=NROWS(LP1)
C
C Fault pictures with ncols or nrows < 3, or nrows > maxipb
C
      IF (NCOL.LT.3.OR.NROW.LT.3.OR.NROW.GT.MAXIPB) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 170
      ENDIF
C
C Prepare buffer pointers
C
      L=LNBUF/LNREAL
      N=LNREAL/LNINT
      I4=LNEDGE*N+1
      DO 10 K=1,3
         IBPS(K)=I4
         I4=I4+(L+2*LNEDGE)*N
   10 CONTINUE
C
C Establish mode
C
      NEIGHB=VARSET(NNEIGH)
      IF (NEIGHB) THEN
         NNGHS=IVAL(NNEIGH)
         IF (NNGHS.LE.0.OR.NNGHS.GT.8) THEN
            ERROR=3
            IDERR=NNEIGH
            GOTO 170
         ENDIF
      ENDIF
C
C Set map numbers, new pixel value and cycle count for each mode
C
      MAXTIM=1
      MAP2=-1
      MAPBN=-1
      USRMAP=.FALSE.
      MAP512=.FALSE.
      SKEL=.FALSE.
C
C Erode mode
C
      IF (VERB.EQ.NERODE) THEN
C
C Erode skeleton: 4 maps in turn, subsidiary map, iterated
C
         SKEL=OPT(NSKELE)
         IF (SKEL) THEN
            MAP1=SKM1
            MAP2=SKM4
            MAPBN=STSKM
            MAXTIM=0
C
C Erode nodes
C
         ELSE IF (OPT(NNODES)) THEN
            MAP1=NODEM
C
C Erode ends: iterated until stable
C
         ELSE IF (OPT(NENDS)) THEN
            MAP1=ENDM
            MAXTIM=0
C
C Erode outline:
C
         ELSE IF (OPT(NOUTLI)) THEN
            MAP1=OL8M
C
C Erode 4-con outline
C
         ELSE IF (OPT(NOL4)) THEN
            MAP1=OL4M
C
C Erode >= given number of clear neighbours
C
         ELSE IF (NEIGHB) THEN
            MAP1=NS8M+1-NNGHS
         ELSE
C
C Erode default: erode for any clear neighbours
C
            MAP1=NS8M
         ENDIF
C
C New pixel value
C
         NEWV=0
C
C Dilation mode
C
      ELSE IF (VERB.NE.NMEDIA) THEN
C
C Dilate separated: 4 maps in rotation, iterated
C
         IF (OPT(NSEPAR)) THEN
            MAP1=VSKM1
            MAP2=VSKM4
            MAXTIM=0
C
C Worth adding map later recognising inv 4-skeletonised configs??
C Dilate >= given number of set neighbours
C
         ELSE IF (NEIGHB) THEN
            MAP1=NS1M-1+NNGHS
C
C Dilate default: dilate for any set neighbour
C
         ELSE
            MAP1=NS1M
         ENDIF
C
C New pixel value
C
         NEWV=1
C
C Median mode: subsidiary map used if centre clear
C
      ELSE
         MAP1=NS5M
         MAPBN=NS4M
         MAP512=.TRUE.
C
C NEWV in fact irrelevant in this mode
C
         NEWV=0
C
C Prevent map or iteration count overrides
C
         GOTO 20
      ENDIF
C
C Override default map or number of passes
C
      USRMAP=VARSET(NWITH)
      IF (USRMAP) THEN
         N=SEMPPN(IVAL(NWITH))
         IF (SEMOPN(1,N,I,J,L,K,M,LP3)) GOTO 170
         MAP512=I.EQ.512
         IF (.NOT.(MAP512.OR.I.EQ.256).OR.J.NE.1.OR.L.GT.8) THEN
            ERROR=5
            IDERR=N
            GOTO 170
         ENDIF
         MAP1=1
         MAP2=L
      ENDIF
      IF (VARSET(NTIMES)) MAXTIM=IVAL(NTIMES)
C
C Unpack any subsidiary map; skeletonisation uses one to recognise
C already skeletonised configurations (so as to be able to omit them
C from future scans), and median filtering switches between two maps
C depending on the central pixel value
C
   20 IF (MAP2.LT.0) MAP2=MAP1
      IF (MAPBN.GT.0) THEN
         MP=0
         DO 40 N=0,15
            DO 30 I=0,15
               MAPB(MP)=VALBIT(CFGM(N,MAPBN),I)
               MP=MP+1
   30       CONTINUE
   40   CONTINUE
      ENDIF
C
C Construct mask testing all maps used
C
      CHBITS=0
      DO 50 MAPN=MAP1,MAP2
         CALL SETBIT(CHBITS,MOD(MAPN,8))
   50 CONTINUE
C
C Loop over layers
C
      DO 160 L=1,NLAYS(LP1)
C
C Initialise row change records
C - bit CTHISP => row changed this pass (at least 8)
C - bit STABLE => row cannot be eroded by further skeletonisation
C - bits MAP1-MAP2 => row changed last pass of corresponding map;
C   these bit numbers taken modulo 8, to leave other status bits free,
C   which imposes a limit of 8 on the number of maps usable in a
C   single command - not currently trapped..
C
         DO 60 J=1,NROW
            STATUS(J)=CHBITS
   60    CONTINUE
C
C Initialise pass and map number
C
         NPASS=1
         MAPN=MAP1
         PICCH=.FALSE.
C
C Initialise records of which row is held in each buffer
C
   70    IB1LHS(IBPS(1)-2)=-1
         IB1LHS(IBPS(2)-2)=-1
         IB1LHS(IBPS(3)-2)=-1
C
C Clear 'changed this pass' flags
C
      DO 80 J=1,NROW
         CALL CLRBIT(STATUS(J),CTHISP)
   80 CONTINUE
C
C Construct configuration map by unpacking CFG table..
C
      IF (.NOT.USRMAP) THEN
         MP=0
         DO 100 N=0,15
            M=CFGM(N,MAPN)
            DO 90 I=0,15
               MAP(MP)=VALBIT(M,I)
               MP=MP+1
   90       CONTINUE
  100    CONTINUE
C
C ..or by reading user supplied map
C
      ELSE
         IF (SEMROW(1,MAP,NFMINT,1,MAPN,LP3)) GOTO 170
      ENDIF
C
C Loop over target (output) rows, counted by J
C
         DO 140 J=1,NROW
C
C Input stage
C -----------
C Skip row if stable
C
            IF (VALBIT(STATUS(J),STABLE).NE.0) GOTO 130
C
C Skip row if neither this row nor its neighbours changed last pass
C
            JP=MAX(MIN(J+1,NROW),1)
            JM=MAX(MIN(J-1,NROW),1)
            IF (IAND(STATUS(J),CHBITS)+IAND(STATUS(JP),CHBITS)
     +         +IAND(STATUS(JM),CHBITS).EQ.0) GOTO 130
C
C Fetch row above, current row, and next row as necessary
C (most of the time, only the next row is necessary; this relatively
C complicated buffer filling allows passes after the first to skip
C stable source rows without messing up the buffering scheme)
C
            DO 110 K=1,3
               ROW=J+K-2
               I4=IBPS(K)
               IF (IB1LHS(I4-2).NE.ROW) THEN
                  IB1LHS(I4-2)=ROW
C
C Copy rows outside top,bottom from outside rows
C
                  ROW=MAX(MIN(ROW,NROW),1)
C
C Read it into buffer 3 of cycled set
C
                  IF (SEMROW(1,IB1LHS(IBPS(K)),NFMINT,ROW,1,LP1))
     +               GOTO 170
C
C Propagate edge pixels
C
                  IB1LHS(I4-1)=IB1LHS(I4)
                  IB1LHS(I4+NCOL)=IB1LHS(I4+NCOL-1)
               ENDIF
  110       CONTINUE
C
C Copy central row to output buffer IB5
C
            CALL CFORM(IB1LHS(IBPS(2)),IB5,NFMINT,NFMINT,NCOL4)
C
C Central processing: loop over pixels within row
C ------------------
C
            CHANGD=.FALSE.
            RSTABL=.TRUE.
            DO 120 I=1,NCOL
C
C Skip this pixel?
C
               N4=IBPS(2)+I-1
               IF (IB1LHS(N4).EQ.0) THEN
                  OLDV=0
               ELSE
                  OLDV=1
               ENDIF
C
               IF (OLDV.EQ.NEWV.AND..NOT.MAP512) GOTO 120
C
C Construct map index
C
               CONFIG=0
               I4=IBPS(1)+I-1
               IF (IB1LHS(I4-1).NE.0) CONFIG=CONFIG+128
               IF (IB1LHS(I4).NE.0)   CONFIG=CONFIG+64
               IF (IB1LHS(I4+1).NE.0) CONFIG=CONFIG+32
C
               IF (IB1LHS(N4-1).NE.0) CONFIG=CONFIG+1
               IF (IB1LHS(N4+1).NE.0) CONFIG=CONFIG+16
C
               I4=IBPS(3)+I-1
               IF (IB1LHS(I4-1).NE.0) CONFIG=CONFIG+2
               IF (IB1LHS(I4).NE.0)   CONFIG=CONFIG+4
               IF (IB1LHS(I4+1).NE.0) CONFIG=CONFIG+8
C
C Include central pixel as b8 if median
C
               IF (MAP512) THEN
                  IF (OLDV.NE.0) CONFIG=CONFIG+256
               ENDIF
C
C If new value different, reset pixel and note change
C
               IF (MAP(CONFIG).NE.OLDV) THEN
                  IB5(I)=MAP(CONFIG)
                  CHANGD=.TRUE.
C
C If skeletonising, consult subsidiary map re pixel stability
C
               ELSE
                  IF (SKEL) THEN
                     IF (MAPB(CONFIG).EQ.0) RSTABL=.FALSE.
                  ENDIF
               ENDIF
C
C Increment pixel number and loop
C
  120       CONTINUE
C
C Update row stability records
C
            IF (SKEL.AND.RSTABL) CALL SETBIT(STATUS(J),STABLE)
C
C Output stage
C ------------
C If changed, output row from IB5 unless in-situ
C
            IF (CHANGD.OR.LP1.NE.LP2) THEN
               IF (SEMROW(2,IB5,NFMINT,J,1,LP2)) GOTO 170
            ENDIF
C
C Note the change
C
            IF (CHANGD) THEN
               CALL SETBIT(STATUS(J),CTHISP)
               PICCH=.TRUE.
            ENDIF
C
C Cycle the buffer pointers and contents records
C
  130       I4=IBPS(1)
            IBPS(1)=IBPS(2)
            IBPS(2)=IBPS(3)
            IBPS(3)=I4
C
C End of loop over rows
C
  140    CONTINUE
C
C Take any further input from LP2
C
         LP1=LP2
C
C Copy 'changed this pass' list to 'changed last pass' posn for
C current map
C
      DO 150 J=1,NROW
         IF (VALBIT(STATUS(J),CTHISP).NE.0) THEN
            CALL SETBIT(STATUS(J),MOD(MAPN,8))
         ELSE
            CALL CLRBIT(STATUS(J),MOD(MAPN,8))
         ENDIF
  150 CONTINUE
C
C More maps to apply?
C
      IF (MAPN.NE.MAP2) THEN
         MAPN=MAPN+1
         GOTO 70
C
C Picture stable (or pass count exhausted)?
C
      ELSE IF (PICCH.AND.NPASS.NE.MAXTIM) THEN
         NPASS=NPASS+1
         MAPN=MAP1
         PICCH=.FALSE.
         GOTO 70
      ENDIF
C
C End of loop over layers
C
  160 CONTINUE
C
  170 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Modules VALBIT, SETBIT, CLRBIT test, set and clear bit B of N
C The lsb is numbered 0; 16 bit positions only are supported, so
C N must lie in -32768 to 32767 and B in 0 to 15
C
      INTEGER FUNCTION VALBIT(N,B)
C
C Returns value (0/1) of bit B of N
C
C Table of 16 bit masks, each with one bit only set in range b0-b15
C
      INTEGER N,B,BITSET(0:15)
C
      DATA BITSET/1,2,4,8,16,32,64,128,256,512,1024,
     +            2048,4096,8192,16384,-32768/
C
      IF (IAND(N,BITSET(B)).EQ.0) THEN
         VALBIT=0
      ELSE
         VALBIT=1
      ENDIF
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE SETBIT(N,B)
C
C Sets bit B of N
C
      INTEGER N,B,BITSET(0:15)
C
      DATA BITSET/1,2,4,8,16,32,64,128,256,512,1024,
     +            2048,4096,8192,16384,-32768/
C
      N=IOR(N,BITSET(B))
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE CLRBIT(N,B)
C
C Clears bit B of N
C
      INTEGER N,B,BITCLR(0:15)
C
      DATA BITCLR/-2,-3,-5,-9,-17,-33,-65,-129,-257,-513,-1025,
     +            -2049,-4097,-8193,-16385,32767/
C
      N=IAND(N,BITCLR(B))
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
