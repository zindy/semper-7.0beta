C Semper 6 processing module BHMT
C
      SUBROUTINE BHMT
C
C Carries out generalised binary Hit or Miss transforms with arbitrary
C structuring elements.  Any number of structuring elements can be
C applied in sequence by supplying them in successive layers of the
C multi-layer binary picture specified by means of the WITH key.  Zero
C pixel values define a Miss, ones define Hits and any other pixel
C values are ignored and have no effect.  The sequence of structuring
C elements can be applied more than once by specifying a number of
C iterations greater than 1 with the TIMES key.  A zero value for the
C TIMES key specifies an infinite number of iterations.  Processing
C automatically terminates if the image does not change after one
C complete iteration.  If the ADD or SUBTRACT option is set, the result
C of applying each structuring element to the current source image is
C added or subtracted from that image.  This provides for thickening and
C thinning transformations.  The changes brought about by each complete
C iteration can be restricted to an arbitrary region by specifying a
C binary mask picture with the MASK key.  Only those pixels where the
C corresponding mask pixel is non-zero are allowed to change.  The
C options SOURCE or OUTPUT can be used to control edge effects.  Any
C undefined source or output pixel is set to the value given by the
C EDGE key if either the SOURCE or OUTPUT option is set.  A source
C pixel is undefined if its position lies outside the source picture.
C An output pixel is undefined if any contributing source pixel is
C undefined.  The contibuting source pixel positions are obtained by
C positioning the origin of the structuring element on the output pixel
C and selecting all source pixels where the structuring element is zero
C or one.  By default, any undefined source pixels are omitted from the
C logical calculation used to obtain the output pixel value.  If P(i)
C represents the ith contributing source pixel value for a Hit and the
C inverse of the source pixel value for a Miss, the result is obtained
C by logically ANDing all the P(i).  The parameters that define the
C sequence of logical operations for a particular sequence of
C structuring elements is stored in the array SE and passed to the
C routine BMPROC to carry them out.  The image is converted into
C bit_packed form, and if it is too large to be stored entirely within
C one row buffer, is stored in a temporary disc picture.  Storing the
C data in pit-packed form dramatically reduces the amount of data
C storage required (by a factor of 8 or 16) and the amount of data
C traffic.  It also makes it possible to process 16 or 32 pixels at the
C same time by carrying out the logical calculations on whole words of
C bit-packed data.  This gives a speed up of between 10 and 25 times.
C Set against this is the overhead of converting to and from the
C bit-packed form.  For small (3 by 3) structuring elements, this takes
C the equivalent of applying 5 to 10 iterations of the structuring
C element, so the benefits only really become apparent for large numbers
C of iterations.  When Semper is able to support the bit-packed data
C form for storing binary pictures, the overhead of converting the data
C will disappear.
C
      LOGICAL CONOPT,OPT,SEMROW,BMPROC
      INTEGER IPACK,IVAL
      REAL    VAL
C
      INTEGER I,J,K,P,E,IOP,IOFF,NC,NP,NE,SFLAG(0:1),OFLAG
      INTEGER NCOL,NROW,NLAY,CCOL,CROW,IX,IY,IEDGE
C
      INCLUDE 'COMMON'
C
      INTEGER NR(LNBUF/LNINT,0:1)
      INTEGER SR(LNBUF/LNINT)
      INTEGER SO(LNBUF/LNINT,0:1)
      INTEGER IC(LNBUF/LNINT)
      INTEGER IR(LNBUF/LNINT)
      INTEGER IE(LNBUF/LNINT)
      INTEGER SE(LNBUF/LNINT)
C
      EQUIVALENCE (NR,RB1),(SR,SO,RB2),(IE,RB3)
      EQUIVALENCE (IC,RB4),(IR,RB5),(SE,RB6)
C
      INTEGER ASOP,HMOP(0:1,0:1)
      DATA HMOP / 3, 2, 12, 8 /
C
C Fault structuring element picture with too many columns to process
C
      IF (NCOLS(LP3).GT.LNBUF/LNINT/2) THEN
         ERROR=5
         IDERR=-5181
         GOTO 90
      ENDIF
C
C Fault conflicting options ADD and SUBTRACT
C
      IF (CONOPT(1764,31242)) GOTO 90
C
C Set up binary opcode for combining result of sub-iteration with
C input data according to whether or not option ADD/SUBTRACT is set
C (ADD => or(S,D), SUBTRACT => and(S,not(D)), else => do nothing)
C
      IF (OPT(1764)) THEN
         ASOP=14
      ELSE IF (OPT(31242)) THEN
         ASOP=4
      ELSE
         ASOP=10
      ENDIF
C
C Fault conflicting options SOURCE and OUTPUT
C
      IF (CONOPT(31021,24860)) GOTO 90
C
C Determine edge value according to value of EDGE key
C
      IF (VAL(8167).EQ.0.0) THEN
         IEDGE=0
      ELSE
         IEDGE=1
      ENDIF
C
C Set up source and output edge processing flags according to SOURCE
C and OUTPUT options and EDGE key (default is to set source edge pixels
C for no effect)
      IF (OPT(31021)) THEN
         SFLAG(0)=IEDGE
         SFLAG(1)=IEDGE
         OFLAG=-1
      ELSE IF (OPT(24860)) THEN
         SFLAG(0)=-1
         SFLAG(1)=-1
         OFLAG=IEDGE
      ELSE
         SFLAG(0)=0
         SFLAG(1)=1
         OFLAG=-1
      ENDIF
C
C Initialise structuring element limits
C
      SE(1)=0
      SE(2)=0
      SE(3)=0
      SE(4)=0
C
C Set number of processing phases
C
      SE(5)=1
C
C Fetch value for TIMES key
C
      SE(6)=IVAL(-374)
C
C Zero or negative iteration count defaults to infinite count
C
      IF (SE(6).LT.1) SE(6)=32766
C
C Initialise count of sub-iterations for structuring element data
C
      SE(7)=0
C
C Initialise pointer into array that will hold structuring element data
C
      IOFF=7
C
C Fetch size of picture describing structuring element
C
      NCOL=NCOLS(LP3)
      NROW=NROWS(LP3)
      NLAY=NLAYS(LP3)
C
C Fetch centre position for same
C
      CCOL=CCOLN(LP3)
      CROW=CROWN(LP3)
C
C Scan through structuring element data
C
      DO 80 K=1,NLAY
C
C Initialise counts for defined columns and defined elements
C for this layer
C
         NC=0
         NP=0
C
C Initialise counts for defined elements contained in each column
C
         DO 10 I=1,NCOL
            NR(I,0)=0
            NR(I,1)=0
   10    CONTINUE
C
C Scan through data for current layer
C
         DO 30 J=1,NROW
C
C Fetch structuring element data from disc
C
            IF (SEMROW(1,SR,NFMINT,J,K,LP3)) GOTO 90
C
C Scan through data for current row
C
            DO 20 I=1,NCOL
C
C Fetch current structuring element value
C
               E=SR(I)
C
C Check for defined (0 or 1) element
C
               IF (E.EQ.0.OR.E.EQ.1) THEN
C
C Increment count of defined elements
C
                  NP=NP+1
C
C Fault overflow of buffers holding column and row positions
C
                  IF (NP.GT.LNBUF/LNINT) GOTO 100
C
C Store column and row position and element value
C
                  IC(NP)=I
                  IR(NP)=J
                  IE(NP)=E
C
C If first defined element in column, increment defined column count
C
                  IF (NR(I,0).EQ.0.AND.NR(I,1).EQ.0) NC=NC+1
C
C Increment count of defined elements for column
C
                  NR(I,E)=NR(I,E)+1
               ENDIF
   20       CONTINUE
   30    CONTINUE
C
C See if any elements defined in current layer
C
         IF (NP.NE.0) THEN
C
C Fault overflow of buffer holding structuring element data
C
            IF (IOFF+1+4*NC+4*NP+4.GT.LNBUF/LNINT) GOTO 100
C
C Increment count of sub-iterations for structuring element data
C
            SE(7)=SE(7)+1
C
C Store count of defined columns
C
            IOFF=IOFF+1
            SE(IOFF)=NC
C
C Set up index for initial binary opcode
C
            IOP=0
C
C Scan through counts of defined elements for each column
C
            DO 60 I=1,NCOL
C
C Determine number of edge processing phases for this column
C (one phase for Hits and one for Misses)
C
               NE=0
               IF (NR(I,0).NE.0) NE=NE+1
               IF (NR(I,1).NE.0) NE=NE+1
C
C See if column contains any defined elements
C
               IF (NE.NE.0) THEN
C
C Determine column offset from origin
C
                  IX=CCOL-I
C
C Update structuring element X limits
C
                  SE(1)=MAX(SE(1), IX)
                  SE(2)=MAX(SE(2),-IX)
C
C Store column offset
C
                  IOFF=IOFF+1
                  SE(IOFF)=IX
C
C Store number of edge processing phases
C
                  IOFF=IOFF+1
                  SE(IOFF)=NE
C
C Run through each edge processing phase (E = 0 = Miss, E = 1 = Hit)
C
                  DO 50 E=0,1
C
C See if any Hits/Misses specified
C
                     IF (NR(I,E).NE.0) THEN
C
C Store source edge flag
C
                        IOFF=IOFF+1
                        SE(IOFF)=SFLAG(E)
C
C Store number of defined elements for Hits/Misses in this column
C
                        IOFF=IOFF+1
                        SE(IOFF)=NR(I,E)
C
C Record pointer value into data array for subsequent use
C
                        SO(I,E)=IOFF+1
C
C Store binary opcodes (first opcode differs from subsequent ones)
C
                        DO 40 J=1,NR(I,E)
C
C Store opcode flag
C
                           IOFF=IOFF+2
                           SE(IOFF)=IOP
C
C Change index for subsequent binary processing
C
                           IOP=1
   40                   CONTINUE
                     ENDIF
   50             CONTINUE
               ENDIF
   60       CONTINUE
C
C Store output edge flag
C
            IOFF=IOFF+1
            SE(IOFF)=OFLAG
C
C Store binary opcode for combining result of sub-iteration with input
C data
C
            IOFF=IOFF+1
            SE(IOFF)=ASOP
C
C Scan through data for current layer again, storing final results in
C data buffer in transposed (column-by-row) form
C
            DO 70 P=1,NP
C
C Fetch column and row position and element value
C
               I=IC(P)
               J=IR(P)
               E=IE(P)
C
C Determine row offset from origin
C
               IY=J-CROW
C
C Update structuring element Y limits
C
               SE(3)=MAX(SE(3),-IY)
               SE(4)=MAX(SE(4), IY)
C
C Store row offset
C
               SE(SO(I,E))=IY
C
C Fetch index for binary opcode
C
               IOP=SE(SO(I,E)+1)
C
C Store binary opcode according to whether initial or subsequent
C opcode and whether Hit or Miss specified
C
               SE(SO(I,E)+1)=HMOP(IOP,E)
C
C Increment pointer value into data array for this column and edge
C processing phase
C
               SO(I,E)=SO(I,E)+2
   70       CONTINUE
         ENDIF
   80 CONTINUE
C
C Fault undefined structuring element
C
      IF (SE(7).EQ.0) THEN
         ERROR=77
         IDMESS='No information in structuring element'
         GOTO 90
      ENDIF
C
C Call binary morphological processing routine
C
      IF (BMPROC(SE)) GOTO 90
C
   90 RETURN
C
C Fault overflow of row buffer holding structuring element data
C
  100 ERROR=77
      IDMESS='Structuring element too large to process'
      GOTO 90
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
