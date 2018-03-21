C Semper 6 processing module BOPCLO
C
      SUBROUTINE BOPCLO
C
C Carries out generalised binary opening/closing with arbitrary
C structuring elements.  BOPEN is exactly equivalent to the commands
C BERODE and BDILATE called in sequence with exactly the same keys and
C options.  Likewise, BCLOSE is exactly equivalent to using BDILATE
C and BERODE in sequence.  Any number of structuring elements can be
C applied in sequence by supplying them in successive layers of the
C multi-layer binary picture specified by means of the WITH key.  The
C sequence of structuring elements can be applied more than once by
C specifying a number of iterations greater than 1 with the TIMES key.
C Processing automatically terminates if the image does not change
C after one complete iteration.  The changes brought about by each
C complete iteration can be restricted to an arbitrary region by
C specifying a binary mask picture with the MASK key.  Only those
C pixels where the corresponding mask pixel is non-zero are allowed
C to change.  The options SOURCE/OUTPUT can be used to control edge
C effects.  Any undefined source or output pixel is set to the value
C given by the EDGE key if either the SOURCE or OUTPUT option is set.
C A source pixel is undefined if its position lies outside the source
C picture.  An output pixel is undefined if any contributing source
C pixel is undefined.  The contibuting source pixels positions are
C obtained by positioning the origin of the structuring element on
C the output pixel and selecting all source pixels where the
C structuring element is non-zero.  For dilation, the structuring
C element is first of all reflected about the origin.  By default,
C any undefined source pixels are omitted from the logical calculation
C used to obtain the output pixel value.  If P(i) represents the ith
C contributing source pixel value for a given output pixel, the result
C for dilation is obtained by logically ORing all the P(i), and the
C result for erosion is obtained by logically ANDing all the P(i).
C The parameters that define the sequence of logical operations for a
C particular sequence of structuring elements is stored in the array
C SE and passed to the routine BMPROC to carry them out.  The image
C is converted into bit_packed form, and if it is too large to be
C stored entirely within one row buffer, is stored in a temporary disc
C picture.  Storing the data in pit-packed form dramatically reduces
C the amount of data storage required (by a factor of 8 or 16) and the
C amount of data traffic.  It also makes it possible to process 16 or
C 32 pixels at the same time by carrying out the logical calculations
C on whole words of bit-packed data.  This gives a speed up of between
C 10 and 25 times.  Set against this is the overhead of converting to
C and from the bit-packed form.  For small (3 by 3) structuring
C elements, this takes the equivalent of applying 5 to 10 iterations of
C the structuring element, so the benefits only really become apparent
C for large numbers of iterations.  When Semper is able to support the
C bit-packed data form for storing binary pictures, the overhead of
C converting the data will disappear.
C
      LOGICAL SEMROW,BMPROC,OPT,CONOPT
      INTEGER IVAL !,IPACK
      REAL    VAL
C
      INTEGER I,J,K,P,IOFF,I1,I2,IOP1,IOP2,NC,NP,NT,NK,IEDGE,IX,IY
      INTEGER NCOL,NROW,NLAY,CCOL,CROW,SFLAG1,SFLAG2,OFLAG1,OFLAG2
C
      INCLUDE 'COMMON'
C
      INTEGER NR(LNBUF/LNINT)
      INTEGER SR(LNBUF/LNINT)
      INTEGER SO(LNBUF/LNINT)
      INTEGER IC(LNBUF/LNINT)
      INTEGER IR(LNBUF/LNINT)
      INTEGER SD(LNBUF/LNINT/2,2)
      INTEGER SE(LNBUF/LNINT)
C
      EQUIVALENCE (NR,RB1),(SR,RB2),(SO,RB3)
      EQUIVALENCE (IC,RB4),(IR,RB5),(SD,SE,RB6)
C
C See if command is BOPEN or BCLOSE, and set up array indices to use
C accordingly
C
      IF (VERB.EQ.3816) THEN
         I1=1
         I2=2
      ELSE
         I1=2
         I2=1
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
C
      IF (OPT(31021)) THEN
         SFLAG1=IEDGE
         SFLAG2=IEDGE
         OFLAG1=-1
         OFLAG2=-1
      ELSE IF (OPT(24860)) THEN
         SFLAG1=-1
         SFLAG2=-1
         OFLAG1=IEDGE
         OFLAG2=IEDGE
      ELSE
         SFLAG1=1
         SFLAG2=0
         OFLAG1=-1
         OFLAG2=-1
      ENDIF
C
C Initialise structuring element limits
C
      SE(1)=0
      SE(2)=0
      SE(3)=0
      SE(4)=0
C
C Set up number of processing phases
C
      SE(5)=2
C
C Fetch value for TIMES key (default = 1)
C
      NT=IVAL(-374)
C
C Fault zero or negative iteration count
C
      IF (NT.LT.1) THEN
         ERROR=3
         IDERR=-374
         GOTO 90
      ENDIF
C
C Initialise count of sub-iterations for structuring element data
C
      NK=0
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
      DO 70 K=1,NLAY
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
            NR(I)=0
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
C Check for defined (non-zero) element
C
               IF (SR(I).NE.0) THEN
C
C Increment count of defined elements
C
                  NP=NP+1
C
C Fault overflow of buffers holding column and row positions
C
                  IF (NP.GT.LNBUF/LNINT) GOTO 100
C
C Store column and row position
C
                  IC(NP)=I
                  IR(NP)=J
C
C If first defined element in column, increment defined column count
C
                  IF (NR(I).EQ.0) NC=NC+1
C
C Increment count of defined elements for column
C
                  NR(I)=NR(I)+1
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
            IF (IOFF+1+4*NC+2*NP+2.GT.LNBUF/LNINT/2) GOTO 100
C
C Increment count of sub-iterations for structuring element data
C
            NK=NK+1
C
C Store count of defined columns
C
            IOFF=IOFF+1
            SD(IOFF,1)=NC
            SD(IOFF,2)=NC
C
C Set up initial binary opcodes
C
            IOP1=12
            IOP2=12
C
C Scan through counts of defined elements for each column
C
            DO 50 I=1,NCOL
C
C See if column contains any defined elements
C
               IF (NR(I).NE.0) THEN
C
C Determine column offset from origin
C
                  IX=I-CCOL
C
C Update structuring element X limits
C
                  SE(1)=MAX(SE(1),ABS(IX))
                  SE(2)=MAX(SE(2),ABS(IX))
C
C Store column offsets
C
                  IOFF=IOFF+1
                  SD(IOFF,I1)=-IX
                  SD(IOFF,I2)= IX
C
C Store number of edge processing phases
C
                  IOFF=IOFF+1
                  SD(IOFF,1)=1
                  SD(IOFF,2)=1
C
C Store source edge flags
C
                  IOFF=IOFF+1
                  SD(IOFF,I1)=SFLAG1
                  SD(IOFF,I2)=SFLAG2
C
C Store number of defined elements in this column
C
                  IOFF=IOFF+1
                  SD(IOFF,1)=NR(I)
                  SD(IOFF,2)=NR(I)
C
C Record pointer value into data array for subsequent use
C
                  SO(I)=IOFF+1
C
C Store binary opcodes (first opcode differs from subsequent ones)
C
                  DO 40 J=1,NR(I)
C
C Store binary opcodes
C
                     IOFF=IOFF+2
                     SD(IOFF,I1)=IOP1
                     SD(IOFF,I2)=IOP2
C
C Change opcodes for subsequent binary processing
C
                     IOP1=8
                     IOP2=14
   40             CONTINUE
               ENDIF
   50       CONTINUE
C
C Store output edge flags
C
            IOFF=IOFF+1
            SD(IOFF,I1)=OFLAG1
            SD(IOFF,I2)=OFLAG2
C
C Set binary opcode for combining result of sub-iteration with source
C data (in this case, do nothing)
C
            IOFF=IOFF+1
            SD(IOFF,1)=10
            SD(IOFF,2)=10
C
C Scan through data for current layer again, storing final results in
C data buffer in transposed (column-by-row) form
C
            DO 60 P=1,NP
C
C Fetch column and row position
C
               I=IC(P)
               J=IR(P)
C
C Determine row offset from origin
C
               IY=J-CROW
C
C Update structuring element Y limits
C
               SE(3)=MAX(SE(3),ABS(IY))
               SE(4)=MAX(SE(4),ABS(IY))
C
C Store row offsets
C
               SD(SO(I),I1)= IY
               SD(SO(I),I2)=-IY
C
C Increment pointer value into data array for this column
C
               SO(I)=SO(I)+2
   60       CONTINUE
         ENDIF
   70 CONTINUE
C
C Fault undefined structuring element
C
      IF (NK.EQ.0) THEN
         ERROR=77
         IDMESS='No information in structuring element'
         GOTO 90
      ENDIF
C
C Store number of required iterations
C
      SD(6,1)=NT
      SD(6,2)=NT
C
C Store count of sub-iterations
C
      SD(7,1)=NK
      SD(7,2)=NK
C
C Merge two blocks of instructions
C
      DO 80 I=6,IOFF
         SE(IOFF+(I-5))=SD(I,2)
   80 CONTINUE
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
