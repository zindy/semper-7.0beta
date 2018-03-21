C Semper 6 processing module BMMAP
C
      SUBROUTINE BMMAP
C
C Carries out morphological processing using 3 by 3 neighbourhood
C mapping tables supplied in the picture specified by means of the
C WITH key.  The mapping tables are stored in bit-packed form in
C one row buffer, for ease and speed of access.  This puts an upper
C limit on the number of mapping tables that can be simultaneously
C applied.  Each of the mapping tables is applied in turn, making
C one complete iteration of the morphological transform.  More than
C one iteration (the default) can be specified by setting the TIMES
C key.  If the TIMES key is set to zero, this specifies an infinite
C number of iterations.  If a given iteration produces no change in
C the output result, processing is stopped automatically.  Changes
C brought about by a given morphological transform can be restricted
C to an arbitrary mask region by specifying a binary mask image with
C the MASK key.  By default, pixels round the edges of the image are
C replicated outwards to complete the data for 3 by 3 neighbourhoods
C that extend outside the limits of the source image.  With the option
C SOURCE or OUTPUT, you have the alternative to specify a value for
C undefined source or output pixels.  The actual value is specified
C with the EDGE key.
C
C BMMAP works by generating a 9-bit neigbourhood index for each source
C pixel and using this index to obtain the output result for the pixel
C in the current mapping table.  Since the result is a binary value,
C the result depends only on whether the mapping table entry is zero
C or non-zero.  The ordering of pixels in a 3 by 3 neighbourhood is as
C follows:
C
C        0 3 6
C        1 4 7
C        2 5 8
C
C where the numbers define bit positions in the neighbourhood index
C with 0 corresponding to the least significant bit.  Generating the
C neighbourhood index value is equivalent to applying the following
C linear convolution kernel:
C
C        2^0 2^3 2^6       1  8  64
C        2^1 2^4 2^7   =   2 16 128
C        2^2 2^5 2^8       4 32 256
C
C The bits are ordered in this way so that 6 of the 9 bits for the index
C value for the next (righthand) pixel is obtained by shifting the index
C value 3 bit positions to the right (dividing by 2^3 = 8).  This leaves
C just 3 new bits to be masked in to obtain the next index value.  This
C process can be run through any number of picture rows, if the data for
C each row is extended by one pixel to left and right and is stored
C contiguously in memory.  The result will be shifted one column to the
C right with single, undefined (i.e. duff) values to left and right
C which are reset on the next iteration or discarded on output.
C
      LOGICAL CONOPT,OPT,SEMROW,BMMMEM,BMMDSK
      INTEGER IPACK,IVAL,IVALPN
C
      INTEGER FLAGS(3),NM,NC,NP,NB,NCOL,NROW,NLAY,IEDGE,J,K
C
      INCLUDE 'COMMON'
C
      INTEGER MAXMAP
      PARAMETER ( MAXMAP = (LNBUF-512*LNINT)/4/16/2 )
C
      INTEGER   MAP(0:LNBUF/LNINT)
      INTEGER*4 BITM(16,MAXMAP,2)
C
      EQUIVALENCE (MAP,RB6),(BITM,MAP(512))
C
C Fetch map picture size
C
      NCOL=NCOLS(LP3)
      NROW=NROWS(LP3)
      NLAY=NLAYS(LP3)
C
C Fault bad size for map picture
C
      IF (NCOL.NE.512.OR.NROW.GT.MAXMAP.OR.NLAY.GT.1) THEN
         ERROR=5
         IDERR=IVALPN(-5181)
         GOTO 30
      ENDIF
C
C Store mapping tables in single buffer array in bit-packed form
C
      DO 20 K=1,NLAY
         DO 10 J=1,NROW
            IF (SEMROW(1,MAP,NFMINT,J,K,LP3)) GOTO 30
            CALL BFORM(1,BITM(1,J,K),MAP,NFMINT,512)
   10    CONTINUE
   20 CONTINUE
C
C Fetch value of TIMES key
C
      FLAGS(1)=IVAL(-374)
C
C Zero or negative values imply indefinite processing
C
      IF (FLAGS(1).LT.1) FLAGS(1)=32766
C
C Fault conflicting options SOURCE and OUTPUT
C
      IF (CONOPT(31021,24860)) GOTO 30
C
C Fetch value of EDGE key and convert it to binary result
C
      IF (IVAL(8167).EQ.0) THEN
         IEDGE=0
      ELSE
         IEDGE=1
      ENDIF
C
C Set up source edge processing flag
C
      IF (OPT(31021)) THEN
         FLAGS(2)=IEDGE
      ELSE
         FLAGS(2)=-1
      ENDIF
C
C Set up output edge processing flag
C
      IF (OPT(24860)) THEN
         FLAGS(3)=IEDGE
      ELSE
         FLAGS(3)=-1
      ENDIF
C
C Set up number of sub-iterations (mapping tables)
C
      NM=NROW
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Determine operating parameters:
C
C     NC = number of 32-bit integers to hold one bit-packed row
C     NP = number of bit-packed rows that can be stored in a row buffer
C
      NC=(NCOL+2+31)/32
      NP=LNBUF/4/NC-2
C
C Fault too large size of source picture (give up when more edge pixels
C are to be processed than the source data itself)
C
      IF (NP.LT.2) THEN
         ERROR=77
         IDMESS='Source picture too large to process'
         GOTO 30
      ENDIF
C
C Determine number of row buffers to contain source data (if more than
C one, data has to be stored in bit-packed form in a temporary picture)
C
      NB=(NROW+NP-1)/NP
C
C Recalculate NP so that data is more equally shared between row buffers
C (otherwise, last buffer could be almost empty, wasting a lot of space)
C
      NP=(NROW+NB-1)/NB
C
C Call appropriate processing routine (memory or disc based)
C
      IF (NB.EQ.1) THEN
         IF (BMMMEM(FLAGS,MAP,BITM,RB1,RB5,NM,NC,NP)) GOTO 30
      ELSE
         IF (BMMDSK(FLAGS,MAP,BITM,RB1,RB5,NM,NC,NP,NB)) GOTO 30
      ENDIF
C
   30 RETURN
C
      END
