C Semper 6 processing module BMPROC
C
      LOGICAL FUNCTION BMPROC(SE)
C
      INTEGER SE(*)
C
C Central morphological processing routine.  Depending on the size of
C the source picture and the size of the structuring element to apply,
C BMPMEM or BMPDSK is called to do the actual processing.  BMPMEM is
C called if the source picture is small enough to be processed entirely
C within the space in memory provided by Semper's row buffers.
C Otherwise, BMPDSK is called which stores the intermediate, bit-packed
C data in a temporary picture on disc.  The details of the morphological
C processing, as defined by a structuring element, are stored in the
C array SE.
C
      LOGICAL BMPMEM,BMPDSK
C
      INTEGER NCOL,NROW,N1,N2,NC,NP,NB
C
      INCLUDE 'COMMON'
C
      BMPROC = .TRUE.
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fetch Y limits for structuring element
C
      N1=SE(3)
      N2=SE(4)
C
C Determine operating parameters:
C
C     NC = number of 32-bit integers to hold one bit-packed row
C     NP = number of bit-packed rows that can be stored in a row buffer
C
      NC=(NCOL+31)/32
      NP=LNBUF/4/NC-(N1+N2)
C
C Fault too large combined size of source picture and structuring
C element (give up when more edge pixels are to be processed than
C the source data itself)
C
      IF (NP.LT.N1+N2) THEN
         ERROR=77
         IDMESS='Source picture and structuring '//
     +          'element too big to process'
         GOTO 10
      ENDIF
C
C Determine number of row buffers to contain source data (if more than
C one, data has to be stored in bit-packed form in a temporary picture)
C
      NB=(NROW+NP-1)/NP
C
C Recalculate NP so that data more equally shared between row buffers
C (otherwise, last buffer could be almost empty, wasting a lot of space)
C
      NP=(NROW+NB-1)/NB
C
C Call appropriate processing routine (memory or disc based)
C
      IF (NB.EQ.1) THEN
         IF (BMPMEM(RB1,RB5,SE,NC,N1,NP,N2)) GOTO 10
      ELSE
         IF (BMPDSK(RB1,RB5,SE,NC,N1,NP,N2,NB)) GOTO 10
      ENDIF
C
      BMPROC = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
