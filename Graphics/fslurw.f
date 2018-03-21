C Semper 6 graphics utility routine FSLURW
C
      LOGICAL FUNCTION FSLURW(IOP,LUT,MODE,BUFFER)
C
      INTEGER IOP,LUT,MODE,BUFFER(*)
C
C Transfer look-up table data from/to display device according to IOP:
C
C     IOP = 1, read data from framestore
C         = 2, write data to framestore
C
C The look-up table table number is specified by LUT and the type of
C look-up table is specified by MODE:
C
C     MODE = 1, monochrome look-up table
C          = 2, false colour  "      "
C          = 3, full colour   "      "
C
      LOGICAL FSLU61
C
      INCLUDE 'COMMON'
C
      FSLURW = FSLU61(IOP,LUT,MODE,BUFFER,ERROR)
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
