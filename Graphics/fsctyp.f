C Semper 6 graphics utility routine FSCTYP
C
      LOGICAL FUNCTION FSCTYP(ITYPE)
C
      INTEGER ITYPE
C
C Selects appearance of cursor (cursor type).  Four cursor types are
C defined.  Other numbers can be ignored or mapped onto other cursor
C patterns.  The four required cursor types are:
C
C   ITYPE = 0, invisible/hidden cursor
C         = 1, small cross (with blank central pixel)
C         = 2, medium cross ( "     "      "      "  )
C         = 3, large cross ( "     "      "      "  )
C         = 4, full screen/window cross-hair
C
      LOGICAL FSCT61
C
      INCLUDE 'COMMON'
C
      FSCTYP = FSCT61(ITYPE,ERROR)
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
