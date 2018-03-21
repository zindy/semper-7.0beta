C Semper 6 system module PRULAB
C
      LOGICAL FUNCTION PRULAB(LABNUM,LABINF,IDENT,P1,P2,P3)
C
C Add information into label index LABINF
C
      INCLUDE 'COMMON'
C
      INTEGER LABNUM,IDENT,P1,P2,P3,LABPTR
C
C Program index area, size of an index entry and maximum labels
C per buffer
C
      INTEGER LABINF(LNINDX),LABENT,LABMAX
      PARAMETER (LABENT=4,LABMAX=(LNINDX/LABENT)-1)
C
      IF (LABNUM .GE. LABMAX .AND. IDENT .NE. TIDEND) THEN
         ERROR = 102
         PRULAB = .TRUE.
      ELSE
C
C Decrement TXTPTR if ID is line (TXTPTR is next point to write)
C
         IF (IDENT .EQ. TIDLIN) P3 = P3 - 1
         LABPTR = LABNUM*LABENT
         LABINF(LABPTR+1) = IDENT
         LABINF(LABPTR+2) = P1
         LABINF(LABPTR+3) = P2
         LABINF(LABPTR+4) = P3
         LABNUM = LABNUM + 1
C
C Increment line number if ID is line
C
         IF (IDENT .EQ. TIDLIN) P1 = P1 + 1
         PRULAB = .FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
