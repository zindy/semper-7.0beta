C Semper 6 subsidiary module GRAPH
C
      LOGICAL FUNCTION GRAPH(MODE,B,M1,M2,IS,IX,IY,FR1,FR2,DFR,
     +                       BL,WH,NHI,IYO)
C
C Scales and plots a 1-D array from elems M1,M2 by IS of real B;
C mode 1 gives connected curve, i.e. graph
C mode 2 gives vertical lines, i.e. histogram
C
      REAL B(256),BL,WH
      INTEGER MODE,M1,M2,IS,IX,IY,FR1,FR2,DFR,NHI,IYO,IFS
C
      LOGICAL FSLN61
C
      INCLUDE 'COMMON'
C
      REAL A,DBL,DWH,SC
      INTEGER FR,I,IBL,IX1,IX2,IY1,IY2,M
C
      M=(M2-M1)/IS+1
      IX1=IX
      IX2=IX1+M-1
      DWH=IY
      IBL=IY+NHI-1
      DBL=IBL
C
C NOTE: Assumes current framestore is the one being written to...
C
      IFS = FS
C
C Draw X axis?
C
      SC=(NHI-1)/(WH-BL)
      IF (WH*BL.LE.0.) THEN
         IY1=DBL+SC*BL+.5
         IF (OVLIND(IFS)) THEN
            DO 10 FR = FR1,FR2,DFR
               IF (FSLN61(IX,IY1,IX2,IY1,FR,ERROR)) GOTO 70
   10       CONTINUE
         ELSE
            IF (FSLN61(IX,IY1,IX2,IY1,0,ERROR)) GOTO 70
         ENDIF
      ENDIF
C
C Draw Y axis?
C
      IF (IYO.GE.IX .AND. IYO.LE.IX2) THEN
         IF (OVLIND(IFS)) THEN
            DO 20 FR = FR1,FR2,DFR
               IF (FSLN61(IYO,IY,IYO,IBL,FR,ERROR)) GOTO 70
   20       CONTINUE
         ELSE
            IF (FSLN61(IYO,IY,IYO,IBL,0,ERROR)) GOTO 70
         ENDIF
      ENDIF
C
C Draw curve
C
      DO 50 I=M1,M2,IS
         A=(BL-B(I))*SC+DBL+.5
         IF (A.LT.DWH) A=DWH
         IF (A.GT.DBL) A=DBL
         IY1=A
         IF (IX1.NE.IX2 .OR. IY1.NE.IY2) THEN
C
            IF (I.NE.M1) THEN
               IF (MODE.EQ.2) THEN
C
C Vertical line mode
C
                  IF (OVLIND(IFS)) THEN
                     DO 30 FR = FR1,FR2,DFR
                        IF (FSLN61(IX2,IBL,IX2,IY2,FR,ERROR)) GOTO 70
   30                CONTINUE
                  ELSE
                     IF (FSLN61(IX2,IBL,IX2,IY2,0,ERROR)) GOTO 70
                  ENDIF
               ELSE
C
C Connected curve mode
C
                  IF (OVLIND(IFS)) THEN
                     DO 40 FR = FR1,FR2,DFR
                        IF (FSLN61(IX1,IY1,IX2,IY2,FR,ERROR)) GOTO 70
   40                CONTINUE
                  ELSE
                     IF (FSLN61(IX1,IY1,IX2,IY2,0,ERROR)) GOTO 70
                  ENDIF
               ENDIF
            ENDIF
C
            IY2=IY1
            IX2=IX1
         ENDIF
C
         IX1=IX1+1
   50 CONTINUE
C
C Normal return
C
      GRAPH=.FALSE.
   60 RETURN
C
C Error return
C
   70 GRAPH=.TRUE.
      GOTO 60
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
