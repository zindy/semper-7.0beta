C Semper system routine SEMCEN
C
      LOGICAL FUNCTION SEMCEN(LPN,CCOL,CROW,CLAY)
      INTEGER LPN,CCOL,CROW,CLAY
C
C Update centre position for picture LPN
C
      LOGICAL SEMLAB,SEMDPD
C
      INCLUDE 'COMMON'
C
      INTEGER I,LABEL(256)
      EQUIVALENCE (LABEL,RB1)
C
      SEMCEN=.TRUE.
C
C Fault centre position outside picture limits
C
      IF (CCOL.LT.1.OR.CCOL.GT.NCOLS(LPN).OR.
     +    CROW.LT.1.OR.CROW.GT.NROWS(LPN).OR.
     +    CLAY.LT.1.OR.CLAY.GT.NLAYS(LPN)) THEN
         ERROR = 63
         IDERR = (1000*DEVN(LPN)) + PICN(LPN)
         GOTO 10
      ENDIF
C
C Fetch picture label
C
      IF (SEMLAB(1,LABEL,LPN)) GOTO 10
C
C Update picture label
C
      LABEL(LBCC1) = CCOL/256
      LABEL(LBCC2) = CCOL - 256*LABEL(LBCC1)
C
      LABEL(LBCR1) = CROW/256
      LABEL(LBCR2) = CROW - 256*LABEL(LBCR1)
C
      LABEL(LBCL1) = CLAY/256
      LABEL(LBCL2) = CLAY - 256*LABEL(LBCL1)
C
C Return picture label to disc
C
      IF (SEMLAB(2,LABEL,LPN)) GOTO 10
C
C If display, update DPD
C
      IF (MEDN(DEVN(LPN)) .EQ. MEDDS) THEN
         IF (SEMDPD(1,PICN(LPN))) GOTO 10
         I = CCOL-CCOLN(LPN)
         DPLEF = DPLEF-I
         DPRIG = DPRIG-I
         DPMB = DPMA*I + DPMB
         I = CROWN(LPN)-CROW
         DPTOP = DPTOP-I
         DPBOT = DPBOT-I
         DPMB2 = DPMA2*I + DPMB2
         IF (SEMDPD(2,PICN(LPN))) GOTO 10
      ENDIF
C
C Update picture control block
C
      CCOLN(LPN) = CCOL
      CROWN(LPN) = CROW
      CLAYN(LPN) = CLAY
C
C Normal return
C
      SEMCEN = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
