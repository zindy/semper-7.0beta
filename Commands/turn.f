C Semper 6 processing module TURN
C
      SUBROUTINE TURN
C
C Rotates/flips source picture in LP1 in various ways.
C Source picture must not be 1-D, and for transformations requiring
C the picture to be transposed, it must also be square and its size
C a power of 2.
C
      INTEGER IVALPN
      LOGICAL OPT,SEMROW,SEMCP2,SEMCEN
C
      INCLUDE 'COMMON'
C
      REAL R
      INTEGER CCOL,CROW,NCOL,NCOL2,NROW
      INTEGER I,J,K,L,LPF,NC,IS,IT,IT1,IT2,IT3,IT4
      LOGICAL LTURN,LANT,LUPS,LOVER,LTRA,LXR
C
C Packed names
C
      INTEGER NANTIC,NUPSID,NOVER,NFROM
      PARAMETER (NANTIC=2180, NUPSID=-2260, NOVER=24885, NFROM=10335)
C
C Fault multi-layer source
C
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 80
      ENDIF
C
C Set options
C
      LANT=OPT(NANTIC)
      LUPS=OPT(NUPSID)
      LOVER=OPT(NOVER)
      LTURN=.NOT.(LANT.OR.LUPS.OR.LOVER)
      LANT=LANT.AND..NOT.LUPS
      LTRA=LTURN.OR.LANT
      LXR=LANT.OR.LUPS
      IF (LOVER) LXR=.NOT.LXR
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault incorrect source picture size
C
      IF (NCOL.EQ.1.OR.NROW.EQ.1.OR.
     +    LTRA.AND.(NCOL.NE.NROW.OR.SEMCP2(NCOL,1))) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 80
      ENDIF
      ERROR=0
C
C Transform output picture origin
C
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
      IF (LXR) CCOL=NCOL-CCOL+1
      IF (LTRA) THEN
         I=CCOL
         CCOL=CROW
         CROW=I
      ENDIF
      IF (LTURN) CCOL=NCOL-CCOL+1
      IF (LUPS) CROW=NROW-CROW+1
      IF (SEMCEN(LP2,CCOL,CROW,1)) GOTO 80
C
      LPF=LP1
      NCOL2=NCOL*2
      NC=NROW/2
      IF (LUPS.AND.LOVER) GOTO 60
      IS=NC
C
C Transposition pass
C
   10 IT=NC/IS
C
C Outer loops: over blocks, from 1 to IT
C
      L=1
C
C Over rows within block, from 1 to IS
C
   20 I=1
   30 IT1=I+2*IS*(L-1)
      IT2=IT1+IS
      IF (SEMROW(1,RB1,NFMCOM,IT1,1,LPF)) GOTO 80
      IF (SEMROW(1,RB2,NFMCOM,IT2,1,LPF)) GOTO 80
C
C Initial X reflection ?
C
      IF (IT.EQ.1.AND.LXR) CALL TURN2(RB1,RB2,NCOL2)
      IF (LTRA) THEN
C
C Number of blocks within a row
C
         DO 50 J=1,IT
C
C Transpose every such block
C
            DO 40 K=1,IS
               IT3=2*(K+2*IS*(J-1))
               IT4=IT3+2*IS
               R=RB2(IT3)
               RB2(IT3)=RB1(IT4)
               RB1(IT4)=R
               IT3=IT3-1
               IT4=IT4-1
               R=RB2(IT3)
               RB2(IT3)=RB1(IT4)
               RB1(IT4)=R
   40       CONTINUE
   50    CONTINUE
      ENDIF
C
C Final X reflection ?
C
      IF (IS.EQ.1.AND.LTURN) CALL TURN2(RB1,RB2,NCOL2)
C
C Rewrite rows
C
      IF (SEMROW(2,RB2,NFMCOM,IT2,1,LP2)) GOTO 80
      IF (SEMROW(2,RB1,NFMCOM,IT1,1,LP2)) GOTO 80
C
C End of outer loops
C
      I=I+1
      IF (I.LE.IS) GOTO 30
      L=L+1
      IF (L.LE.IT) GOTO 20
C
C Repeat transposition passes on 90 degree options
C
      LPF=LP2
      IF (LTRA) THEN
         IS=IS/2
         IF (IS.NE.0) GOTO 10
      ENDIF
C
C Y reflection ?
C
   60 IF (LUPS) THEN
         J = NROW
         DO 70 I=1,NROW/2
            IF (SEMROW(1,RB1,NFMCOM,I,1,LPF)) GOTO 80
            IF (SEMROW(1,RB2,NFMCOM,J,1,LPF)) GOTO 80
            IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) GOTO 80
            IF (SEMROW(2,RB2,NFMCOM,I,1,LP2)) GOTO 80
            J = J - 1
   70    CONTINUE
      ENDIF
C
   80 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module TURN2
C
      SUBROUTINE TURN2(RB1,RB2,NCOL2)
C
C Reflects rows RB1,RB2 (both complex form)
C
      INTEGER NCOL2
      REAL RB1(NCOL2),RB2(NCOL2)
C
      INTEGER I,J,K,NCOL
      REAL R
C
      NCOL=NCOL2/2
      DO 10 J=1,NCOL,2
         K=NCOL2-J
         R=RB1(J)
         RB1(J)=RB1(K)
         RB1(K)=R
         R=RB2(J)
         RB2(J)=RB2(K)
         RB2(K)=R
         I=J+1
         K=K+1
         R=RB1(I)
         RB1(I)=RB1(K)
         RB1(K)=R
         R=RB2(I)
         RB2(I)=RB2(K)
         RB2(K)=R
   10 CONTINUE
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
