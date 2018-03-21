C Semper VI processing module ROLL
C
      SUBROUTINE ROLL
C
C Rolls current lut until abandoned
C
      LOGICAL SEMLUT,FSLU61,ABANDN
C Packed name
      PARAMETER (NTIMES=-374)
C
      INCLUDE 'COMMON'
      PARAMETER (MAXINT=LNBUF/LNINT)
      INTEGER LUTBUF(MAXINT)
      EQUIVALENCE (RB1,LUTBUF)
C
C Limited number of cycles?
      MAXCY=IVAL(NTIMES)
      NCY=1
C
C Load lut data
      LUTN=CLUT
      IF (SEMLUT(1,LUTN,MODE,LUTBUF)) RETURN
C
C Begin outer cycle
C -----------------
    5 DO 90 NS=1,LUTLEN
C
C Load to hardware
      IF (FSLU61(2,LUTN,MODE,LUTBUF,ERROR)) RETURN
      IF (ABANDN(ERROR)) GOTO 40
C
C Roll lut one step
      NTABLS=1
      IF (MODE.NE.1) NTABLS=3
      DO 30 N=1,NTABLS
         I1=(N-1)*LUTLEN+1
         I2=I1+LUTLEN-2
         L1=LUTBUF(I1)
         DO 20 I=I1,I2
            LUTBUF(I)=LUTBUF(I+1)
   20    CONTINUE
         LUTBUF(I2)=L1
   30 CONTINUE
   90 CONTINUE
C
C Repeat outer cycle indefinitely?
      IF (MAXCY.LE.0) GOTO 5
      IF (NCY.LT.MAXCY) THEN
         NCY=NCY+1
         GOTO 5
      ENDIF
C
C Restore original lut on exit
   40 IF (SEMLUT(1,LUTN,MODE,LUTBUF)) RETURN
      IF (FSLU61(2,LUTN,MODE,LUTBUF,ERROR)) RETURN
      RETURN
      END
