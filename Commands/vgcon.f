C Semper 6 processing module VGCON
C
      SUBROUTINE VGCON
C
C Displays a picture as a contour map using FSLN61 calls
C
      LOGICAL DSPSUB,MAGSUB,SEMROW,OPTNO,FSLN61,FSINIT,FSBORD
      INTEGER IVAL,IVALPN
C
      INCLUDE 'COMMON'
C
      LOGICAL TLP,TRP,BLP,BRP,IMPART
      INTEGER DGRAN,DGRAN1,DGRAN2,PND,IFS
      INTEGER MAGN,IX,IY,INFORM,M1,M2,N1,N2,NLAY,MAXPXL,JS,IS,NL
      INTEGER ILEFT,K1,K2,IN1,IN2,J1,J,I1,I,I2,I3,J2,J3,L,K,LEVELS
      REAL PRANGE,YSC,TL,BL,TR,BR,A
C
C Packed names
C
      INTEGER NLEVEL,NFROM,NTIMES,NLETTE,NBORDE
      PARAMETER (NLEVEL=19422,NFROM=10335,NTIMES=-374)
      PARAMETER (NLETTE=19420,NBORDE=3818)
C
      LEVELS=IVAL(NLEVEL)
      IF (LEVELS.LE.0) THEN
         ERROR=3
         IDERR=NLEVEL
         GOTO 80
      ENDIF
C
C Call DSPSUB - establishes range, opens display and adds text
C
      MAGN=IVAL(NTIMES)
      IF (DSPSUB(IX,PND,0,OPTNO(NLETTE),INFORM,MAGN,M1,M2,N1,N2))
     +   GOTO 80
      NLAY=NLAYS(LP2)
C
C Establish subregion size and form
C
      IF (NROWS(LP2).EQ.1) THEN
         IDERR=IVALPN(NFROM)
         ERROR=5
         GOTO 80
      ENDIF
      MAXPXL=LNBUF/LNREAL/2
      IF (INFORM.NE.3) THEN
         MAXPXL=2*MAXPXL
         INFORM=2
      ENDIF
C
C Establish sampling
C
      JS=PXSAM(LP2)
      IS=JS
C
C Accomodate fp/complex form switch
C
      IF (INFORM.EQ.3) THEN
         IS=2*IS
         M1=2*M1-1
         M2=2*M2
      ENDIF
C
C Find framestore device number
C
      IFS=DEVN(LP2)
C
C Establish lateral and grey scaling
C
      DGRAN=GPSIZ(IFS)
      DGRAN1=DGRAN/2
      DGRAN2=(DGRAN-1)/2
      PRANGE=VMAX-VMIN
      YSC=PRANGE/(LEVELS+1)
C
C Loop over layers
C
      NL=SMGI3
C
C Set display position for real part
C
   10 IMPART=.FALSE.
      ILEFT=XLEFT(LP2)
      K1=FRAME1(LP2)+NL-SMGI3
      K2=FRAME2(LP2)
C
C Prepare two buffer pointers
C
   20 IN1=1
      IN2=LNBUF/LNREAL+21
C
C Loop over picture rows
C
      J1=YTOP(LP2)
      DO 70 J=N1,N2,JS
         IF (MAGN.GT.1) THEN
            IF (MAGSUB(MAGN,RB1(IN1),J,NL,.TRUE.)) GOTO 80
         ELSE
            IF (SEMROW(1,RB1(IN1),INFORM,J,NL,LP1)) GOTO 80
         ENDIF
         IF (J.NE.N1) THEN
            I1=ILEFT-1
            J1=J1+DGRAN
            DO 60 I=M1,M2,IS
               TR=RB1(IN2+I-1)
               BR=RB1(IN1+I-1)
               IF (I.EQ.M1) GOTO 50
               I2=I1+DGRAN2
               I3=I1+DGRAN
               J2=J1-DGRAN1
               J3=J1-DGRAN
               A=VMIN
               DO 40 L=1,LEVELS
                  A=A+YSC
                  TLP=TL.GE.A
                  TRP=TR.GE.A
                  BLP=BL.GE.A
                  BRP=BR.GE.A
                  IF (.NOT.TLP) THEN
                     TRP=.NOT.TRP
                     BLP=.NOT.BLP
                     BRP=.NOT.BRP
                  ENDIF
C
                  IF (OVLIND(IFS)) THEN
                     DO 30 K=K1,K2,NLAY
                        IF (TRP) THEN
                           IF (.NOT.BLP) THEN
                              IX=I1
                              IY=J2
                              IF (BRP) THEN
                                IF (FSLN61(I2,J1,IX,IY,K,ERROR)) GOTO 80
                              ELSE
                                IF (FSLN61(I3,J2,IX,IY,K,ERROR)) GOTO 80
                              ENDIF
                           ELSE IF (.NOT.BRP) THEN
                              IX=I2
                              IY=J1
                              IF (FSLN61(I3,J2,IX,IY,K,ERROR)) GOTO 80
                           ELSE
                              IF (.NOT.TLP) GOTO 50
                           ENDIF
                        ELSE
                           IX=I2
                           IY=J3
                           IF (BLP) THEN
                              IF (BRP) THEN
                                IF (FSLN61(I3,J2,IX,IY,K,ERROR)) GOTO 80
                              ELSE
                                IF (FSLN61(I2,J1,IX,IY,K,ERROR)) GOTO 80
                              ENDIF
                           ELSE
                              IF (BRP) THEN
                                IF (FSLN61(I3,J2,I1,J2,K,ERROR)) GOTO 80
                                IF (FSLN61(I2,J1,IX,IY,K,ERROR)) GOTO 80
                              ELSE
                                IF (FSLN61(IX,IY,I1,J2,K,ERROR)) GOTO 80
                              ENDIF
                           ENDIF
                        ENDIF
   30                CONTINUE
                  ELSE
                     IF (TRP) THEN
                        IF (.NOT.BLP) THEN
                           IX=I1
                           IY=J2
                           IF (BRP) THEN
                              IF (FSLN61(I2,J1,IX,IY,0,ERROR)) GOTO 80
                           ELSE
                              IF (FSLN61(I3,J2,IX,IY,0,ERROR)) GOTO 80
                           ENDIF
                        ELSE IF (.NOT.BRP) THEN
                           IX=I2
                           IY=J1
                           IF (FSLN61(I3,J2,IX,IY,0,ERROR)) GOTO 80
                        ELSE
                           IF (.NOT.TLP) GOTO 50
                        ENDIF
                     ELSE
                        IX=I2
                        IY=J3
                        IF (BLP) THEN
                           IF (BRP) THEN
                              IF (FSLN61(I3,J2,IX,IY,0,ERROR)) GOTO 80
                           ELSE
                              IF (FSLN61(I2,J1,IX,IY,0,ERROR)) GOTO 80
                           ENDIF
                        ELSE
                           IF (BRP) THEN
                              IF (FSLN61(I3,J2,I1,J2,0,ERROR)) GOTO 80
                              IF (FSLN61(I2,J1,IX,IY,0,ERROR)) GOTO 80
                           ELSE
                              IF (FSLN61(IX,IY,I1,J2,0,ERROR)) GOTO 80
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
                  REQFSF=.TRUE.
   40          CONTINUE
C
   50          CONTINUE
               TL=TR
               BL=BR
               I1=I1+DGRAN
   60       CONTINUE
         ENDIF
C
C Swap buffer pointers
C
         I=IN1
         IN1=IN2
         IN2=I
   70 CONTINUE
C
C Imaginary part too?
C
      IF (INFORM.EQ.3 .AND. .NOT.IMPART) THEN
         IMPART=.TRUE.
         M1=M1+1
         ILEFT=ILEFT+DPIMO
         GOTO 20
      ENDIF
C
C Next layer?
C
      NL=NL+1
      IF (NL.LE.SMGI6) GOTO 10
C
C Frame result
C
      IF (.NOT.OPTNO(NBORDE)) THEN
         IF (FSINIT(3,PND)) GOTO 80
         IF (FSBORD()) GOTO 80
      ENDIF
C
   80 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
