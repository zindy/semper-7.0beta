C Semper 6 system module SEMROW
C
      LOGICAL FUNCTION SEMROW(IOP,A,AFORM,ROW,LAYER,LPN)
      INTEGER IOP,AFORM,ROW,LAYER,LPN
      REAL A(*)
C
C Reads and writes pictures rows from LPN
C
      LOGICAL DISC,ABANDN,FSRO61,FSRI61,SEMLNF,SEMMON
C
      INCLUDE 'COMMON'
C
      REAL BL,WH
      INTEGER*4 I4N,NC4
      INTEGER DEVICE,DGRAN,FORM,I,INFORM,IPART,IX,IY,JSAM,K,K1,K2,LNFORM
      INTEGER MEDIUM,N,NCOL,NLAY,NROW
C
      SEMROW=.FALSE.
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         WRITE (RECORD,10) IOP,AFORM,ROW,LAYER,LPN
         IF (SEMMON(RECORD,'SEMROW',2)) GOTO 90
      ENDIF
C
   10 FORMAT ('!-SEMROW: op',I2,' form',I2,' row',I5,' layer',I5,
     +        ' lpn',I3)
C
C ------ ------- ------
C
C
C Quick validation of LPN in case of user programming error
C
      IF (LPN .EQ. 0 .OR. LPN .GT. NLPS) THEN
         ERROR = 165
         IDERR = LPN
         GOTO 90
      ENDIF
C
      IF (WHEN(LPN) .EQ. 0) THEN
         ERROR = 166
         IDERR = LPN
         GOTO 90
      ENDIF
C
      DEVICE=DEVN(LPN)
      MEDIUM=MEDN(DEVICE)
      NCOL=NCOLS(LPN)
      NC4=NCOL
      NROW=NROWS(LPN)
      NLAY=NLAYS(LPN)
      FORM=FORMN(LPN)
      IF (ROW.LE.0.OR.ROW.GT.NROW) GOTO 70
      IF (LAYER.LE.0.OR.LAYER.GT.NLAY) THEN
         ERROR = 61
         GOTO 80
      ENDIF
C
C Check length overflow on conversion
C
      I = MAX0(FORM,AFORM)
      IF (SEMLNF(I,LNFORM)) GOTO 90
      IF (NC4*LNFORM.GT.LNBUF) THEN
         ERROR = 47
         GOTO 80
      ENDIF
C
C If writing, check WP..
C
      IF (IOP.EQ.2) THEN
         IF (WSTAT(LPN).LT.0) THEN
            ERROR = 41
            GOTO 80
         ENDIF
C
C ..record write
C
         WSTAT(LPN)=1
         REQDRR=.TRUE.
C
C ..and record lpn for possible selection
C
         OPLPN=LPN
      ENDIF
C
C Switch acc to device
C
      IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
C
C Disc/memory
C -----------
C Calculate data address for row, allowing for label
C
         IF (SEMLNF(FORM,LNFORM)) GOTO 90
         N=(NCOL*LNFORM-1)/LNBLK+1
         I4N=NROW
         I4N=ADDR(LPN)+LABSZE+((LAYER-1)*I4N+ROW-1)*N
         NC4=NCOL
         IF (DISC(IOP,DEVICE,NC4,A,I4N,AFORM,FORM)) GOTO 90
         SEMROW=ABANDN(ERROR)
      ELSE IF (MEDIUM .EQ. MEDDS) THEN
C
C Display
C -------
C
C If input, check unit sampling and readable
C
         JSAM=PXSAM(LPN)
C
         INFORM=AFORM
         IF (IOP.EQ.1) THEN
            IF (JSAM.NE.1) THEN
               ERROR = 33
               GOTO 80
            ENDIF
C
            IF (PROTN(DEVICE).NE.0) THEN
               ERROR = 15
               GOTO 90
            ENDIF
         ELSE
C
C Otherwise, check for row missed by sampling?
C
            IY=ROW-1
            IF ((IY/JSAM)*JSAM.NE.IY) GOTO 60
C
C If writing from non-complex to complex, convert form now
C
            IF (AFORM.NE.NFMCOM.AND.FORM.EQ.NFMCOM) THEN
               CALL CFORM(A,A,AFORM,FORM,NC4)
               INFORM=NFMCOM
            ENDIF
         ENDIF
C
         DGRAN=GPSIZ(DEVICE)
         IX=XLEFT(LPN)
         IY=YTOP(LPN)+(ROW-1)/JSAM*DGRAN
         K1=FRAME1(LPN)+(LAYER-1)
         K2=FRAME2(LPN)
         BL=GSMIN(LPN)
         WH=GSMAX(LPN)
C
C Transfer real part then imaginary - first find coords of lhs
C
         IPART=1
   20    IF (IOP.EQ.2) THEN
C
C Output
C
            IF (BL.NE.WH) THEN
               DO 50 K=K1,K2,NLAY
                  IF (FSRO61(A(IPART),1,NCOL,JSAM,INFORM,IX,IY,K,
     +                       BL,WH,0,ERROR)) GOTO 90
   50          CONTINUE
C
C Set flag to flush graphics buffer at the end of the current command
C
               REQFSF=.TRUE.
            ENDIF
         ELSE
C
C Input
C
            IF (FSRI61(A(IPART),NCOL,AFORM,IX,IY,K1,BL,WH,0,ERROR))
     +         GOTO 90
         ENDIF
C
         IF (FORM.EQ.NFMCOM .AND. INFORM.EQ.NFMCOM) THEN
            IF (IPART.NE.2) THEN
               IX=IX+((NCOL-1)/JSAM+1)*DGRAN
               IPART=2
               GOTO 20
            ENDIF
         ENDIF
C
         SEMROW=ABANDN(ERROR)
      ENDIF
C
   60 RETURN
C
C Errors
C
   70 ERROR=9
   80 IDERR=DEVN(LPN)*1000
      IF (PICN(LPN) .GT. 0) IDERR = IDERR+PICN(LPN)
   90 SEMROW=.TRUE.
      GOTO 60
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END

C Semper 6 system module SEMROW
C
      LOGICAL FUNCTION SEMROWI(IOP,A,AFORM,ROW,LAYER,LPN)
      INTEGER IOP,AFORM,ROW,LAYER,LPN
      INTEGER*4 A(*)
C
C Reads and writes pictures rows from LPN
C
      LOGICAL DISC,ABANDN,FSRO61,FSRI61,SEMLNF,SEMMON
C
      INCLUDE 'COMMON'
C
      REAL BL,WH
      INTEGER*4 I4N,NC4
      INTEGER DEVICE,DGRAN,FORM,I,INFORM,IPART,IX,IY,JSAM,K,K1,K2,LNFORM
      INTEGER MEDIUM,N,NCOL,NLAY,NROW
C
      SEMROW=.FALSE.
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         WRITE (RECORD,10) IOP,AFORM,ROW,LAYER,LPN
         IF (SEMMON(RECORD,'SEMROW',2)) GOTO 90
      ENDIF
C
   10 FORMAT ('!-SEMROW: op',I2,' form',I2,' row',I5,' layer',I5,
     +        ' lpn',I3)
C
C ------ ------- ------
C
C
C Quick validation of LPN in case of user programming error
C
      IF (LPN .EQ. 0 .OR. LPN .GT. NLPS) THEN
         ERROR = 165
         IDERR = LPN
         GOTO 90
      ENDIF
C
      IF (WHEN(LPN) .EQ. 0) THEN
         ERROR = 166
         IDERR = LPN
         GOTO 90
      ENDIF
C
      DEVICE=DEVN(LPN)
      MEDIUM=MEDN(DEVICE)
      NCOL=NCOLS(LPN)
      NC4=NCOL
      NROW=NROWS(LPN)
      NLAY=NLAYS(LPN)
      FORM=FORMN(LPN)
      IF (ROW.LE.0.OR.ROW.GT.NROW) GOTO 70
      IF (LAYER.LE.0.OR.LAYER.GT.NLAY) THEN
         ERROR = 61
         GOTO 80
      ENDIF
C
C Check length overflow on conversion
C
      I = MAX0(FORM,AFORM)
      IF (SEMLNF(I,LNFORM)) GOTO 90
      IF (NC4*LNFORM.GT.LNBUF) THEN
         ERROR = 47
         GOTO 80
      ENDIF
C
C If writing, check WP..
C
      IF (IOP.EQ.2) THEN
         IF (WSTAT(LPN).LT.0) THEN
            ERROR = 41
            GOTO 80
         ENDIF
C
C ..record write
C
         WSTAT(LPN)=1
         REQDRR=.TRUE.
C
C ..and record lpn for possible selection
C
         OPLPN=LPN
      ENDIF
C
C Switch acc to device
C
      IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
C
C Disc/memory
C -----------
C Calculate data address for row, allowing for label
C
         IF (SEMLNF(FORM,LNFORM)) GOTO 90
         N=(NCOL*LNFORM-1)/LNBLK+1
         I4N=NROW
         I4N=ADDR(LPN)+LABSZE+((LAYER-1)*I4N+ROW-1)*N
         NC4=NCOL
         IF (DISC(IOP,DEVICE,NC4,A,I4N,AFORM,FORM)) GOTO 90
         SEMROW=ABANDN(ERROR)
      ELSE IF (MEDIUM .EQ. MEDDS) THEN
C
C Display
C -------
C
C If input, check unit sampling and readable
C
         JSAM=PXSAM(LPN)
C
         INFORM=AFORM
         IF (IOP.EQ.1) THEN
            IF (JSAM.NE.1) THEN
               ERROR = 33
               GOTO 80
            ENDIF
C
            IF (PROTN(DEVICE).NE.0) THEN
               ERROR = 15
               GOTO 90
            ENDIF
         ELSE
C
C Otherwise, check for row missed by sampling?
C
            IY=ROW-1
            IF ((IY/JSAM)*JSAM.NE.IY) GOTO 60
C
C If writing from non-complex to complex, convert form now
C
            IF (AFORM.NE.NFMCOM.AND.FORM.EQ.NFMCOM) THEN
               CALL CFORM(A,A,AFORM,FORM,NC4)
               INFORM=NFMCOM
            ENDIF
         ENDIF
C
         DGRAN=GPSIZ(DEVICE)
         IX=XLEFT(LPN)
         IY=YTOP(LPN)+(ROW-1)/JSAM*DGRAN
         K1=FRAME1(LPN)+(LAYER-1)
         K2=FRAME2(LPN)
         BL=GSMIN(LPN)
         WH=GSMAX(LPN)
C
C Transfer real part then imaginary - first find coords of lhs
C
         IPART=1
   20    IF (IOP.EQ.2) THEN
C
C Output
C
            IF (BL.NE.WH) THEN
               DO 50 K=K1,K2,NLAY
                  IF (FSRO61(A(IPART),1,NCOL,JSAM,INFORM,IX,IY,K,
     +                       BL,WH,0,ERROR)) GOTO 90
   50          CONTINUE
C
C Set flag to flush graphics buffer at the end of the current command
C
               REQFSF=.TRUE.
            ENDIF
         ELSE
C
C Input
C
            IF (FSRI61(A(IPART),NCOL,AFORM,IX,IY,K1,BL,WH,0,ERROR))
     +         GOTO 90
         ENDIF
C
         IF (FORM.EQ.NFMCOM .AND. INFORM.EQ.NFMCOM) THEN
            IF (IPART.NE.2) THEN
               IX=IX+((NCOL-1)/JSAM+1)*DGRAN
               IPART=2
               GOTO 20
            ENDIF
         ENDIF
C
         SEMROW=ABANDN(ERROR)
      ENDIF
C
   60 RETURN
C
C Errors
C
   70 ERROR=9
   80 IDERR=DEVN(LPN)*1000
      IF (PICN(LPN) .GT. 0) IDERR = IDERR+PICN(LPN)
   90 SEMROW=.TRUE.
      GOTO 60
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
