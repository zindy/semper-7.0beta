C Semper 6 processing module MRTRAN
C
      SUBROUTINE MRTRAN
C
C Transposes or rotates the source picture in LP1.  The source picture
C must be square and the size must consist only of the factors 2, 3, 4
C and 5.  The picture is transposed using a 'fast' multi-radix algorithm
C Rotation about the physical centre of the picture is achieved by
C shearing the picture in the X direction before and after transposing
C it.  The parts of the source picture that are sheared outside the
C picture limits are lost.  The corresponding blank areas in the output
C picture are set to the background value specified by the VALUE key.
C
      LOGICAL SEMOPN,SEMCEN,SEMROW,SEMFAC
      INTEGER SEMFRM,IVALPN
      REAL VAL
C
      INCLUDE 'COMMON'
C
      REAL ANGLE,C,S,U1,V1,U2,V2,XCEN,YCEN,X,XI,XJ,B
      INTEGER CLASS,FORM,FACTOR(10),BLS,BLS2,OBLS,BASE
      INTEGER NROW,NCOL,NPASS,I0,J0,ICEN,JCEN,INFORM,L,LE,LS,NPIC,J1,J
      INTEGER JBL1,JBL2,JBL3,JBL4,K,I1,I,IBL1,IBL2,IBL3,IBL4,IPASS
      LOGICAL BASE2,BASE3,BASE4,PRESH,PSTSH
C
C Packed names
C
      INTEGER NROTAT,NANGLE,NVALUE,NFROM,NTO
      PARAMETER (NROTAT=29420, NANGLE=2167, NVALUE=-3253)
      PARAMETER (NFROM=10335, NTO=-601)
C
C Fault multi-layer source picture
C
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 110
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault non-square or non-factorisable source picture size
C
      IF (NCOL.NE.NROW.OR.SEMFAC(NCOL,FACTOR,NPASS)) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 110
      ENDIF
C
C Process ANGLE key if verb is ROTATE
C
      IF (VERB.EQ.NROTAT) THEN
C
C Fetch ANGLE key
C
         ANGLE=VAL(NANGLE)
         C=COS(ANGLE)
         S=SIN(ANGLE)
C
C Fault angle within 30 degrees of 0/180 degrees
C
         IF (ABS(S).LT.0.5) THEN
            ERROR=3
            IDERR=NANGLE
            GOTO 110
         ENDIF
C
C Determine shear parameters
C
         U1=1.0/S
         V1=C/S
C
         U2=-S
         V2=-C
C
C Determine origin for transformation
C
         I0=1+NCOL/2
         J0=1+NROW/2
C
C Fetch source picture centre position
C
         XCEN=REAL(CCOLN(LP1))
         YCEN=REAL(CROWN(LP1))
C
C Shear X coordinate
C
         XI=REAL(I0)
         XJ=REAL(J0)
         XCEN=XI+(XCEN-XI-V1*(XJ-YCEN))/U1
C
C Transpose X and Y
C
         X=XCEN
         XCEN=YCEN
         YCEN=X
C
C Shear X coordinate again
C
         XCEN=XI+(XCEN-XI-V2*(XJ-YCEN))/U2
C
C Round to nearest pixel position
C
         ICEN=NINT(XCEN)
         JCEN=NINT(YCEN)
C
C Otherwise, just transpose X and Y for new centre position
C
      ELSE
         ICEN=CROWN(LP1)
         JCEN=CCOLN(LP1)
      ENDIF
C
C Determine intermediate data form
C
      FORM=SEMFRM(FORMN(LP1))
      IF (FORM.EQ.NFMCOM) THEN
         INFORM=NFMCOM
         L=2
      ELSE
         INFORM=NFMFP
         L=1
      ENDIF
C
C Open output picture
C
      NPIC=IVALPN(NTO)
      CLASS=CLASSN(LP1)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,CLASS,INFORM,LP2)) GOTO 110
C
C Fetch background pixel value
C
      B=VAL(NVALUE)
C
C Begin transpose passes
C
      BLS=NCOL
      DO 100 IPASS=1,NPASS
C
C Set flags for shear transformation if verb ROTATE and first or last pa
C
         PRESH=VERB.EQ.NROTAT.AND.IPASS.EQ.1
         PSTSH=VERB.EQ.NROTAT.AND.IPASS.EQ.NPASS
C
C If last pass, re-open output picture in final output form and store
C transformed centre position
C
         IF (IPASS.EQ.NPASS) THEN
            IF (SEMOPN(2,NPIC,NCOL,NROW,1,CLASS,FORM,LP2)) GOTO 110
            IF (SEMCEN(LP2,ICEN,JCEN,1)) GOTO 110
         ENDIF
C
         BASE=FACTOR(IPASS)
         BASE2=BASE.EQ.2
         BASE3=BASE.EQ.3
         BASE4=BASE.EQ.4
C
         OBLS=BLS
         BLS=BLS/BASE
         BLS2=L*BLS
C
C Pass through picture, transposing in blocks of BLS points square
C
         DO 90 J1=1,NROW,OBLS
            DO 80 J=J1,J1+BLS-1
C
C Fetch required rows, performing shear if rotating
C
               IF (SEMROW(1,RB1,INFORM,J,1,LP1)) GOTO 110
               IF (PRESH) CALL MRTRA2(B,RB1,RB6,NCOL,I0,J0,J,U1,V1,L)
               JBL1=J+BLS
               IF (SEMROW(1,RB2,INFORM,JBL1,1,LP1)) GOTO 110
               IF (PRESH) CALL MRTRA2(B,RB2,RB6,NCOL,I0,J0,JBL1,U1,V1,L)
               IF (BASE2) GOTO 10
               JBL2=JBL1+BLS
               IF (SEMROW(1,RB3,INFORM,JBL2,1,LP1)) GOTO 110
               IF (PRESH) CALL MRTRA2(B,RB3,RB6,NCOL,I0,J0,JBL2,U1,V1,L)
               IF (BASE3) GOTO 10
               JBL3=JBL2+BLS
               IF (SEMROW(1,RB4,INFORM,JBL3,1,LP1)) GOTO 110
               IF (PRESH) CALL MRTRA2(B,RB4,RB6,NCOL,I0,J0,JBL3,U1,V1,L)
               IF (BASE4) GOTO 10
               JBL4=JBL3+BLS
               IF (SEMROW(1,RB5,INFORM,JBL4,1,LP1)) GOTO 110
               IF (PRESH) CALL MRTRA2(B,RB5,RB6,NCOL,I0,J0,JBL4,U1,V1,L)
C
C Pass along rows, carrying out transpose from top left
C
   10          DO 40 K=1,L
                  LE = L*NCOL
                  LS = L*OBLS
                  DO 30 I1=K,LE,LS
                     DO 20 I=I1,I1+L*(BLS-1),L
C
C Base 2 code
C
                        IBL1=I+BLS2
                        X=RB2(I)
                        RB2(I)=RB1(IBL1)
                        RB1(IBL1)=X
                        IF (BASE2) GOTO 20
C
C Extra code for base 3
C
                        IBL2=IBL1+BLS2
                        X=RB3(I)
                        RB3(I)=RB1(IBL2)
                        RB1(IBL2)=X
                        X=RB3(IBL1)
                        RB3(IBL1)=RB2(IBL2)
                        RB2(IBL2)=X
                        IF (BASE3) GOTO 20
C
C Extra code for base 4
C
                        IBL3=IBL2+BLS2
                        X=RB4(I)
                        RB4(I)=RB1(IBL3)
                        RB1(IBL3)=X
                        X=RB4(IBL1)
                        RB4(IBL1)=RB2(IBL3)
                        RB2(IBL3)=X
                        X=RB4(IBL2)
                        RB4(IBL2)=RB3(IBL3)
                        RB3(IBL3)=X
                        IF (BASE4) GOTO 20
C
C Extra code for base 5
C
                        IBL4=IBL3+BLS2
                        X=RB5(I)
                        RB5(I)=RB1(IBL4)
                        RB1(IBL4)=X
                        X=RB5(IBL1)
                        RB5(IBL1)=RB2(IBL4)
                        RB2(IBL4)=X
                        X=RB5(IBL2)
                        RB5(IBL2)=RB3(IBL4)
                        RB3(IBL4)=X
                        X=RB5(IBL3)
                        RB5(IBL3)=RB4(IBL4)
                        RB4(IBL4)=X
   20                CONTINUE
   30             CONTINUE
   40          CONTINUE
C
C Return rows to store in reverse order
C
               IF (BASE4) GOTO 50
               IF (BASE3) GOTO 60
               IF (BASE2) GOTO 70
               IF (PSTSH) CALL MRTRA2(B,RB5,RB6,NCOL,I0,J0,JBL4,U2,V2,L)
               IF (SEMROW(2,RB5,INFORM,JBL4,1,LP2)) GOTO 110
   50          IF (PSTSH) CALL MRTRA2(B,RB4,RB6,NCOL,I0,J0,JBL3,U2,V2,L)
               IF (SEMROW(2,RB4,INFORM,JBL3,1,LP2)) GOTO 110
   60          IF (PSTSH) CALL MRTRA2(B,RB3,RB6,NCOL,I0,J0,JBL2,U2,V2,L)
               IF (SEMROW(2,RB3,INFORM,JBL2,1,LP2)) GOTO 110
   70          IF (PSTSH) CALL MRTRA2(B,RB2,RB6,NCOL,I0,J0,JBL1,U2,V2,L)
               IF (SEMROW(2,RB2,INFORM,JBL1,1,LP2)) GOTO 110
               IF (PSTSH) CALL MRTRA2(B,RB1,RB6,NCOL,I0,J0,J,U2,V2,L)
               IF (SEMROW(2,RB1,INFORM,J,1,LP2)) GOTO 110
   80       CONTINUE
   90    CONTINUE
C
C Switch input LP number to temporary output LP number
C
         LP1=LP2
  100 CONTINUE
C
  110 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module MRTRA2
C
      SUBROUTINE MRTRA2(B,B1,B2,NCOL,I0,J0,J,U,V,L)
C
C Exracts row 'in place' in row buffer B1, using spare buffer B2 for
C intermediate storage.  The following shear transformation is applied:
C    X = I0 + U*(I-I0) + V*(J0-J)
C Any pixels that transform outside the range 1 to NCOL are set to the
C background pixel value B.  The row data is real if L is 1, and
C complex if L is 2.
C
      INTEGER L,NCOL
      REAL B,B1(L*NCOL),B2(NCOL),U,V
      INTEGER I0,J0,J
C
      INTEGER K,IX,I
      REAL X
C
C Process real data (L=1) or complex data (L=2)
C
      DO 30 K=1,L
C
C Copy real or imaginary part of data to B2
C
         IX=1
         DO 10 I=K,L*NCOL,L
            B2(IX)=B1(I)
            IX=IX+1
   10    CONTINUE
C
C Determine initial transformed position
C
         X=(1.0-U)*REAL(I0)+V*REAL(J0-J)
C
C Extract data from B2 to B1 by interpolating between pixel values
C
         DO 20 I=K,L*NCOL,L
C
C Determine transformed position
C
            X=X+U
            IX=INT(X)
C
C Determine pixel value
C
            IF (IX.LT.1.OR.IX.GE.NCOL) THEN
               B1(I)=B
            ELSE
               B1(I)=B2(IX)+(X-REAL(IX))*(B2(IX+1)-B2(IX))
            ENDIF
   20    CONTINUE
   30 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
