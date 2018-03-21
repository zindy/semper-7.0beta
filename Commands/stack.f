C Semper 6 processing module STACK
C
      SUBROUTINE STACK
C
C Combines all layers of source pictures into one multi-layer
C output picture.  Specified layers can be stacked with the LAYER key.
C All the source pictures must have the same number of rows and columns.
C
      LOGICAL SEMOPN,SEMCLS,SEMROW,VARSET
      INTEGER SEMFRM,IVALPN,IVAL
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,N,N1,N2,LAYERS,LAYMIN,NCOL,NROW,NLAY,L,L1,L2
      INTEGER NPIC,NFORM,K,K1,K2,J
      LOGICAL LAYSET
C
C Packed names
      INTEGER NFROM,NFR2,NTO,NLAYER,NLA2
      PARAMETER (NFROM=10335, NFR2=10352, NTO=-601)
      PARAMETER (NLAYER=19265, NLA2=19272)
C
C Determine range of source picture numbers
      N1=IVALPN(NFROM)
      N2=IVALPN(NFR2)
C
C Fault invalid picture number range
      IF (N2.LT.N1) THEN
         ERROR=3
         IDERR=NFROM
         RETURN
      ENDIF
C
C Check for different row/column sizes, count total number of source
C picture layers and determine smallest number of source layers
      LAYERS=NLAYS(LP1)
      LAYMIN=NLAYS(LP1)
C
      DO 10 N=N1+1,N2
C
C Open next source picture
         IF (SEMOPN(1,N,NCOL,NROW,NLAY,CLASS,FORM,LP2)) RETURN
C
C Fault source picture with different row or column size
         IF (NCOL.NE.NCOLS(LP1).OR.NROW.NE.NROWS(LP1)) THEN
            ERROR=5
            IDERR=N
            RETURN
         ENDIF
C
C Add number of layers to total
         LAYERS=LAYERS+NLAY
C
C Update smallest number of source layers
         LAYMIN=MIN(LAYMIN,NLAY)
C
C Close source picture
         IF (SEMCLS(LP2)) RETURN
   10 CONTINUE
C
C Determine whether LAYER key is set
      LAYSET=VARSET(NLAYER)
C
C Determine number of output layers from LAYER key (if set)
      IF (LAYSET) THEN
C
C Fetch first layer number
         L1=IVAL(NLAYER)
C
C Determine second layer number
         IF (VARSET(NLA2)) THEN
            L2=IVAL(NLA2)
C
C Fault invalid layer number range
            IF (L1.LT.1.OR.L2.LT.L1.OR.L2.GT.LAYMIN) THEN
               ERROR=3
               IDERR=NLAYER
               RETURN
            ENDIF
         ELSE
            L2=L1
         ENDIF
C
C Number of output layers
         LAYERS=(N2-N1+1)*(L2-L1+1)
      ENDIF
C
C Set up information about output picture
      NPIC=IVALPN(NTO)
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CLASS=CLASSN(LP1)
      NFORM=SEMFRM(FORMN(LP1))
C
C Fault overlapping source and output picture numbers
      IF (NPIC.GE.N1.AND.NPIC.LE.N2) THEN
         ERROR=59
         RETURN
      ENDIF
C
C Open new output picture
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,LAYERS,CLASS,NFORM,LP2)) RETURN
C
C Process all source pictures
      L=1
      DO 40 N=N1,N2
C
C Open next source picture
         IF (SEMOPN(1,N,NCOL,NROW,NLAY,CLASS,FORM,LP1)) RETURN
C
C Determine range of source layers numbers to process
         IF (LAYSET) THEN
            K1=L1
            K2=L2
         ELSE
            K1=1
            K2=NLAY
         ENDIF
C
C Process source picture layers
         DO 30 K=K1,K2
C
C Copy source layer to output layer
            DO 20 J=1,NROW
               IF (SEMROW(1,RB4,NFORM,J,K,LP1)) RETURN
               IF (SEMROW(2,RB4,NFORM,J,L,LP2)) RETURN
   20       CONTINUE
C
C Increment output layer number
            L=L+1
   30    CONTINUE
C
C Close source picture
         IF (SEMCLS(LP1)) RETURN
   40 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
