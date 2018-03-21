C Semper 6 processing module SEPART
C
      SUBROUTINE SEPART
C
C Copies the layers in the source picture to a series of single layer
C output pictures.  Specified layers can be separated with the
C LAYER key
C
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL SEMOPN,SEMCLS,SEMROW,VARSET
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,J,K,K1,K2,NCOL,NFORM,NPIC,NROW
C
C Packed names
C
      INTEGER NFROM,NTO,NLAYER,NLA2
      PARAMETER (NFROM=10335,NTO=-601,NLAYER=19265,NLA2=19272)
C
C Determine whether LAYER key is set
C
      IF (VARSET(NLAYER)) THEN
C
C Fetch first layer number
C
         K1=IVAL(NLAYER)
C
C Determine second layer number
C
         IF (VARSET(NLA2)) THEN
            K2=IVAL(NLA2)
C
C Fault invalid layer number range
C
            IF (K1.LT.1.OR.K2.LT.K1.OR.K2.GT.NLAYS(LP1)) THEN
               ERROR=3
               IDERR=NLAYER
               GOTO 30
            ENDIF
         ELSE
            K2=K1
         ENDIF
C
C Otherwise, process all source layers
C
      ELSE
         K1=1
         K2=NLAYS(LP1)
      ENDIF
C
C Set up information about output picture
C
      NPIC=IVALPN(NTO)
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CLASS=CLASSN(LP1)
      NFORM=SEMFRM(FORMN(LP1))
C
C Process specified layers of source picture
C
      DO 20 K=K1,K2
C
C Open new output picture
C
         LP2=LP1
         IF (SEMOPN(2,NPIC,NCOL,NROW,1,CLASS,NFORM,LP2)) GOTO 30
C
C Copy source layer to output picture
C
         DO 10 J=1,NROW
            IF (SEMROW(1,RB4,NFORM,J,K,LP1)) GOTO 30
            IF (SEMROW(2,RB4,NFORM,J,1,LP2)) GOTO 30
   10    CONTINUE
C
C Close output picture
C
         IF (SEMCLS(LP2)) GOTO 30
C
C Increment output picture number
C
         NPIC=NPIC+1
   20 CONTINUE
C
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
