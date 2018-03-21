C Semper 6 subsidiary module INVFT2
C
      LOGICAL FUNCTION INVFT2(ITO,CLASS,MSQ,ZEROC,RNORM)
C
      INTEGER ITO,CLASS
      LOGICAL MSQ,ZEROC
      REAL    RNORM
C
C Effects 2-D inverse FT from LP1 to ITO:
C
C [in] integer ITO: output picture number; returned open as LP2;
C        form is set by Semper option
C [in] integer CLASS: output picture class
C [in] logical MSQ: take mod squared before xforming
C [in] logical ZEROC: zero central pixel before xforming
C [in] real RNORM: normalisation factor control:
C        if +: xform multiplied by RNORM (for XCF)
C        if 0: xform divided by product of sizes (normal use)
C        if -: xform divided by final central value (for ACF)
C
C This module simply checks sizes and organises intermediate/output
C pictures for DFTM/DFTXB2 or COLFT/COLFT2
C
C Local declarations
C
      INTEGER SEMFRM
      LOGICAL SEMTFC,SEMFAC,SEMCP2,SEMOPN,SEMCLS
      LOGICAL DFTM,DFTXB2,COLFT,COLFT2
C
      REAL    RN
      INTEGER*4 NC,NR,NCBLK,NCCOM,FFTBUF,WCOFF,WROFF,MPOFF
      INTEGER BASE(9),N(2),FORM,NCOL,NROW,NF,LPN,NCOP,NROP
      LOGICAL HPL
C
C Global declarations
C
      INCLUDE 'COMMON'
C
      REAL CBS(2,0:LNSBUF/LNCOMP-1)
C
      EQUIVALENCE (CBS,RBS)
C
      INTEGER*4 N41,N44,N45,N46,N4INT,N4COMP,N4BLK
      PARAMETER ( N41=1, N44=4, N45=5, N46=6 )
      PARAMETER ( N4INT=LNINT, N4COMP=LNCOMP, N4BLK=LNBLK )
C
      INVFT2=.TRUE.
C
C Determine whether source picture is full or half plane fourier
C
      IF (SEMTFC(LP1,HPL)) GOTO 10
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Determine output picture size
C
      IF (HPL) THEN
         NCOP=2*(NCOL-1)
      ELSE
         NCOP=NCOL
      ENDIF
C
      NROP=NROW
C
C Determine output picture form
C
      IF (HPL) THEN
         FORM=SEMFRM(NFMFP)
      ELSE
         FORM=SEMFRM(NFMCOM)
      ENDIF
C
C Determine data buffer requirements for multi-radix approach
C
      NC=NCOL
      NR=NROW
C
C Determine column size rounded up to next logical disc block
C
C     NCBLK=(NC/(LNBLK/LNCOMP))*(LNBLK/LNCOMP)
C     IF (NCBLK.LT.NC) NCBLK=NCBLK+(LNBLK/LNCOMP)
C
      NCBLK=(NC/(N4BLK/N4COMP))*(N4BLK/N4COMP)
      IF (NCBLK.LT.NC) NCBLK=NCBLK+(N4BLK/N4COMP)
C
C Determine X re-ordering table size rounded up to next multiple of
C complex data
C
C     NCCOM=NC/(LNCOMP/LNINT)
C     IF (NCCOM*(LNCOMP/LNINT).LT.NC) NCCOM=NCCOM+1
C
      NCCOM=NC/(N4COMP/N4INT)
      IF (NCCOM*(N4COMP/N4INT).LT.NC) NCCOM=NCCOM+N41
C
C Determine total amount of buffer space required in bytes
C
C     FFTBUF=(6*NCBLK+NC*4/5+NR*4/5+NCCOM)*LNCOMP
C
      FFTBUF=(N46*NCBLK+NC*N44/N45+NR*N44/N45+NCCOM)*N4COMP
C
C See if required buffer space exceeds available buffer space
C If so, try radix 2 approach
C
      IF (FFTBUF.GT.LNSBUF) THEN
C
C Fault output picture size if not a power of 2
C
         IF (SEMCP2(NCOP,NROP)) GOTO 20
C
C Open complex intermediate picture
C
         LPN=LP1
         IF (SEMOPN(2,ITO,NCOL,NROW,1,CLASS,NFMCOM,LPN)) GOTO 10
C
C Transform columns
C
         IF (COLFT(LP1,LPN,1,NCOP,HPL,MSQ,ZEROC)) GOTO 10
C
C Open output picture
C
         LP2=LPN
         IF (SEMOPN(2,ITO,NCOP,NROP,1,CLASS,FORM,LP2)) GOTO 10
C
C Set up normalisation factor
C
         IF (RNORM.EQ.0.0) THEN
            RN=1.0/(REAL(NCOP)*REAL(NROP))
         ELSE
            RN=RNORM
         ENDIF
C
C Bit reverse columns
C
         IF (COLFT2(LPN,LP2,1,NCOP,HPL,.FALSE.,.FALSE.,RN)) GOTO 10
C
C Otherwise, try to use multi-radix approach
C
      ELSE
C
C Fault output picture size if not factorisable or X size too small
C
         IF (SEMFAC(NCOP,BASE,NF).OR.SEMFAC(NROP,BASE,NF).OR.
     +       NCOP.LT.4) GOTO 20
C
C Determine array offsets into row buffer space
C
C        WCOFF=LNSBUF/LNCOMP-NCCOM-NR*4/5-NC*4/5
C        WROFF=LNSBUF/LNCOMP-NCCOM-NR*4/5
C        MPOFF=LNSBUF/LNCOMP-NCCOM
C
         WCOFF=LNSBUF/N4COMP-NCCOM-NR*N44/N45-NC*N44/N45
         WROFF=LNSBUF/N4COMP-NCCOM-NR*N44/N45
         MPOFF=LNSBUF/N4COMP-NCCOM
C
C Half-plane to real case
C -----------------------
C
         IF (HPL) THEN
C
C Storage management:
C                 size       form      usage          local form
C DFTXB2 from LP1 N/2+1 x M  any                      complex
C          to LPN N x M      fp                       formn(lpn)
C  DFTM  from LPN .....      ..        first i/p      complex
C         via LPN .....      ..        intermed i/o   formn(lpn)
C          to LP2 .....      any       last o/p       fp
C
C LPN is treated as N/2 x M complex; space is realloc as LP2 unless
C final o/p is non-fp
C
C Open possibly intermediate complex LPN (actually fp and two times
C the width??)
C
            LPN=LP1
            IF (SEMOPN(2,ITO,2*(NCOL-1),NROW,1,CLASS,NFMFP,LPN)) GOTO 10
C
C Set up picture size for intermediate stages of processing
C
            N(1)=NCOL-1
            N(2)=NROW
C
C Perform extra base 2 pass and recombine c-s and c-as parts
C
            IF (DFTXB2(1,LP1,LPN,N,MSQ,.FALSE.,ZEROC)) GOTO 10
C
C Open LP2 with requested final form
C
            LP2=LPN
            IF (SEMOPN(2,ITO,2*(NCOL-1),NROW,1,CLASS,FORM,LP2)) GOTO 10
C
C Set up normalisation factor
C
            IF (RNORM.EQ.0.0) THEN
               RN=1.0/(REAL(NCOL-1)*REAL(NROW))
            ELSE
               RN=RNORM
            ENDIF
C
C Transform LPN to LP2
C
            IF (DFTM(1,LPN,LP2,LPN,N,HPL,.FALSE.,.FALSE.,.FALSE.,RN,
     +               CBS,CBS(1,WCOFF),CBS(1,WROFF),CBS(1,MPOFF),
     +               NCBLK,WCOFF)) GOTO 10
C
C Complex case
C ------------
C
         ELSE
C
C Storage management:
C                 size       form      usage          local form
C  DFTM  from LP1 N x M      complex   first i/p      complex
C         via LPN N x M temp complex   intermed       formn(lpn)
C          to LP2 N x M      any       last o/p       complex
C
C If LP2 is complex and distinct from LP1, it serves as LPN too
C
C Open LP2 with requested final form
C
            LP2=LP1
            IF (SEMOPN(2,ITO,NCOL,NROW,1,CLASS,FORM,LP2)) GOTO 10
C
C Open temp LPN (unless LP2 is complex and distinct from LP1)
C
            IF (FORM.EQ.NFMCOM.AND.LP2.NE.LP1) THEN
               LPN=LP2
            ELSE
               LPN=LP1
               IF (SEMOPN(3,0,NCOL,NROW,1,CLASS,NFMCOM,LPN)) GOTO 10
            ENDIF
C
C Set up picture size for intermediate stages of processing
C
            N(1)=NCOL
            N(2)=NROW
C
C Set up normalisation factor
C
            IF (RNORM.EQ.0.0) THEN
               RN=1.0/(REAL(NCOL)*REAL(NROW))
            ELSE
               RN=RNORM
            ENDIF
C
C Transform LP1 via LPN to LP2
C
            IF (DFTM(1,LP1,LP2,LPN,N,HPL,MSQ,.FALSE.,ZEROC,RN,
     +               CBS,CBS(1,WCOFF),CBS(1,WROFF),CBS(1,MPOFF),
     +               NCBLK,WCOFF)) GOTO 10
C
         ENDIF
C
C Close temp picture
C
         IF (PICN(LPN).EQ.0) THEN
            IF (SEMCLS(LPN)) GOTO 10
         ENDIF
      ENDIF
C
      INVFT2=.FALSE.
C
   10 RETURN
C
C Fault bad size for source picture
C
   20 ERROR=5
      IDERR=1000*DEVN(LP1)+PICN(LP1)
      GOTO 10
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
