C Semper 6 subsidiary module FT2D
C
      LOGICAL FUNCTION FT2D(ITO,CLASS,FORM,MSQ,LN)
C
      INTEGER ITO,CLASS,FORM
      LOGICAL MSQ,LN
C
C Effects 2-D forward FFT from LP1 to ITO:
C
C [in] integer ITO: output picture number; returned open as LP2
C [in] integer CLASS: output picture class
C [in] integer FORM: output picture form
C [in] logical MSQ: output mod squared of FT
C [in] logical LN: output ln mod squared of FT, if msq
C
C This module simply checks sizes and organises intermediate/output
C pictures for DFTM/DFTXB2 or COLFT/COLFT2
C
      LOGICAL SEMFAC,SEMCP2,SEMOPN,SEMCLS,SEMCEN
      LOGICAL DFTM,DFTXB2,COLFT,COLFT2
C
C Local declarations
C
      INTEGER*4 NC,NR,NCBLK,NCCOM,FFTBUF,WCOFF,WROFF,MPOFF
      INTEGER BASE(9),N(2),NCOL,NROW,NF,LPN,CCOL,CROW,NCOP,NROP
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
      FT2D=.TRUE.
C
C Non-complex source picture transforms to half-plane complex result
C
      HPL=FORMN(LP1).NE.NFMCOM
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Determine output picture size
C
      IF (HPL) THEN
         NCOP=NCOL/2+1
      ELSE
         NCOP=NCOL
      ENDIF
C
      NROP=NROW
C
C Determine data buffer requirements for multi-radix approach
C
      NC=NCOP
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
C Fault source picture size if not power of 2
C
         IF (SEMCP2(NCOL,NROW)) GOTO 20
C
C Determine centre position for output picture
C
         IF (HPL) THEN
            CCOL=1
         ELSE
            CCOL=1+NCOL/2
         ENDIF
C
         CROW=1+NROW/2
C
C Open (possibly intermediate) output
C
         LPN=LP1
         IF (SEMOPN(2,ITO,NCOP,NROP,1,CLASS,NFMCOM,LPN)) GOTO 10
C
C Record centre position
C
         IF (SEMCEN(LPN,CCOL,CROW,1)) GOTO 10
C
C Obtain FFT using old radix 2 approach, generating right half-plane
C of xform if source complex
C
         IF (COLFT(LP1,LPN,-1,NCOL,HPL,.FALSE.,.FALSE.)) GOTO 10
C
C Reopen output if final form not complex
C
         LP2=LPN
         IF (FORM.NE.NFMCOM) THEN
            IF (SEMOPN(2,ITO,NCOP,NROP,1,CLASS,FORM,LP2)) GOTO 10
         ENDIF
C
C Bit-reverse columns, incl row transforms and optional mod squ, ln
C
         IF (COLFT2(LPN,LP2,-1,NCOL,HPL,MSQ,LN,0.0)) GOTO 10
C
C Otherwise, try to use multi-radix approach
C
      ELSE
C
C Fault source picture size if not factorisable or X size too small
C
         IF (SEMFAC(NCOL,BASE,NF).OR.SEMFAC(NROW,BASE,NF).OR.
     +       NCOL.LT.4) GOTO 20
C
C Determine array offsets onto row buffer space
C
C        WCOFF=LNSBUF/LNCOMP-NCCOM-NR*4/5-NC*4/5
C        WROFF=LNSBUF/LNCOMP-NCCOM-NR*4/5
C        MPOFF=LNSBUF/LNCOMP-NCCOM
C
         WCOFF=LNSBUF/N4COMP-NCCOM-NR*N44/N45-NC*N44/N45
         WROFF=LNSBUF/N4COMP-NCCOM-NR*N44/N45
         MPOFF=LNSBUF/N4COMP-NCCOM
C
C Real to half-plane case
C -----------------------
C
         IF (HPL) THEN
C
C Storage management:
C                 size       form      usage          local form
C  DFTM  from LP1 N x M      any       first i/p      fp
C         via LPN N/2+1 x M  complex   itermed i/o    formn(lpn)
C          to LPN .........  .......   last o/p       formn(lpn)
C DFTXB2 from LPN .........  .......
C          to LP2 N/2+1 x M  any       last o/p       cmplx
C
C Only N/2 of LPN is needed, but extra 1 allows space to be re-alloc
C as LP2 in most cases
C
C Open possibly intermediate complex LPN
C
            LPN=LP1
            IF (SEMOPN(2,ITO,NCOL/2+1,NROW,1,CLASS,NFMCOM,LPN)) GOTO 10
C
C Record centre position
C
            IF (SEMCEN(LPN,1,1+NROW/2,1)) GOTO 10
C
C Set up picture size for intermediate stages of processing
C
            N(1)=NCOL/2
            N(2)=NROW
C
C Transform LP1 to LPN
C
            IF (DFTM(-1,LP1,LPN,LPN,N,HPL,.FALSE.,.FALSE.,.FALSE.,0.0,
     +               CBS,CBS(1,WCOFF),CBS(1,WROFF),CBS(1,MPOFF),
     +               NCBLK,WCOFF)) GOTO 10
C
C Open LP2, with requested final form
C
            LP2=LPN
            IF (SEMOPN(2,ITO,NCOL/2+1,NROW,1,CLASS,FORM,LP2)) GOTO 10
C
C Extract c-s and c-as parts and perform xtra base 2 pass
C
            IF (DFTXB2(-1,LPN,LP2,N,MSQ,LN,.FALSE.)) GOTO 10
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
            IF (FORMN(LP2).EQ.NFMCOM.AND.LP2.NE.LP1) THEN
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
C Transform LP1 via LPN to LP2
C
            IF (DFTM(-1,LP1,LP2,LPN,N,HPL,MSQ,LN,.FALSE.,0.0,
     +               CBS,CBS(1,WCOFF),CBS(1,WROFF),CBS(1,MPOFF),
     +               NCBLK,WCOFF)) GOTO 10
         ENDIF
C
C Close temp picture
C
         IF (PICN(LPN).EQ.0) THEN
            IF (SEMCLS(LPN)) GOTO 10
         ENDIF
      ENDIF
C
      FT2D=.FALSE.
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
