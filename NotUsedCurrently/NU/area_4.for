      subroutine AREA
* Program written 1990 by M.W. Hounslow - updated by N.K. Tovey 1993-5
*                      and J. M. Wang 5/1/1995
* Purpose:-
*      To provide a simple way to determine the  [percentage]
*      of pixels in a given grey - level range.
*      If the TOTAL option is selected the actual number of pixels
*      is returned as is the mean of the selected range.  An option 
*      allows LOG mean to be computed.
*
*      The default option also prints values to screen. This may be
*      supressed.
*
* Variables returned to SEMPER      T  - total number of pixels
*                                   MEA - Mean of selected range
*
*      Usual sub-region keys operate  and marks the dispaly if mark set
*
* keys:
*  From      - picture selected  [select]
*  Grey,gr2  - The selected grey levels between which the % of picture
*              they occupy. If only GREY is set then only determine for
*              one grey level : (in f77 are int1,int2)  [1,0]
*OPTIONS:
*  Total    - prints total number of pixels instaed of % area [no]
*             and returns t,mea as number of pixels and sum of pixels
*             if LN is also set the LN sum of pixels
*  Type     - type to screen if notype does not type to screen
*  average  - determine the average grey-level of pixels in range selected
*  LN       - takes the natural log mean
*
*  NOTE:-      total and average cannot both be set
*
      IMPLICIT NONE
* next line should be changed to 'common.f' for UNIX      
      INCLUDE 'COMMON.FOR'
      INTEGER*4 N
      INTEGER PLENG,CS,CE,RS,RE,LS,LE
      INTEGER J,K,FM,INT1,INT2,I,IVALPN,ULEV,LLEV,NLN
      PARAMETER(PLENG=(LNBUF+(2*LNEDGE*LNREAL))/LNINT)
      INTEGER NFRO,NINT1,NINT2,NTOT,NT,NTYP,NAVER,NMEAN,IB(0:PLENG*6)
* NB uses keys greylevel gr2 at int1 and in2
      PARAMETER (NFRO=10335,NINT1=11925,NINT2=11952,NTOT=-621,NT=-1
     + ,NTYP=-1017,NAVER=2485,NMEAN=21001,ULEV=32767,LLEV=-32768,
     + NLN=19760)
      LOGICAL OTOT,ONTYP,ANNOT,SEMROW,SEMLU,OPT,MRKREG
      LOGICAL MARSET,OPTNO,OMEAN,OLN,SEMCON,GETRG1
      REAL T,VAL,MEAN
      EQUIVALENCE (RB1,IB(0))
*

* get number of columns and number of lines in input picture
      FM=FORMN(LP1)
      IF(FM.GT.1)THEN
* will not do if form is fp or complex
       ERROR=43
       IDERR=IVALPN(NFRO)
       RETURN     
      ENDIF
      IF(NCOLS(LP1).GE.(PLENG*6)-50)THEN
        ERROR=47
        IDERR=IVALPN(NFRO)
      ENDIF
* get the intensity ranges
      INT1=NINT(VAL(NINT1))
      INT2=NINT(VAL(NINT2))
* error checks
      IF((INT1.LT.LLEV.OR.INT1.GT.ULEV).OR.(INT2.LT.LLEV.OR.INT2.GT.
     + ULEV))THEN
       ERROR=3
       IDERR=NINT1
       RETURN
      ENDIF
*
      IF(INT1.EQ.0.AND.INT2.EQ.0)THEN
* This is default value ie. look for intensity=0
        INT1=0
        INT2=0
        ELSEIF(INT1.GT.0.AND.INT2.EQ.0)THEN
* This is if only grey is set and no gr2
        INT2=INT1
        ELSEIF(INT1.GT.INT2)THEN
* This is if the largest value is first
        I=INT1
        INT1=INT2
        INT2=I
      ENDIF
*      print *,' int1= ,int2= ',int1,int2
      OTOT=OPT(NTOT)     
      ONTYP=OPTNO(NTYP)
      OMEAN=OPT(NAVER)
      OLN=OPT(NLN)
* conflicting options check
      IF(OTOT.AND.OMEAN)THEN
       ERROR=60
       IDERR=NTOT
       IDERR2=NAVER
       RETURN
      ENDIF
* get the subregion specified
      IF(GETRG1(CS,RS,LS,CE,RE,LE,LP1))RETURN
      IF(MARSET(ANNOT,K))RETURN
      IF(K.LT.2000.AND.ANNOT)THEN
       IF(MRKREG(0))RETURN
      ENDIF
*
      N=0
      T=0
      MEAN=0.0
      DO 10 I=SMGI3,SMGI6
       DO 10 K=SMGI2,SMGI5
         IF(SEMROW(1,IB(0),NFMINT,K,I,LP1))RETURN
         DO 10 J=SMGI1,SMGI4
            IF(IB(J).GE.INT1.AND.IB(J).LE.INT2)THEN
              N=N+1
              IF(OMEAN)THEN
                IF(OLN)THEN
                 IF(IB(J).LT.0)THEN
                   ERROR=97
                   RETURN
                 ENDIF
                 MEAN=MEAN+ALOG(FLOAT(IB(J)+1))
                 ELSE
                 MEAN=MEAN+FLOAT(IB(J))
                ENDIF
              ENDIF
            ENDIF
10    CONTINUE
*
      IF(OTOT)THEN
        T=FLOAT(N)
        ELSEIF(N.GT.0)THEN
        T=FLOAT(N)*100.0/((FLOAT(SMGI4-SMGI1+1)*FLOAT(SMGI5-SMGI2+1))*
     +  FLOAT(SMGI6-SMGI3+1))
      ENDIF
      IF(.NOT.ONTYP)THEN
        IF(OTOT)THEN
          WRITE(RECORD,'(a,f12.0)')' Total number of pixels = ', T
          ELSE
          IF(.NOT.OTOT)WRITE(RECORD,'(A,F10.4)')
     +    ' Percentage of pixels = ',T
        ENDIF
        IF(SEMCON(RECORD))RETURN
        IF(OMEAN.AND.N.GT.0)THEN
         IF(.NOT.OLN)WRITE(RECORD,'(A,F10.4)')' Average= ',
     +   MEAN/FLOAT(N)
         IF(OLN)WRITE(RECORD,'(A,F10.4)')' Log Average= ',
     +   EXP(MEAN/FLOAT(N))-1.0
         IF(SEMCON(RECORD))RETURN
        ENDIF
      ENDIF
      IF(SEMLU(1,NT,T))RETURN
      IF(N.EQ.0)RETURN
      IF(OTOT)THEN
       IF(SEMLU(1,NMEAN,MEAN))RETURN
       ELSE
       IF(OLN)THEN
         IF(SEMLU(1,NMEAN,EXP(MEAN/FLOAT(N))))RETURN
         ELSE
         IF(SEMLU(1,NMEAN,MEAN/FLOAT(N)))RETURN
       ENDIF
      ENDIF
      RETURN
      END







