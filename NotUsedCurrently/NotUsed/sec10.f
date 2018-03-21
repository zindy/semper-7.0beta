      SUBROUTINE ANOT4
C----------------------------------------------------------------------
C
C  Marks a displayed picture (key 'to') with arrows placed at points
C  supplied in a plist ( key 'at'). The directions and relative sizes
C  of the arrows are taken from a two-layer picture (key 'with')
C  containing x (layer 1) and y (layer 2) values. The value of RSCale
C  sets the length of the longest arrow and the scale factor used to
C  achieve this is returned in R2. If RSCale is unset, then an
C  'intelligent'  default scaling is calculated based upon the distance
C  between the first two points.
C
C  NOTE: for COMPLEX 'with' or 'at' input the imaginary part is ignored
C
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  An4 :ANOT4  $1=dis  to=$1  $2=sel  with=$2  $3=999  at=$3  rscale=
C              open(lp1,old)=to  open(lp2,old)=with  open(lp3,old)=at
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
C  Semper functions
C
      LOGICAL FSINIT,FSARRO,SEMROW,VARSET,SETVAR,FSERAS,FSQLIM,OPT
      LOGICAL SEMSEL
      INTEGER IPACK,IVALPN
      REAL VAL
C
      INTEGER  TO,NTO,WITH,NWITH,AT,NAT,NRSC,NR2,NERASE
      INTEGER  TONROW,TONCOL,TOCROW,TOCCOL
      INTEGER  WINROW,WINCOL,WINLAY,WICROW,WICCOL,WICLAS
      INTEGER  ATNCOL,ATNLAY,ATCLAS
      INTEGER I,J,JROW,IPLI
      REAL RSCALE,R2,X,Y,X2,Y2,DX,DY
      LOGICAL ERASE
C
C   SEMPER codes
C
      NTO=IPACK('TO')
      NWITH=IPACK('WITH')
      NAT=IPACK('AT')
      NRSC=IPACK('RSCALE')
      NR2=IPACK('R2')
      NERASE=IPACK('ERASE')
C
C  pictures to,with,at
C
      TO=IVALPN(NTO)
      WITH=IVALPN(NWITH)
      AT=IVALPN(NAT)
C
C  TO (displayed) picture parameters
C
      TONROW=NROWS(LP1)
      TONCOL=NCOLS(LP1)
      TOCROW=CROWN(LP1)
      TOCCOL=CCOLN(LP1)
C
C  with picture parameters
C
      WINROW=NROWS(LP2)
      WINCOL=NCOLS(LP2)
      WINLAY=NLAYS(LP2)
      WICROW=CROWN(LP2)
      WICCOL=CCOLN(LP2)
      WICLAS=CLASSN(LP2)
C
C  at picture parameters
C
      ATNCOL=NCOLS(LP3)
      ATNLAY=NLAYS(LP3)
      ATCLAS=CLASSN(LP3)
C
C  Fault bad number of layers for 'with'
C
      IF(WINLAY .NE. 2) THEN
         ERROR=77
         WRITE(IDMESS,10) WITH, WINLAY
   10    FORMAT('AN4: WITH must have nlay=2. Picture ',I6,' has ',I5)
         RETURN
      ENDIF
C
C  Fault bad number of layers for 'at'
C
      IF(ATNLAY .LT. 2) THEN
         ERROR=77
         WRITE(IDMESS,20) AT, ATNLAY
   20    FORMAT('AN4: AT must have nlay=2. Picture ',I6,' has ',I5)
         RETURN
      ENDIF
C
C  Fault bad class for 'with'
C
      IF(WICLAS .EQ. NCLPLI) THEN
         ERROR=77
         WRITE(IDMESS,30) WITH
   30    FORMAT('AN4: WITH picture,',I6,' must NOT be a PLIST')
         RETURN
      ENDIF
C
C  Fault bad class for 'at'
C
      IF(ATCLAS .NE. NCLPLI) THEN
         ERROR=77
         WRITE(IDMESS,40) AT
   40    FORMAT('AN4: AT picture,',I6,' must be a PLIST ')
         RETURN
      ENDIF
C
C  Fault conflicting picture sizes and origins for 'to' and 'with'
C
      IF(TONROW.NE.WINROW .OR. TONCOL.NE.WINCOL .OR. TOCROW.NE.WICROW
     +                    .OR. TOCCOL.NE. WICCOL) THEN
         ERROR=77
         WRITE(IDMESS,50) TO,WITH
   50    FORMAT('AN4: Pictures ',I6,' and ',I6,
     +          ' have differing sizes or origins')
         RETURN
      ENDIF
C
C  read 'at' list x-values to RB1 (layer 1), y-values to RB2 (layer 2)
C
      IF(SEMROW(1,RB1,NFMFP,1,1,LP3)) RETURN
      IF(SEMROW(1,RB2,NFMFP,1,2,LP3)) RETURN
C
C  set RSCALE=maximum arrow length
C
      IF(VARSET(NRSC)) THEN
         RSCALE=VAL(NRSC)
      ELSE
         IF(ATNCOL .EQ. 1) THEN
            RSCALE=REAL(TONROW+TONCOL)*0.5
         ELSE
            DX=RB1(2)-RB1(1)
            DY=RB2(2)-RB2(1)
            RSCALE=SQRT(DX*DX+DY*DY)
         ENDIF
      ENDIF
C
C  Check silly arrow sizes
C
      IF(RSCALE .LT. 1.0 .OR. ( RSCALE .GT. TONCOL
     +                   .AND.  RSCALE .GT. TONROW )) THEN
         ERROR=77
         WRITE(IDMESS,60) RSCALE
   60    FORMAT('AN4: Silly arrow size, ',F10.3)
         RETURN
      ENDIF
C
C  Initialise graphics  ( 'to' picture coordinates )
C
      IF(FSINIT(3,TO)) RETURN
C
C  Set JROW = 0 to flag that no 'with' rows have been read
C
      JROW=0
C
      R2=0.0
C
C  First pass through 'at' plist to determine maximum vector length
C
C
C  For each point in PLIST
C
      DO 70 IPLI=1,ATNCOL
C
C  X,Y = point specified by 'at'
C
         X=RB1(IPLI)
         Y=RB2(IPLI)
C
C  Calculate picture column and row corresponding to X,Y
C
         I=TOCCOL+NINT(X)
         J=TOCROW-NINT(Y)
C
C  Fault outside picture
C
         IF(I.LT.1 .OR. I.GT.TONCOL .OR. J.LT.1 .OR. J.GT.TONROW) THEN
            ERROR=9
            RETURN
         ENDIF
C
C  Read appropriate row from layers 1,2 of 'with' picture to RB3,RB4
C  if necessary (if the row is currently in buffer do not read)
C
         IF(JROW .NE. J) THEN
            IF(SEMROW(1,RB3,NFMFP,J,1,LP2)) RETURN
            IF(SEMROW(1,RB4,NFMFP,J,2,LP2)) RETURN
            JROW=J
         ENDIF
C
C  and set DX = desired length of arrow in x-direction, similarly DY
C
         DX=RB3(I)
         DY=RB4(I)
C
C  set R2 to greatest vector length-squared
C
         R2=AMAX1(DX*DX+DY*DY,R2)
C
   70 CONTINUE
C
C  check to see if need to erase overlay
C
      ERASE=OPT(NERASE)
      IF(ERASE) THEN
C
C   locate edges of display partition
C
         IF(FSQLIM(X,X2,Y,Y2)) RETURN
C
C   erase overlay
C
         IF(FSERAS(2,X,X2,Y,Y2)) RETURN
C
      ENDIF
C
C  if nonzero vectors to plot
C
      IF(R2 .NE. 0.0) THEN
C
C  set R2 = scale factor so that RSCALE is the maximum arrow length
C
         R2=RSCALE/SQRT(R2)
C
C  Now a second pass through 'at' list to do the actual annotation
C
         DO 80 IPLI=1,ATNCOL
            X=RB1(IPLI)
            Y=RB2(IPLI)
            I=TOCCOL+NINT(X)
            J=TOCROW-NINT(Y)
C
            IF(JROW .NE. J) THEN
               IF(SEMROW(1,RB3,NFMFP,J,1,LP2)) RETURN
               IF(SEMROW(1,RB4,NFMFP,J,2,LP2)) RETURN
               JROW=J
            ENDIF
C
            DX=RB3(I)
            DY=RB4(I)
C
C  Draw scaled arrow
C
            IF(FSARRO(X,Y,X+DX*R2,Y+DY*R2)) RETURN
C
   80    CONTINUE
C
      ENDIF
C
C  output value of R2 to SEMPER
C
      IF(SETVAR(NR2,R2)) RETURN
C
C  reset selected picture !
C
      IF(SEMSEL(LP2)) RETURN
C
      RETURN
      END
