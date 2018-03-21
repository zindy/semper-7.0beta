C Semper 6 processing module SEMPIX
C---------------------------------------------------------------------
C
C      SUBROUTINE SEMPIX
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles commands P and PIXEL
C
C---------------------------------------------------------------------
C
      SUBROUTINE SEMPIX
C     =================
C
      INTEGER IVAL
C
      LOGICAL SEMDCR,SEMOPN,SEMROW,SEMXPL
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      REAL VX(3)
      INTEGER IPTR,J,K,L,N,CLASS,FORM,NCOL,NROW,NLAY
C
      INTEGER IB1(LNBUF/LNINT)
      EQUIVALENCE (RB1,IB1)
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) GOTO 40
C
C P processing (pixel settings) - if no current row, open SELECT
C
      IDERR = SELECT
      IF (BUFROW .EQ. 0) THEN
         IF (SEMOPN(1,IDERR,NCOL,NROW,NLAY,CLASS,FORM,BUFLPN)) GOTO 40
C
C Choose and record form
C
         IF (FORM .EQ. NFMBYT) THEN
            BUFFRM = NFMINT
         ELSE
            BUFFRM = FORM
         ENDIF
      ENDIF
C
C Check WP flag (which will fault tape writes too)
C
      IF (WSTAT(BUFLPN) .LT. 0) THEN
C
C Picture N is protected
C
         ERROR = 41
         GOTO 40
      ENDIF
C
C Decode coordinates
C
      K = IPTR
   10 IF (K .GT. COMLIM) THEN
C
C Syntax error
C
         ERROR = 17
         GOTO 40
      ENDIF
C
      IF (LINBUF(K) .NE. KEQUAL) THEN
         K = K + 1
         GOTO 10
      ENDIF
      K = K - 1
C
      N = 3
      VX(2) = 0.
      VX(3) = 0.
      IF (SEMXPL(LINBUF,K,IPTR,VX,N)) GOTO 40
C
C Skip over '='
C
      IPTR = IPTR + 1
C
C Convert to row and layer numbers
C
      K = CCOLN(BUFLPN) + NINT(VX(1))
      J = CROWN(BUFLPN) - NINT(VX(2))
      L = CLAYN(BUFLPN) + NINT(VX(3))
C
C Check X within range
C
   20 IF (K .LE. 0 .OR. K .GT. NCOLS(BUFLPN)) THEN
C
C Outside picture
C
         ERROR = 9
         GOTO 40
      ENDIF
C
C Expect 2 values for complex, 1 otherwise. Default complex part to zero
C
      IF (BUFFRM .EQ. NFMCOM) THEN
         N = 2
      ELSE
         N = 1
      ENDIF
C
      VX(2) = 0.
      IF (SEMXPL(LINBUF,COMLIM,IPTR,VX,N)) GOTO 40
C
C Fetch row, unless already buffered
C
      IF (BUFROW .NE. 0) THEN
         IF (J .EQ. IABS(BUFROW)) THEN
            IF (L .EQ. BUFLAY) GOTO 30
         ENDIF
      ENDIF
C
C New current row needed; dump present one if written-to
C
      IF (SEMDCR(0)) GOTO 40
C
C Choose new form if required
C
      IF (FORMN(BUFLPN) .EQ. NFMBYT) THEN
         BUFFRM = NFMINT
      ELSE
         BUFFRM = FORMN(BUFLPN)
      ENDIF
      IF (SEMROW(1,RB1,BUFFRM,J,L,BUFLPN)) GOTO 40
C
C Record current row, written to
C
   30 BUFROW = -J
      BUFLAY = L
C
C Insert new value
C
      IF (BUFFRM .EQ. NFMCOM) THEN
         RB1(2*K-1) = VX(1)
         RB1(2*K) = VX(2)
      ELSE IF (BUFFRM .EQ. NFMFP) THEN
         RB1(K) = VX(1)
      ELSE
         IB1(K) = VX(1)
      ENDIF
C
C Bump X coord and repeat?
C
      K = K + 1
      IF (IPTR .LE. COMLIM) GOTO 20
C
   40 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
