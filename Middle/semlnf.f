C Semper 6 system module SEMLNF
C
      LOGICAL FUNCTION SEMLNF(FORM,LNFORM)
C
C Returns length in bytes of pixels having indicated form
C
      INTEGER FORM,LNFORM
C
      INCLUDE 'COMMON'
C
      SEMLNF = .TRUE.
C
      IF (FORM .EQ. NFMBYT) THEN
         LNFORM = 1
      ELSE IF (FORM .EQ. NFMINT) THEN
         LNFORM = LNINT
      ELSE IF (FORM .EQ. NFMFP) THEN
         LNFORM = LNREAL
      ELSE IF (FORM .EQ. NFMCOM) THEN
         LNFORM = LNCOMP
      ELSE
         ERROR = 77
         WRITE(IDMESS,10) FORM
   10    FORMAT('Internal form error - SEMLNF got form',I5)
      ENDIF
C
      SEMLNF = .FALSE.
C
      RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
