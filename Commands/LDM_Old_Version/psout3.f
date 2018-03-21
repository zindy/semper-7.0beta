C Sub-processing module PSOUT3
C
      LOGICAL FUNCTION PSOUT3(MODE,LENCAP,LDISP,LBORDE,
     +                                 LORIGI,LCAPTN,LHEADE)
      INTEGER MODE
      LOGICAL LENCAP,LDISP,LBORDE,LORIGI,LCAPTN,LHEADE
C
      LOGICAL PSOUT0,PSOUT5,PSOUT6,PSOUTB
C
      INCLUDE 'COMMON'
C
      PSOUT3 = .TRUE.
C
C Write out procedure definition to process image
C
      IF (PSOUTB()) GOTO 10
      IF (PSOUT0('%%BeginProcSet: show_image 1.0 0')) GOTO 10
      IF (PSOUT0('/show_image {')) GOTO 10
      IF (PSOUT0(' gsave')) GOTO 10
      IF (PSOUT0(' image_matrix')) GOTO 10
      IF (PSOUT0(' icol irow 8 [icol 0 0 irow neg 0 irow]')) GOTO 10
      IF (PSOUT0(' {currentfile pixels readline pop} image')) GOTO 10
      IF (PSOUT0(' grestore} def')) GOTO 10
      IF (PSOUT0('%%EndProcSet: show_image 1.0 0')) GOTO 10
C
C Write output remaining procedure definitions according to whether
C ENCAPSULATED option is set or not
C
      IF (LENCAP) THEN
         IF (PSOUTB()) GOTO 10
         IF (PSOUT0('%%BeginProcSet: image_matrix 1.0 0')) GOTO 10
         IF (PSOUT0('/image_matrix {icol irow scale} def')) GOTO 10
         IF (PSOUT0('%%EndProcSet: image_matrix 1.0 0')) GOTO 10
      ELSE
         IF (PSOUT5(MODE,LDISP,LHEADE)) GOTO 10
         IF (PSOUT6(LBORDE,LORIGI,LCAPTN)) GOTO 10
      ENDIF
C
C Write out terminating comment for prolog
C
      IF (PSOUT0('%%EndProlog')) GOTO 10
C
C Write out setting up commands
C
      IF (PSOUTB()) GOTO 10
      IF (PSOUT0('%%BeginSetup')) GOTO 10
      IF (PSOUT0('currenttransfer')) GOTO 10
      IF (PSOUT0(
     +'{dummy exec 0.129412 sub 2.74194 mul} dup 0 currenttransfer put'
     + )) GOTO 10
      IF (PSOUT0('settransfer')) GOTO 10
C
      IF (.NOT.LENCAP) THEN
         IF (PSOUT0('page_limits')) GOTO 10
      ENDIF
C
      PSOUT3 = PSOUT0('%%EndSetup')
C
   10 RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd., All Rights Reserved
C
      END
