C Sub-processing module PSOUT5
C
      LOGICAL FUNCTION PSOUT5(MODE,LDISP,LHEADE)
      INTEGER MODE
      LOGICAL LDISP,LHEADE
C
      INCLUDE 'COMMON'
C
      LOGICAL PSOUTB,PSOUT0
C
      PSOUT5 = .TRUE.
C
C Write out procedure definition to process image
C
      IF (LHEADE) THEN
         IF (PSOUTB()) GOTO 10
         IF (PSOUT0('%%BeginProcSet: show_header 1.0 0')) GOTO 10
         IF (PSOUT0('/show_header {')) GOTO 10
         IF (PSOUT0(' gsave')) GOTO 10
         IF (PSOUT0(' /Helvetica findfont 7 scalefont setfont'
     +       )) GOTO 10
         IF (PSOUT0(' pageleft 5 add pagetop 10 sub xyround moveto'
     +       )) GOTO 10
C
         IF (LDISP) THEN
            IF (MODE.EQ.1) THEN
               IF (PSOUT0(' (Frame size:   ) show')) GOTO 10
            ELSE IF (MODE.EQ.2) THEN
               IF (PSOUT0(' (Partition size:   ) show')) GOTO 10
            ELSE IF (MODE.EQ.3) THEN
               IF (PSOUT0(' (Picture size:   ) show')) GOTO 10
            ENDIF
C
            IF (PSOUT0(' ncol 8 string cvs show (,) show')) GOTO 10
            IF (PSOUT0(' nrow 8 string cvs show')) GOTO 10
            IF (PSOUT0(' (          Sub-region size:   ) show'
     +          )) GOTO 10
            IF (PSOUT0(
     +          ' icol2 icol1 sub 1 add 8 string cvs show (,) show'
     +          )) GOTO 10
            IF (PSOUT0(' irow2 irow1 sub 1 add 8 string cvs show'
     +          )) GOTO 10
            IF (PSOUT0(' (          Frame:   ) show 8 string cvs show'
     +          )) GOTO 10
            IF (PSOUT0(' (          Scaling factor:   ) show')) GOTO 10
            IF (PSOUT0(' times 10 string cvs show')) GOTO 10
            IF (PSOUT0(' (          Current date:   ) show')) GOTO 10
            IF (PSOUT0(' currentdatestring show')) GOTO 10
            IF (PSOUT0(' (          Current time:   ) show')) GOTO 10
            IF (PSOUT0(' currenttimestring show')) GOTO 10
            IF (PSOUT0(' pageright 1 sub pagetop 14 sub xyround moveto'
     +          )) GOTO 10
            IF (PSOUT0(' pageleft  1 add pagetop 14 sub xyround lineto'
     +          )) GOTO 10
         ELSE
            IF (PSOUT0(' (Picture size:   ) show')) GOTO 10
            IF (PSOUT0(' ncol 8 string cvs show (,) show')) GOTO 10
            IF (PSOUT0(' nrow 8 string cvs show (,) show')) GOTO 10
            IF (PSOUT0(' nlay 8 string cvs show')) GOTO 10
            IF (PSOUT0(' (          Class:   ) show classstring show'
     +          )) GOTO 10
            IF (PSOUT0(' (          Form:   ) show formstring show'
     +          )) GOTO 10
            IF (PSOUT0(' (          Creation date:   ) show')) GOTO 10
            IF (PSOUT0(' createdatestring show')) GOTO 10
            IF (PSOUT0(' (          Creation time:   ) show')) GOTO 10
            IF (PSOUT0(' createtimestring show')) GOTO 10
            IF (PSOUT0(' pageleft 5 add pagetop 22 sub moveto'
     +          )) GOTO 10
            IF (PSOUT0(' (Title:   )  show')) GOTO 10
            IF (PSOUT0(' title1string show')) GOTO 10
            IF (PSOUT0(' title2string show')) GOTO 10
            IF (PSOUT0(' title3string show')) GOTO 10
            IF (PSOUT0(' pageleft 5 add pagetop 34 sub moveto'
     +          )) GOTO 10
            IF (PSOUT0(' (Sub-region size:   ) show')) GOTO 10
            IF (PSOUT0(
     +          ' icol2 icol1 sub 1 add 8 string cvs show (,) show'
     +          )) GOTO 10
            IF (PSOUT0(' irow2 irow1 sub 1 add 8 string cvs show'
     +          )) GOTO 10
            IF (PSOUT0(' (          Layer:   ) show 8 string cvs show'
     +          )) GOTO 10
            IF (PSOUT0(' (          Scaling factor:   ) show')) GOTO 10
            IF (PSOUT0(' times 10 string cvs show')) GOTO 10
            IF (PSOUT0(' (          Range:   ) show')) GOTO 10
            IF (PSOUT0(' range1string show (,) show')) GOTO 10
            IF (PSOUT0(' range2string show')) GOTO 10
            IF (PSOUT0(' (          Current date:   ) show')) GOTO 10
            IF (PSOUT0(' currentdatestring show')) GOTO 10
            IF (PSOUT0(' (          Current time:   ) show')) GOTO 10
            IF (PSOUT0(' currenttimestring show')) GOTO 10
            IF (PSOUT0(' pageright 1 sub pagetop 38 sub xyround moveto'
     +          )) GOTO 10
            IF (PSOUT0(' pageleft  1 add pagetop 38 sub xyround lineto'
     +          )) GOTO 10
         ENDIF
C
         IF (PSOUT0(' pageleft  1 add pagetop  1 sub xyround lineto'
     +          )) GOTO 10
         IF (PSOUT0(' pageright 1 sub pagetop  1 sub xyround lineto'
     +          )) GOTO 10
         IF (PSOUT0(' closepath')) GOTO 10
         IF (PSOUT0(' 0.1 setlinewidth 2 setlinecap stroke')) GOTO 10
         IF (PSOUT0(' grestore} def')) GOTO 10
         IF (PSOUT0('%%EndProcSet: show_header 1.0 0')) GOTO 10
      ENDIF
C
      PSOUT5 = .FALSE.
   10 RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd., All Rights Reserved
C
      END
