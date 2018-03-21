C Sub-processing module PSOUT6
C
      LOGICAL FUNCTION PSOUT6(LBORDE,LORIGI,LCAPTN)
      LOGICAL LBORDE,LORIGI,LCAPTN
C
      INCLUDE 'COMMON'
C
      LOGICAL PSOUT0,PSOUTB
C
      PSOUT6 = .TRUE.
C
C Write out procedure definition to process image
C
      IF (PSOUTB()) GOTO 10
      IF (PSOUT0(
     +      '/trnd {transform round exch round exch} def')) GOTO 10
      IF (PSOUT0(
     +      '/xyround {trnd itransform} def')) GOTO 10
C
      IF (PSOUTB()) GOTO 10
      IF (PSOUT0('%%BeginProcSet: page_limits 1.0 0')) GOTO 10
      IF (PSOUT0('/page_limits {')) GOTO 10
      IF (PSOUT0(' gsave')) GOTO 10
      IF (PSOUT0(' clippath pathbbox')) GOTO 10
      IF (PSOUT0(' /pagetop    exch def')) GOTO 10
      IF (PSOUT0(' /pageright  exch def')) GOTO 10
      IF (PSOUT0(' /pagebottom exch def')) GOTO 10
      IF (PSOUT0(' /pageleft   exch def')) GOTO 10
      IF (PSOUT0(' grestore} def')) GOTO 10
      IF (PSOUT0('%%EndProcSet: page_limits 1.0 0')) GOTO 10
C
      IF (PSOUTB()) GOTO 10
      IF (PSOUT0('%%BeginProcSet: image_matrix 1.0 0')) GOTO 10
      IF (PSOUT0('/image_matrix {')) GOTO 10
      IF (PSOUT0(' /current_matrix matrix currentmatrix def')) GOTO 10
      IF (PSOUT0(' pageleft pageright add 2 div')) GOTO 10
      IF (PSOUT0(' pagebottom pagetop add 2 div')) GOTO 10
      IF (PSOUT0(' xyround translate')) GOTO 10
      IF (PSOUT0(' landscape {90 rotate} if')) GOTO 10
      IF (PSOUT0(' icol times mul irow times mul scale')) GOTO 10
      IF (PSOUT0(' -0.5 -0.5 xyround translate} def')) GOTO 10
      IF (PSOUT0('%%EndProcSet: image_matrix 1.0 0')) GOTO 10
C
      IF (LBORDE) THEN
         IF (PSOUTB()) GOTO 10
         IF (PSOUT0('%%BeginProcSet: show_border 1.0 0')) GOTO 10
         IF (PSOUT0('/show_border {')) GOTO 10
         IF (PSOUT0(' gsave')) GOTO 10
         IF (PSOUT0(' image_matrix')) GOTO 10
         IF (PSOUT0(' complex {0.5 0 trnd 0.5 1 trnd} if')) GOTO 10
         IF (PSOUT0(' 1 0 trnd 1 1 trnd 0 1 trnd 0 0 trnd')) GOTO 10
         IF (PSOUT0(' current_matrix setmatrix landscape {90 rotate} if'
     +      )) GOTO 10
         IF (PSOUT0(' itransform moveto itransform lineto')) GOTO 10
         IF (PSOUT0(' itransform lineto itransform lineto closepath'
     +       )) GOTO 10
         IF (PSOUT0(' complex {itransform moveto itransform lineto} if'
     +       )) GOTO 10
         IF (PSOUT0(' 0.1 setlinewidth 2 setlinecap stroke')) GOTO 10
         IF (PSOUT0(' grestore} def')) GOTO 10
         IF (PSOUT0('%%EndProcSet: show_border 1.0 0')) GOTO 10
      ENDIF
C
      IF (LORIGI) THEN
         IF (PSOUTB()) GOTO 10
         IF (PSOUT0('%%BeginProcSet: show_origin 1.0 0')) GOTO 10
         IF (PSOUT0('/show_origin {')) GOTO 10
         IF (PSOUT0(' gsave')) GOTO 10
         IF (PSOUT0(' image_matrix')) GOTO 10
         IF (PSOUT0(
     +      ' /xcen ccol icol1 sub 2 mul 1 add 2 div icol div def'
     +      )) GOTO 10
         IF (PSOUT0(
     +      ' /ycen irow2 crow sub 2 mul 1 add 2 div irow div def'
     +      )) GOTO 10
         IF (PSOUT0(' /imcen xcen 0.5 add def')) GOTO 10
         IF (PSOUT0(' /xcenin xcen 0 lt xcen 1 gt or not def')) GOTO 10
         IF (PSOUT0(' /ycenin ycen 0 lt ycen 1 gt or not def')) GOTO 10
         IF (PSOUT0(' /imcenin complex {imcen 0 lt imcen 1 gt or not}'
     +      )) GOTO 10
         IF (PSOUT0('                  {false} ifelse def')) GOTO 10
         IF (PSOUT0(' xcenin  {xcen 0 trnd xcen 1 trnd} if')) GOTO 10
         IF (PSOUT0(' ycenin  {0 ycen trnd 1 ycen trnd} if')) GOTO 10
         IF (PSOUT0(' imcenin {imcen 0 trnd imcen 1 trnd} if')) GOTO 10
         IF (PSOUT0(' current_matrix setmatrix landscape {90 rotate} if'
     +      )) GOTO 10
         IF (PSOUT0(' imcenin {itransform moveto  0  4 rlineto'
     +      )) GOTO 10
         IF (PSOUT0('          itransform moveto  0 -4 rlineto} if'
     +      )) GOTO 10
         IF (PSOUT0(' ycenin  {itransform moveto  4  0 rlineto'
     +      )) GOTO 10
         IF (PSOUT0('          itransform moveto -4  0 rlineto} if'
     +      )) GOTO 10
         IF (PSOUT0(' xcenin  {itransform moveto  0  4 rlineto'
     +      )) GOTO 10
         IF (PSOUT0('          itransform moveto  0 -4 rlineto} if'
     +      )) GOTO 10
         IF (PSOUT0(' 0.1 setlinewidth 2 setlinecap stroke'
     +      )) GOTO 10
         IF (PSOUT0(' grestore} def')) GOTO 10
         IF (PSOUT0('%%EndProcSet: show_origin 1.0 0')) GOTO 10
      ENDIF
C
      IF (LCAPTN) THEN
         IF (PSOUTB()) GOTO 10
         IF (PSOUT0('%%BeginProcSet: show_caption 1.0 0')) GOTO 10
         IF (PSOUT0('/show_caption {')) GOTO 10
         IF (PSOUT0(' gsave')) GOTO 10
         IF (PSOUT0(' image_matrix')) GOTO 10
         IF (PSOUT0(' 0.5 above {1} {0} ifelse trnd')) GOTO 10
         IF (PSOUT0(
     +      ' current_matrix setmatrix landscape {90 rotate} if'
     +      )) GOTO 10
         IF (PSOUT0(' itransform moveto')) GOTO 10
         IF (PSOUT0(' 0 above {12} {-20} ifelse rmoveto')) GOTO 10
         IF (PSOUT0(' /Helvetica findfont 10 scalefont setfont'
     +      )) GOTO 10
         IF (PSOUT0(' caption1string stringwidth pop')) GOTO 10
         IF (PSOUT0(' caption2string stringwidth pop add')) GOTO 10
         IF (PSOUT0(' caption3string stringwidth pop add')) GOTO 10
         IF (PSOUT0(' 2 div neg 0 rmoveto')) GOTO 10
         IF (PSOUT0(' caption1string show')) GOTO 10
         IF (PSOUT0(' caption2string show')) GOTO 10
         IF (PSOUT0(' caption3string show')) GOTO 10
         IF (PSOUT0(' grestore} def')) GOTO 10
         IF (PSOUT0('%%EndProcSet: show_caption 1.0 0')) GOTO 10
      ENDIF
C
      PSOUT6 = .FALSE.
   10 RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd., All Rights Reserved
C
      END
